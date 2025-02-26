#!/usr/bin/env python3
"""
Compute the proportion of 'night-time' 5-second windows (logit >= 1.0)
for each site-date-treatment, excluding the 'snaps' sound, 
and only for dates with >=90% coverage.

Additionally:
- 'count' = total # of detected 5-sec windows (logit >=1, excluding snaps).
- 'max_poss_count' = total # of 5-sec windows recorded (post-coverage).
- 'log_max_poss_count' = log of that total.

Night-time is defined as: 
    (sunset - 30 min) on Day X
      through
    (sunrise + 30 min) on Day X+1

We then label that entire period as 'night of Day X'.

Steps:
1) Load and parse raw_file_list.csv for each country:
   - Exclude any date < 90% coverage using the duty cycle approach in count_ecofunctions.py.
   - Parse treatment from the filename.
   - Keep site, date, treatment, etc.

2) For each date, compute the local 'night' window using Astral, 
   based on lat/long + known local timezone:
   - sunset(day X) - 30 min => start
   - sunrise(day X+1) + 30 min => end

3) In agile_outputs, read all subfolders except 'snaps'.
   - For each inference CSV found, filter rows with logit >= 1.
   - Parse date/time, site, and treatment from the filename.
   - Keep only rows that fall into that night's window.

4) Within that night window, figure out how many total 5-sec windows were recorded,
   and how many had logit >= 1, grouped by (country, site, date, treatment).
   - 'count' = (# logit>=1)
   - 'max_poss_count' = total # 5-sec windows (post-coverage)
   - proportion_night_detections = (# logit >=1) / (total #)
   - log_max_poss_count = log of (total #)

5) Save a CSV with columns: 
   [country, site, date, treatment, proportion_night_detections, count, max_poss_count, log_max_poss_count]
"""

import os
import logging
import pandas as pd
import numpy as np
from datetime import datetime, timedelta, date

from astral import LocationInfo
from astral.sun import sun

from count_ecofunctions import (
    BASE_DIR,
    COUNTRY_CONFIG,   
    FILE_COVERAGE,    
    LOGIT_CUTOFF,     
    parse_site,
    parse_date_time,
    parse_treatment,
    get_expected_daily_recordings
)

OUTPUT_PATH = os.path.join(
    BASE_DIR,
    "marrs_acoustics/data/results/functions",
    "settlement_cuescape.csv"
)

# Coordinates + Local Timezone per country
COUNTRY_INFO = {
    "australia": {
        "coords": (-16.846378, 146.228253),
        "timezone": "Australia/Brisbane",  
    },
    "kenya": {
        "coords": (-2.215361,  41.014972),
        "timezone": "Africa/Nairobi",
    },
    "indonesia": {
        "coords": (-4.92913,   119.3175),
        "timezone": "Asia/Makassar",
    },
    "maldives": {
        "coords": (4.8864,     72.9278),
        "timezone": "Indian/Maldives",
    },
    "mexico": {
        "coords": (18.34133,  -87.80717),
        "timezone": "America/Cancun",
    },
}


def load_raw_file_list_simple(country: str) -> pd.DataFrame:
    """
    Load raw_file_list.csv for the given country,
    then exclude any date with <90% coverage for the given duty_cycle.
    Parse site, date, and treatment from each filename.

    Returns a DataFrame with columns: ["filename", "site", "date", "treatment"] 
    for coverage-passing combos.
    """
    raw_list_path = os.path.join(
        BASE_DIR,
        "marrs_acoustics/data",
        f"output_dir_{country}",
        "raw_file_list.csv"
    )

    if not os.path.isfile(raw_list_path):
        logging.warning(f"raw_file_list.csv not found for {country}. Path: {raw_list_path}")
        return pd.DataFrame(columns=["filename", "site", "date", "treatment"])

    df_raw = pd.read_csv(raw_list_path)

    # We'll parse site + date + treatment from the filename
    rows = []
    for _, row_data in df_raw.iterrows():
        filename = row_data["filename"]
        filename_part = filename.split("/", 1)[-1] if "/" in filename else filename
        site = parse_site(filename_part)
        treatment = parse_treatment(filename_part)
        dt_str = filename_part[7:15]  # 'YYYYMMDD'
        rows.append((filename, site, dt_str, treatment))

    df_temp = pd.DataFrame(rows, columns=["filename", "site", "date", "treatment"])

    # Count how many files (rows) per site–date
    duty_cycle = COUNTRY_CONFIG[country]["duty_cycle"]
    expected_daily = get_expected_daily_recordings(duty_cycle)
    coverage_threshold = FILE_COVERAGE * expected_daily

    df_counts = df_temp.groupby(["site", "date"]).size().reset_index(name="n_files")
    insufficient = df_counts[df_counts["n_files"] < coverage_threshold]
    if not insufficient.empty:
        for _, row_ex in insufficient.iterrows():
            logging.info(
                f"Excluding {country}, site={row_ex['site']}, date={row_ex['date']} "
                f"(only {row_ex['n_files']} of {expected_daily} expected)"
            )
    # Keep only combos above threshold
    df_counts = df_counts[df_counts["n_files"] >= coverage_threshold]

    # Merge back so only site–date combos above coverage remain
    df_valid = pd.merge(
        df_temp,
        df_counts[["site", "date"]],
        on=["site", "date"],
        how="inner"
    )
    df_valid.drop_duplicates(inplace=True)

    return df_valid  # columns: ["filename", "site", "date", "treatment"]


def get_night_window(the_date: date, country: str) -> (datetime, datetime):
    """
    Return (night_start, night_end) for 'night_of' the_date in local time:
      night_of_date = (sunset(the_date) - 30min) .. (sunrise(the_date+1) + 30min).

    We use the real local timezone from COUNTRY_INFO to ensure Astral lines up with local time.
    Then we remove tzinfo so we can compare with naive dt_local from the filename.
    """
    lat, lon = COUNTRY_INFO[country]["coords"]
    tz_name = COUNTRY_INFO[country]["timezone"]

    loc = LocationInfo(
        name=country,
        region="None",
        timezone=tz_name,
        latitude=lat,
        longitude=lon
    )

    s_today = sun(loc.observer, date=the_date, tzinfo=loc.timezone)
    sunset_time = s_today["sunset"].replace(tzinfo=None) - timedelta(minutes=30)

    next_day = the_date + timedelta(days=1)
    s_next = sun(loc.observer, date=next_day, tzinfo=loc.timezone)
    sunrise_time = s_next["sunrise"].replace(tzinfo=None) + timedelta(minutes=30)

    return (sunset_time, sunrise_time)


def gather_night_total_windows(country: str) -> pd.DataFrame:
    """
    For each coverage-passing row in raw_file_list_simple,
    check if that 1-minute file starts in the 'night_of_date' window => add 12 (5s windows).

    Returns columns:
      [country, site, night_of_date, treatment, total_5s_windows_night].
    """
    df_raw = load_raw_file_list_simple(country)
    if df_raw.empty:
        return pd.DataFrame(columns=["country", "site", "night_of_date", "treatment", "total_5s_windows_night"])
  
    rows = []
    for _, row_data in df_raw.iterrows():
        filename = row_data["filename"]
        site = row_data["site"]
        treatment = row_data["treatment"]
        dt_local = parse_date_time(filename)  # naive datetime
        local_date = dt_local.date()

        night_start, night_end = get_night_window(local_date, country)
        if night_start <= dt_local < night_end:
            night_of_date = local_date
        else:
            # check previous day
            prev_date = local_date - timedelta(days=1)
            ns, ne = get_night_window(prev_date, country)
            if ns <= dt_local < ne:
                night_of_date = prev_date
            else:
                continue

        rows.append((
            country,
            site,
            night_of_date.strftime("%Y%m%d"),
            treatment,
            12  # 1 minute => 12 x 5sec
        ))

    df_minutes = pd.DataFrame(
        rows, 
        columns=["country","site","night_of_date","treatment","total_5s_windows_night"]
    )
    df_sum = (
        df_minutes
        .groupby(["country","site","night_of_date","treatment"], as_index=False)["total_5s_windows_night"]
        .sum()
    )
    return df_sum


def gather_night_inferences(country: str) -> pd.DataFrame:
    """
    For each subfolder in agile_outputs (except 'snaps'):
      - Load inference CSV, filter logit >= 1
      - Parse local datetime => see if it belongs to that night's window
      - Keep site, date, treatment
      - If it belongs, 5s_window_detected=1

    Return [country, site, night_of_date, treatment, 5s_window_detected].
    """
    base_path = os.path.join(BASE_DIR, "marrs_acoustics/data", f"output_dir_{country}", "agile_outputs")
    if not os.path.isdir(base_path):
        logging.warning(f"No agile_outputs folder for {country}")
        return pd.DataFrame(columns=["country","site","night_of_date","treatment","5s_window_detected"])

    presence_rows = []
    for sound_folder in os.listdir(base_path):
        if sound_folder.lower() == "snaps":
            logging.info(f"Skipping 'snaps' folder for {country}")
            continue

        folder_path = os.path.join(base_path, sound_folder)
        if not os.path.isdir(folder_path):
            continue

        csv_path = os.path.join(folder_path, f"{sound_folder}_inference.csv")
        if not os.path.isfile(csv_path):
            logging.info(f"No CSV for {sound_folder} in {country}")
            continue

        logging.info(f"Parsing {sound_folder} for {country}...")
        df_infer = pd.read_csv(csv_path)
        col_logit = " logit" if " logit" in df_infer.columns else "logit"
        df_infer = df_infer[df_infer[col_logit] >= LOGIT_CUTOFF]

        for _, row_inf in df_infer.iterrows():
            filename_part = row_inf["filename"].split("/", 1)[-1]
            dt_local = parse_date_time(filename_part)
            site = parse_site(filename_part)
            treatment = parse_treatment(filename_part)

            local_date = dt_local.date()
            night_start, night_end = get_night_window(local_date, country)
            if night_start <= dt_local < night_end:
                night_of_date = local_date
            else:
                # check previous day
                prev_date = local_date - timedelta(days=1)
                ns, ne = get_night_window(prev_date, country)
                if ns <= dt_local < ne:
                    night_of_date = prev_date
                else:
                    continue

            presence_rows.append((country, site, night_of_date.strftime("%Y%m%d"), treatment, 1))

    df_detections = pd.DataFrame(
        presence_rows, 
        columns=["country","site","night_of_date","treatment","5s_window_detected"]
    )
    return df_detections


def process_country_nighttime_proportion(country: str) -> pd.DataFrame:
    """
    1) gather_night_total_windows => total_5s_windows_night for (country, site, night_of_date, treatment)
    2) gather_night_inferences   => # detected windows for the same grouping
    3) count = # detected windows
    4) max_poss_count = total_5s_windows_night
    5) proportion_night_detections = count / max_poss_count
    6) log_max_poss_count = log(max_poss_count)
    7) rename 'night_of_date' => 'date'
    """
    df_total = gather_night_total_windows(country)
    if df_total.empty:
        return pd.DataFrame(columns=[
            "country","site","date","treatment",
            "proportion_night_detections","count","max_poss_count","log_max_poss_count"
        ])

    df_detect = gather_night_inferences(country)
    # Sum all detections (5s_window_detected) per group
    df_detect_sum = (
        df_detect
        .groupby(["country","site","night_of_date","treatment"], as_index=False)["5s_window_detected"]
        .sum()
        .rename(columns={"5s_window_detected":"count"})
    )

    # Merge total & detected
    df_merged = pd.merge(
        df_total, 
        df_detect_sum,
        on=["country","site","night_of_date","treatment"],
        how="left"
    )
    # If no detections => fillna(0)
    df_merged["count"] = df_merged["count"].fillna(0).astype(int)

    # rename total_5s_windows_night => max_poss_count
    df_merged.rename(columns={"total_5s_windows_night": "max_poss_count"}, inplace=True)

    # proportion = count / max_poss_count
    df_merged["proportion_night_detections"] = (
        df_merged["count"] / df_merged["max_poss_count"]
    )

    # log_max_poss_count = log(max_poss_count)
    # protect from log(0)
    df_merged["log_max_poss_count"] = df_merged["max_poss_count"].apply(
        lambda x: np.log(x) if x > 0 else 0
    )

    # rename 'night_of_date' => 'date'
    df_merged.rename(columns={"night_of_date":"date"}, inplace=True)

    # final columns
    return df_merged[[
        "country","site","date","treatment",
        "proportion_night_detections","count","max_poss_count","log_max_poss_count"
    ]]


def main() -> None:
    logging.basicConfig(level=logging.INFO)

    columns = [
        "country","site","date","treatment",
        "proportion_night_detections","count","max_poss_count","log_max_poss_count"
    ]
    combined_df = pd.DataFrame(columns=columns)

    for country in COUNTRY_CONFIG.keys():
        logging.info(f"Processing nighttime proportion for {country}...")
        df_country = process_country_nighttime_proportion(country)
        combined_df = pd.concat([combined_df, df_country], ignore_index=True)

    combined_df.sort_values(["country","site","date","treatment"], inplace=True)
    combined_df.to_csv(OUTPUT_PATH, index=False)
    logging.info(f"Saved nighttime proportions to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()