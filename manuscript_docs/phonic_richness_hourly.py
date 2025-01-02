#!/usr/bin/env python3
"""
Produce a CSV of 'phonic richness' (number of unique sounds) per hour.

Overview:
1. For each country, we load its raw_file_list.csv:
   - Parse filename into a fully adjusted datetime (date + hour), plus site, treatment.
   - Perform the same daily coverage check as before (exclude entire days if <90%).
     => We'll keep the hour detail in the final output, but coverage is still daily-based.
2. In agile_outputs, for each subfolder (sound) except "snap", load rows (logit >= 1.0),
   parse site/date/hour/treatment, and store presence of that sound for each hour.
3. Group by (country, site, date, hour, treatment), counting unique sounds => 'phonic richness'.
4. Merge with the coverage-passing combos from step (1) (left join), fill missing with 0.
5. Save everything as phonic_richness_hourly.csv.
"""

import os
import logging
import pandas as pd
from datetime import datetime, timedelta

# Reuse some global config from our original combine_counts.py script (assuming same folder).
# Make sure you have combine_counts.py or an equivalent from which to import these.
from combine_counts import (
    BASE_DIR,
    COUNTRY_CONFIG,
    FILE_COVERAGE,       # e.g. 0.9
    LOGIT_CUTOFF,        # e.g. 1.0
    parse_treatment,
    parse_site,
)

# Where we'll save our final hourly output
OUTPUT_RICHNESS_HOURLY_PATH = os.path.join(
    BASE_DIR,
    "marrs_acoustics/data/results/functions",
    "phonic_richness_hourly.csv"
)

def parse_filename_datetime(filename_part: str, country: str) -> datetime:
  """
  Parse the full datetime (with hours, minutes, seconds) from the filename,
  then apply the country's offset, returning a Python datetime object.
  Example: "ind_D2_20220830_130600.WAV" => parse => 2022-08-30 13:06:00 local
  """
  dt_str = filename_part[7:7+15]  # e.g. '20220830_130600'
  dt_obj = datetime.strptime(dt_str, "%Y%m%d_%H%M%S")
  offset_hrs = COUNTRY_CONFIG[country]["offset"]
  return dt_obj + timedelta(hours=offset_hrs)

def get_expected_daily_recordings(duty_cycle: int) -> int:
  """
  Same as before. For duty_cycle=4, we expect 360 recordings/day, etc.
  """
  minutes_per_day = 24 * 60
  return int(minutes_per_day / duty_cycle)

def load_raw_file_list_hourly(country: str) -> pd.DataFrame:
  """
  1) Load the raw_file_list.csv for this country.
  2) Parse the fully adjusted datetime => (date, hour), plus site, treatment.
  3) Do the same daily coverage check as before (exclude entire day if coverage < 90%).
  4) Return DataFrame of (country, site, date, hour, treatment) combos that passed coverage.
  """
  raw_list_path = os.path.join(
    BASE_DIR,
    "marrs_acoustics/data",
    f"output_dir_{country}",
    "raw_file_list.csv"
  )

  if not os.path.isfile(raw_list_path):
    logging.warning(f"raw_file_list.csv not found for {country}: {raw_list_path}")
    return pd.DataFrame(columns=["country", "site", "date", "hour", "treatment"])

  df_raw = pd.read_csv(raw_list_path)

  # Build up rows
  raw_rows = []
  for _, row in df_raw.iterrows():
    filename_full = row["filename"]
    filename_part = filename_full.split("/", 1)[-1] if "/" in filename_full else filename_full

    dt_corrected = parse_filename_datetime(filename_part, country)
    date_str = dt_corrected.strftime("%Y%m%d")
    hour_val = dt_corrected.hour  # integer hour 0..23

    treatment = parse_treatment(filename_part)
    site = parse_site(filename_part)

    raw_rows.append((country, site, date_str, hour_val, treatment))

  df_out = pd.DataFrame(raw_rows, columns=["country", "site", "date", "hour", "treatment"])

  # Count how many files per site–date (ignore hour for coverage).
  # Coverage is daily-based, so we do the same approach as before:
  df_daily_counts = (
    df_out.groupby(["site", "date"])
    .size()
    .reset_index(name="n_files")
  )

  duty_cycle = COUNTRY_CONFIG[country]["duty_cycle"]
  expected_daily = get_expected_daily_recordings(duty_cycle)
  coverage_threshold = FILE_COVERAGE * expected_daily

  # Identify insufficient coverage
  insufficient = df_daily_counts[df_daily_counts["n_files"] < coverage_threshold]
  if not insufficient.empty:
    for _, row_ex in insufficient.iterrows():
      logging.info(
        f"Excluding entire day for {country}, site={row_ex['site']}, date={row_ex['date']} "
        f"(only {row_ex['n_files']} of {expected_daily} expected)"
      )

  # Keep only combos from days passing coverage
  df_daily_counts = df_daily_counts[df_daily_counts["n_files"] >= coverage_threshold]

  # Merge back so we only keep hours from coverage-passing days
  df_valid = pd.merge(
    df_out,
    df_daily_counts[["site", "date"]],  # keep day-based coverage pass
    on=["site", "date"],
    how="inner"
  )

  df_valid.drop_duplicates(inplace=True)
  return df_valid

def gather_sound_presence_hourly(country: str, logit_cutoff: float = 1.0) -> pd.DataFrame:
  """
  Look in the agile_outputs folder for subfolders => each subfolder is a 'sound'.
  Skip "snap". If <sound>_inference.csv exists, parse rows with logit >= cutoff.
  For each row => parse date, hour, site, treatment => that sound is present 
  in that hour.
  Return DataFrame of [country, site, date, hour, treatment, sound].
  """
  presence_rows = []
  country_agile_dir = os.path.join(
    BASE_DIR,
    "marrs_acoustics/data",
    f"output_dir_{country}",
    "agile_outputs"
  )
  if not os.path.isdir(country_agile_dir):
    logging.warning(f"No agile_outputs directory for {country}: {country_agile_dir}")
    return pd.DataFrame(columns=["country", "site", "date", "hour", "treatment", "sound"])

  for sound_folder in os.listdir(country_agile_dir):
    folder_path = os.path.join(country_agile_dir, sound_folder)
    if not os.path.isdir(folder_path):
      continue

    # skip if the folder is snap
    if sound_folder.lower() == "snap":
      logging.info(f"Not using: {sound_folder}")
      continue

    csv_path = os.path.join(folder_path, f"{sound_folder}_inference.csv")
    if not os.path.isfile(csv_path):
      logging.info(f"Not using: {sound_folder} (no CSV found).")
      continue

    logging.info(f"Using: {sound_folder}")
    df_infer = pd.read_csv(csv_path)
    # Filter rows with logit >= cutoff
    if " logit" in df_infer.columns:
      df_infer = df_infer[df_infer[" logit"] >= logit_cutoff]
    else:
      # If the column is just "logit"
      df_infer = df_infer[df_infer["logit"] >= logit_cutoff]

    for _, row_inf in df_infer.iterrows():
      filename_full = row_inf["filename"]
      filename_part = filename_full.split("/", 1)[-1] if "/" in filename_full else filename_full

      dt_corrected = parse_filename_datetime(filename_part, country)
      date_str = dt_corrected.strftime("%Y%m%d")
      hour_val = dt_corrected.hour

      treatment = parse_treatment(filename_part)
      site = parse_site(filename_part)

      presence_rows.append(
        (country, site, date_str, hour_val, treatment, sound_folder)
      )

  df_presence = pd.DataFrame(
    presence_rows,
    columns=["country", "site", "date", "hour", "treatment", "sound"]
  )
  return df_presence

def process_country_phonic_richness_hourly(country: str, logit_cutoff: float) -> pd.DataFrame:
  """
  1) Load coverage-passing combos with hour detail => (country, site, date, hour, treatment).
  2) gather_sound_presence_hourly => (country, site, date, hour, treatment, sound).
  3) Group by these keys, counting unique sounds => 'count'.
  4) Left-merge with coverage combos so we only keep valid coverage days/hours.
  """
  df_coverage = load_raw_file_list_hourly(country)
  df_presence = gather_sound_presence_hourly(country, logit_cutoff)

  if df_presence.empty:
    # No sounds => set count=0 for everything
    df_presence_count = pd.DataFrame(
      columns=["country", "site", "date", "hour", "treatment", "count"]
    )
  else:
    df_presence_count = (
      df_presence
      .groupby(["country", "site", "date", "hour", "treatment"], as_index=False)["sound"]
      .nunique()
      .rename(columns={"sound": "count"})
    )

  df_out = pd.merge(
    df_coverage,
    df_presence_count,
    how="left",
    on=["country", "site", "date", "hour", "treatment"]
  )
  df_out["count"] = df_out["count"].fillna(0).astype(int)

  return df_out

def main() -> None:
  """
  Build 'phonic richness' DataFrame per hour for all countries, then save.
  """
  logging.basicConfig(level=logging.INFO)

  countries = list(COUNTRY_CONFIG.keys())  # or define subset if needed
  combined_df = pd.DataFrame(columns=["country", "site", "date", "hour", "treatment", "count"])

  for country in countries:
    logging.info(f"Processing hourly phonic richness for {country}...")
    df_country = process_country_phonic_richness_hourly(country, LOGIT_CUTOFF)
    combined_df = pd.concat([combined_df, df_country], ignore_index=True)

  # Sort by e.g. country, treatment, site, date, hour
  combined_df.sort_values(["country", "treatment", "site", "date", "hour"], inplace=True)

  combined_df.to_csv(OUTPUT_RICHNESS_HOURLY_PATH, index=False)
  logging.info(f"Saved hourly phonic richness to {OUTPUT_RICHNESS_HOURLY_PATH}")

if __name__ == "__main__":
  main()
