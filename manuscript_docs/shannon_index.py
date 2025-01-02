#!/usr/bin/env python3
"""
Compute a daily Shannon index of sound diversity for each site/date/treatment/country.

Steps:
1. Load and parse raw_file_list.csv for each country (daily coverage check).
   - Exclude any date that doesn't meet the coverage threshold (<90% of expected).
   - We keep the final combos: country, site, date, treatment.
2. In agile_outputs, for each subfolder (assume subfolder name is 'sound'), find sound_inference.csv:
   - Parse rows with logit >= 1.0, extract site/date/treatment (with offset).
   - Count how many inference hits for each (country, site, date, treatment, sound).
3. Merge all sounds for that date/site/treatment, so we have counts for each sound type.
4. Compute the Shannon index:
   - H = -Σ (p_i * ln(p_i)), where p_i = count_i / total_count, across all sounds i.
5. Merge with the coverage DataFrame (left join), filling missing with 0 => Shannon=0.
6. Save final CSV with columns: country, site, date, treatment, shannon.
"""

import os
import logging
import pandas as pd
import numpy as np
from datetime import datetime, timedelta

# Reuse your existing config from combine_counts (or define them here):
BASE_DIR = os.getenv("BASE_DIR")
if not BASE_DIR:
  raise ValueError("BASE_DIR environment variable is not set.")

COUNTRY_CONFIG = {
  "australia": {"offset": -10, "duty_cycle": 4},
  "kenya": {"offset": -3, "duty_cycle": 4},
  "indonesia": {"offset": 0,  "duty_cycle": 2},
  "maldives": {"offset": +5, "duty_cycle": 4},
  "mexico": {"offset": +7, "duty_cycle": 4}
}

LOGIT_CUTOFF = 1.0
FILE_COVERAGE = 0.9  # 90%
OUTPUT_SHANNON_PATH = os.path.join(
  BASE_DIR,
  "marrs_acoustics/data/results/functions",
  "shannon_index.csv"
)

def parse_treatment(filename_part: str) -> str:
  """
  Same as before: uses filename_part[4] to determine H/D/R/N.
  """
  if len(filename_part) <= 4:
    return "unknown"
  char = filename_part[4]
  if char == "H":
    return "healthy"
  if char == "D":
    return "degraded"
  if char == "R":
    return "restored"
  if char == "N":
    return "newly_restored"
  return "unknown"

def parse_site(filename_part: str) -> str:
  """
  Same as before: e.g. 'ind_D2_20220830_130600.WAV' -> site='D2'
  """
  parts = filename_part.split("_")
  return parts[1] if len(parts) > 1 else "unknown"

def parse_filename_datetime(filename_part: str, country: str) -> datetime:
  """
  Parse the full datetime (YYYYMMDD_HHMMSS) from the filename,
  then apply the offset from COUNTRY_CONFIG.
  """
  dt_str = filename_part[7:7+15]  # '20220830_130600'
  dt_obj = datetime.strptime(dt_str, "%Y%m%d_%H%M%S")
  offset_hrs = COUNTRY_CONFIG[country]["offset"]
  return dt_obj + timedelta(hours=offset_hrs)

def parse_date(filename_part: str, country: str) -> str:
  """
  Return just YYYYMMDD from the offset-corrected datetime.
  """
  dt_corrected = parse_filename_datetime(filename_part, country)
  return dt_corrected.strftime("%Y%m%d")

def get_expected_daily_recordings(duty_cycle: int) -> int:
  """
  If '4' => 1 min on, 3 min off => 15 cycles/hour => 360/day, etc.
  """
  minutes_per_day = 24 * 60
  return int(minutes_per_day / duty_cycle)

def load_raw_file_list(country: str) -> pd.DataFrame:
  """
  Loads the raw_file_list.csv for coverage checks, returning
  (country, site, date, treatment) combos that pass coverage.
  """
  raw_list_path = os.path.join(
    BASE_DIR,
    "marrs_acoustics/data",
    f"output_dir_{country}",
    "raw_file_list.csv"
  )
  if not os.path.isfile(raw_list_path):
    logging.warning(f"No raw_file_list for {country}")
    return pd.DataFrame(columns=["country", "site", "date", "treatment"])

  df_raw = pd.read_csv(raw_list_path)
  rows = []
  for _, row_csv in df_raw.iterrows():
    filename_full = row_csv["filename"]
    filename_part = filename_full.split("/", 1)[-1] if "/" in filename_full else filename_full

    date_str = parse_date(filename_part, country)
    site = parse_site(filename_part)
    treatment = parse_treatment(filename_part)
    rows.append((country, site, date_str, treatment))

  df_out = pd.DataFrame(rows, columns=["country", "site", "date", "treatment"])
  # Coverage is daily, so count how many files per site–date
  df_counts = df_out.groupby(["site", "date"]).size().reset_index(name="n_files")

  duty_cycle = COUNTRY_CONFIG[country]["duty_cycle"]
  expected_daily = get_expected_daily_recordings(duty_cycle)
  coverage_threshold = FILE_COVERAGE * expected_daily

  # Exclude days < coverage threshold
  insufficient = df_counts[df_counts["n_files"] < coverage_threshold]
  if not insufficient.empty:
    for _, row_ex in insufficient.iterrows():
      logging.info(
        f"Excluding {country}, site={row_ex['site']}, date={row_ex['date']} "
        f"(only {row_ex['n_files']} of {expected_daily} expected)"
      )
  df_counts = df_counts[df_counts["n_files"] >= coverage_threshold]

  # Keep only coverage-passing combos
  df_valid = pd.merge(
    df_out,
    df_counts[["site", "date"]],
    on=["site", "date"],
    how="inner"
  )
  df_valid.drop_duplicates(inplace=True)

  return df_valid

def gather_sound_counts(country: str, logit_cutoff: float = 1.0) -> pd.DataFrame:
  """
  For each subfolder (sound) in agile_outputs, if <sound>_inference.csv exists:
    - read it, filter logit >= cutoff
    - parse site/date/treatment
    - group by (country, site, date, treatment, sound) => sum how many hits
  Return a DataFrame with [country, site, date, treatment, sound, count].
  """
  presence_rows = []

  agile_dir = os.path.join(
    BASE_DIR,
    "marrs_acoustics/data",
    f"output_dir_{country}",
    "agile_outputs"
  )
  if not os.path.isdir(agile_dir):
    logging.warning(f"No agile_outputs folder for {country}: {agile_dir}")
    return pd.DataFrame(columns=["country", "site", "date", "treatment", "sound", "count"])

  for sound_folder in os.listdir(agile_dir):
    folder_path = os.path.join(agile_dir, sound_folder)
    if not os.path.isdir(folder_path):
      continue

    csv_path = os.path.join(folder_path, f"{sound_folder}_inference.csv")
    if not os.path.isfile(csv_path):
      logging.info(f"Not using sound '{sound_folder}' (no CSV).")
      continue

    logging.info(f"Using sound '{sound_folder}' for {country}.")
    df_infer = pd.read_csv(csv_path)
    # Filter by logit
    # Adjust column name if needed: " logit" vs "logit"
    col_logit = " logit" if " logit" in df_infer.columns else "logit"
    df_infer = df_infer[df_infer[col_logit] >= logit_cutoff]

    # For each valid row, parse
    rows_infer = []
    for _, row_inf in df_infer.iterrows():
      filename_full = row_inf["filename"]
      filename_part = filename_full.split("/", 1)[-1] if "/" in filename_full else filename_full

      date_str = parse_date(filename_part, country)
      site = parse_site(filename_part)
      treatment = parse_treatment(filename_part)

      rows_infer.append((country, site, date_str, treatment, sound_folder))

    df_temp = pd.DataFrame(rows_infer, columns=["country", "site", "date", "treatment", "sound"])
    # group => count how many times that sound appeared
    df_grouped = (
      df_temp.groupby(["country", "site", "date", "treatment", "sound"], as_index=False)
      .size()
      .rename(columns={"size": "count"})
    )
    presence_rows.append(df_grouped)

  if not presence_rows:
    return pd.DataFrame(columns=["country", "site", "date", "treatment", "sound", "count"])

  df_all_sounds = pd.concat(presence_rows, ignore_index=True)
  return df_all_sounds

def compute_shannon(df_group: pd.DataFrame) -> float:
  """
  Given a subset of data for one group (site, date, etc.),
  compute Shannon = - Σ p_i ln(p_i), where p_i = count_i / total.
  """
  total = df_group["count"].sum()
  if total == 0:
    return 0.0
  p = df_group["count"] / total
  return -(p * np.log(p)).sum()

def process_country_shannon(country: str) -> pd.DataFrame:
  """
  1) Load coverage combos => (country, site, date, treatment).
  2) gather_sound_counts => (country, site, date, treatment, sound, count).
  3) Group by (country, site, date, treatment) => compute Shannon index.
  4) Left-merge with coverage combos => fill missing with 0 => 'shannon'.
  """
  df_coverage = load_raw_file_list(country)
  df_sounds = gather_sound_counts(country, LOGIT_CUTOFF)

  # If no sounds found, everything will be 0
  if df_sounds.empty:
    df_shannon = pd.DataFrame(columns=["country", "site", "date", "treatment", "shannon"])
    # Just fill coverage combos with 0
    df_shannon = pd.merge(
      df_coverage,
      df_shannon,
      on=["country", "site", "date", "treatment"],
      how="left"
    )
    df_shannon["shannon"] = df_shannon["shannon"].fillna(0)
    return df_shannon

  # Group => compute Shannon
  df_sh = (
    df_sounds.groupby(["country", "site", "date", "treatment"], as_index=False)
    .apply(lambda g: pd.Series({"shannon": compute_shannon(g)}))
    .reset_index()
  )

  # Merge with coverage combos
  df_final = pd.merge(
    df_coverage,
    df_sh[["country", "site", "date", "treatment", "shannon"]],
    how="left",
    on=["country", "site", "date", "treatment"]
  )
  df_final["shannon"] = df_final["shannon"].fillna(0)

  return df_final

def main() -> None:
  logging.basicConfig(level=logging.INFO)

  countries = list(COUNTRY_CONFIG.keys())
  combined_df = pd.DataFrame(columns=["country", "site", "date", "treatment", "shannon"])

  for country in countries:
    logging.info(f"Processing Shannon index for {country}...")
    df_country = process_country_shannon(country)
    combined_df = pd.concat([combined_df, df_country], ignore_index=True)

  combined_df.sort_values(["country", "treatment", "site", "date"], inplace=True)
  combined_df.to_csv(OUTPUT_SHANNON_PATH, index=False)
  logging.info(f"Saved daily Shannon index to {OUTPUT_SHANNON_PATH}")

if __name__ == "__main__":
  main()
