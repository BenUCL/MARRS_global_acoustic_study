#!/usr/bin/env python3
"""
Produce a CSV of 'phonic richness' (number of unique sounds per date/site).

Steps:
1. For each country, load its raw_file_list.csv (as done before):
   - Parse each filename to extract site, date (with time offset), treatment.
   - Count how many files exist per site–date and exclude any that have <90% coverage 
     (based on the expected daily recordings, computed from the duty_cycle).
   - Keep only those site–date–treatment combos that pass coverage.

2. In the agile_outputs folder for each country:
   - Look through all subfolders, each subfolder corresponds to a 'sound' name.
     For instance, a folder 'creek' would presumably contain 'creek_inference.csv'.
   - Ignore any sound folder named 'snap'.
   - If the CSV for that sound exists, load it, parse logit >= 1.0,
     then parse site/date/treatment (similar to the old script).
   - For each site/date that has at least one valid inference row for that sound,
     we consider that sound "present" on that site/date.

3. After checking all sounds for a given country, we have a set (or list) of 
   (country, site, date, treatment, sound) combos.
   - Group these by (country, site, date, treatment) and find the number of unique sounds.
   - This number becomes our 'count'.

4. Merge these results with the coverage-filtered combos from step 1 (left join),
   so we only keep site–date combos that passed coverage.
   - Fill missing counts with 0 if no sounds were present.

5. Append across all countries, sort, and save as phonic_richness.csv.
"""

import os
import logging
import pandas as pd
from datetime import datetime, timedelta

# We'll reuse many of the functions and variables from 'combine_counts.py'.
# For example, parse_date, parse_site, parse_treatment, etc.
# Make sure this script is in the same folder so the import works.
from count_ecofunctions import (
    BASE_DIR,
    COUNTRY_CONFIG,
    FILE_COVERAGE,
    LOGIT_CUTOFF,
    parse_date,
    parse_site,
    parse_treatment,
    load_raw_file_list,  # We'll reuse this for coverage checks
)

# A new output CSV
OUTPUT_RICHNESS_PATH = os.path.join(
    BASE_DIR,
    "marrs_acoustics/data/results/functions",
    "phonic_richness.csv"
)

def gather_sound_presence(country: str, logit_cutoff: float = 1.0) -> pd.DataFrame:
  """
  In the agile_outputs folder for `country`, find all subfolders, each presumably a sound name.
  - Ignore subfolders named 'snap'.
  - If we see a subfolder called e.g. 'creek', expect 'creek_inference.csv'.
    If that file exists, parse it:
      - Keep only rows with logit >= logit_cutoff
      - Extract site, date (with offset), treatment
      - Each site/date that appears => that sound is 'present' for that site/date/treatment
  Return a DataFrame of columns: [country, site, date, treatment, sound]
  """
  presence_rows = []

  # Agile outputs path for this country
  country_agile_dir = os.path.join(
      BASE_DIR,
      "marrs_acoustics/data",
      f"output_dir_{country}",
      "agile_outputs"
  )

  if not os.path.isdir(country_agile_dir):
    logging.warning(f"No agile_outputs directory found for {country}: {country_agile_dir}")
    return pd.DataFrame(columns=["country", "site", "date", "treatment", "sound"])

  # Each subfolder is a potential sound name
  for sound_folder in os.listdir(country_agile_dir):
    # Full path to subfolder
    folder_path = os.path.join(country_agile_dir, sound_folder)
    # We only want folders (sometimes there might be stray files)
    if not os.path.isdir(folder_path):
      continue

    # Skip if the folder is 'snap'
    if sound_folder.lower() == "snap":
      logging.info(f"Not using: {sound_folder}")
      continue

    # The inference CSV should be e.g. 'creek_inference.csv' in that folder
    csv_path = os.path.join(folder_path, f"{sound_folder}_inference.csv")
    if not os.path.isfile(csv_path):
      logging.info(f"Not using: {sound_folder} (no CSV found).")
      continue

    # If we get here, we have a valid CSV => parse it
    logging.info(f"Using: {sound_folder}")
    df_infer = pd.read_csv(csv_path)

    # Filter out rows with logit < 1.0 (beware of possible leading space in the column name)
    # Adjust as needed if your column is truly " logit" or "logit".
    # Below assumes " logit" just like in the original script:
    df_infer = df_infer[df_infer[" logit"] >= logit_cutoff]

    for _, row_inf in df_infer.iterrows():
      filename_full = row_inf["filename"]
      filename_part = filename_full.split("/", 1)[-1] if "/" in filename_full else filename_full

      treatment = parse_treatment(filename_part)
      site = parse_site(filename_part)
      date_str = parse_date(filename_part)

      # Each row => this (sound_folder) is present at site/date/treatment
      presence_rows.append((country, site, date_str, treatment, sound_folder))

  # Build and return
  df_presence = pd.DataFrame(
      presence_rows,
      columns=["country", "site", "date", "treatment", "sound"]
  )
  return df_presence


def process_country_phonic_richness(country: str, logit_cutoff: float) -> pd.DataFrame:
  """
  1) Load coverage-passing combos (using load_raw_file_list from the old script).
     => (country, site, date, treatment) for combos passing coverage.
  2) gather_sound_presence => (country, site, date, treatment, sound)
  3) For each site–date–treatment, count unique 'sound' values.
  4) Left-merge with coverage combos so we keep only coverage-passing site–dates.
     Fill missing with 0.
  """
  # Step 1: coverage combos
  df_coverage = load_raw_file_list(country)

  # Step 2: presence of various sounds
  df_presence = gather_sound_presence(country, logit_cutoff)

  # Step 3: group by site–date–treatment => count unique sounds
  if df_presence.empty:
    # No sounds found => just create a "count = 0" for all coverage combos
    df_presence_count = pd.DataFrame(
      columns=["country", "site", "date", "treatment", "count"]
    )
  else:
    # Count unique sounds
    # groupby returns multiple rows => use .nunique() on 'sound'
    df_presence_count = (
      df_presence
      .groupby(["country", "site", "date", "treatment"], as_index=False)["sound"]
      .nunique()
      .rename(columns={"sound": "count"})
    )

  # Step 4: left merge with coverage combos => fill missing with 0
  df_out = pd.merge(
    df_coverage, 
    df_presence_count, 
    how="left",
    on=["country", "site", "date", "treatment"]
  )
  df_out["count"] = df_out["count"].fillna(0).astype(int)

  return df_out


def main() -> None:
  """
  Build 'phonic richness' DataFrame for all countries, then save to OUTPUT_RICHNESS_PATH.
  """
  logging.basicConfig(level=logging.INFO)

  # We'll re-use the same list of countries from combine_counts
  # If you prefer a smaller subset, define it here:
  countries = list(COUNTRY_CONFIG.keys())

  combined_df = pd.DataFrame(columns=["country", "site", "date", "treatment", "count"])

  for country in countries:
    logging.info(f"Processing {country} for phonic richness...")
    df_country = process_country_phonic_richness(country, LOGIT_CUTOFF)
    combined_df = pd.concat([combined_df, df_country], ignore_index=True)

  # sort the order into something sensible and write csv
  combined_df.sort_values(["country", "treatment", "site", "date"], inplace=True)

  out_path = os.path.join(
    BASE_DIR,
    "marrs_acoustics/data/results/functions",
    "phonic_richness.csv"
  )
  combined_df.to_csv(out_path, index=False)
  logging.info(f"Saved phonic richness to {out_path}")


if __name__ == "__main__":
  main()
