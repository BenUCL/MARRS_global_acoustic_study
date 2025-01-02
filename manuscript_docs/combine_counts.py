#!/usr/bin/env python3
"""
Combine inference CSV data by country, treatment, site, and date into a single CSV.

Steps:
1. For each country, load its "raw_file_list.csv" (full set of possible recordings).
   - Parse each filename to extract site, date (with time offset), and treatment.
   - Collect unique combinations of (country, site, date, treatment).
   - Count how many files per site–date combo, exclude any with <90% of expected count.
2. Load the inference CSV in the agile_outputs folder for each country (if it exists).
   - Parse filenames in the same way (site, date with offset, treatment).
   - Group by (country, site, date, treatment), count how many inferences per group.
   - Multiply the count by the duty_cycle for that country.
3. Perform a left-merge of the raw_file_list combos with the inference counts.
   - Any missing combos in the inference dataset get a count of 0.
4. Append all countries' results into one DataFrame.
5. Save the combined data to OUTPUT_PATH.
"""

import os
import logging
import pandas as pd
from datetime import datetime, timedelta

# Global variables
SOUND = "scrape"
LOGIT_CUTOFF = 1.0

# If a date has < the COVERAGE_THRESHOLD * expected_daily recordings, exclude it
FILE_COVERAGE = 0.9

# Dictionary holding time offsets (hrs) and duty cycles
COUNTRY_CONFIG = {
  "australia": {"offset": -10, "duty_cycle": 4},
  "kenya": {"offset": -3, "duty_cycle": 4},
  "indonesia": {"offset": 0,  "duty_cycle": 2},
  "maldives": {"offset": +5, "duty_cycle": 4},
  "mexico": {"offset": +7, "duty_cycle": 4}
}
COUNTRIES = list(COUNTRY_CONFIG.keys())

BASE_DIR = os.getenv("BASE_DIR")
if not BASE_DIR:
  raise ValueError("BASE_DIR environment variable is not set.")

OUTPUT_PATH = os.path.join(
  BASE_DIR,
  "marrs_acoustics/data/results/functions",
  f"{SOUND}_combined_count.csv"
)

def parse_treatment(filename_part: str) -> str:
  """
  Return treatment from the 5th character (0-based index 4).
  E.g. 'ind_D2_20220830_130600.WAV':
       0 1 2 3 4 5 ...
       i n d _ D ...
       => 'D' => "degraded"
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
  Parse site from the chunk after the first underscore.
  E.g. 'ind_D2_20220830_130600.WAV' -> site='D2'
  """
  parts = filename_part.split("_")
  return parts[1] if len(parts) > 1 else "unknown"

def parse_date(filename_part: str, country: str) -> str:
  """
  Parse date/time from filename, apply the offset, return date in YYYYMMDD format.
  E.g. 'ind_D2_20220830_130600.WAV' -> '20220830_130600'
  """
  dt_str = filename_part[7:7+15]  # '20220830_130600'
  dt_obj = datetime.strptime(dt_str, "%Y%m%d_%H%M%S")

  offset_hrs = COUNTRY_CONFIG[country]["offset"]
  corrected_dt = dt_obj + timedelta(hours=offset_hrs)
  return corrected_dt.strftime("%Y%m%d")

def get_expected_daily_recordings(duty_cycle: int) -> int:
  """
  Return how many files we expect in one full day for a given duty cycle.
  If '4' => 1 min on, 3 min off => 4 minutes per cycle => 15 cycles per hour => 360 a day.
  If '2' => 1 min on, 1 min off => 2 minutes per cycle => 30 cycles per hour => 720 a day.
  """
  minutes_per_day = 24 * 60
  return int(minutes_per_day / duty_cycle)

def load_raw_file_list(country: str) -> pd.DataFrame:
  """
  Load and parse the raw_file_list.csv for the given country.
  Return a DataFrame of unique combos: country, site, date, treatment.
  Exclude any site–date combos that have <90% of the expected file count.
  """
  raw_list_path = os.path.join(
    BASE_DIR,
    "marrs_acoustics/data",
    f"output_dir_{country}",
    "raw_file_list.csv"
  )

  if not os.path.isfile(raw_list_path):
    logging.warning(f"raw_file_list.csv not found for {country}. Path: {raw_list_path}")
    return pd.DataFrame(columns=["country", "site", "date", "treatment"])

  df_raw = pd.read_csv(raw_list_path)
  rows = []
  for _, row in df_raw.iterrows():
    filename_full = row["filename"]
    filename_part = filename_full.split("/", 1)[-1] if "/" in filename_full else filename_full

    treatment = parse_treatment(filename_part)
    site = parse_site(filename_part)
    date_str = parse_date(filename_part, country)

    rows.append((country, site, date_str, treatment))

  # Don't drop duplicates yet, keep one row per file
  df_out = pd.DataFrame(rows, columns=["country", "site", "date", "treatment"])

  # Count how many files (rows) per site–date
  df_counts = df_out.groupby(["site", "date"]).size().reset_index(name="n_files")

  duty_cycle = COUNTRY_CONFIG[country]["duty_cycle"]
  expected_daily = get_expected_daily_recordings(duty_cycle)
  coverage_threshold = FILE_COVERAGE * expected_daily

  # Identify insufficient coverage
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
  df_valid = pd.merge(df_out, df_counts[["site", "date"]], on=["site", "date"], how="inner")

  # Now if you want unique site–date–treatment combos in the final result,
  # you can drop duplicates if needed. For counting coverage, we needed them all.
  df_valid.drop_duplicates(inplace=True)

  return df_valid


def load_inference_counts(country: str, sound: str, logit_cutoff: float) -> pd.DataFrame:
  """
  Read the CSV from agile_outputs for the given country/sound.
  Parse filenames to get (country, site, date, treatment).
  Filter out rows with logit < 1.0, group, and multiply by duty_cycle.
  """
  csv_path = os.path.join(
    BASE_DIR,
    "marrs_acoustics/data",
    f"output_dir_{country}",
    f"agile_outputs/{sound}",
    f"{sound}_inference.csv"
  )

  if not os.path.isfile(csv_path):
    logging.warning(f"CSV does not exist for {country}, {sound}. Tried: {csv_path}")
    return pd.DataFrame(columns=["country", "site", "date", "treatment", "count"])

  df_infer = pd.read_csv(csv_path)
  # Filter out rows with logit < 1.0, BEWARE OF LEADING SPACE IN ' logit'
  df_infer = df_infer[df_infer[" logit"] >= logit_cutoff]

  rows = []
  for _, row in df_infer.iterrows():
    filename_full = row["filename"]
    filename_part = filename_full.split("/", 1)[-1] if "/" in filename_full else filename_full

    treatment = parse_treatment(filename_part)
    site = parse_site(filename_part)
    date_str = parse_date(filename_part, country)

    rows.append((country, site, date_str, treatment))

  df_temp = pd.DataFrame(rows, columns=["country", "site", "date", "treatment"])
  df_counts = (
    df_temp.groupby(["country", "site", "date", "treatment"], as_index=False)
    .size()
    .rename(columns={"size": "count"})
  )

  duty_cycle = COUNTRY_CONFIG[country]["duty_cycle"]
  df_counts["count"] = df_counts["count"] * duty_cycle

  return df_counts


def process_country(country: str, sound: str, logit_cutoff: float) -> pd.DataFrame:
  """
  1) Load raw_file_list combos -> (country, site, date, treatment).
  2) Load inference counts -> (country, site, date, treatment, count).
  3) Merge raw combos with counts (left join), fill missing counts with 0.
  """
  df_raw_combos = load_raw_file_list(country)
  df_infer_counts = load_inference_counts(country, sound, logit_cutoff)

  # Merge
  df_merged = pd.merge(
    df_raw_combos,
    df_infer_counts,
    how="left",
    on=["country", "site", "date", "treatment"]
  )
  df_merged["count"] = df_merged["count"].fillna(0).astype(int)

  return df_merged

def main() -> None:
  """
  Build combined DataFrame across all countries, then save to OUTPUT_PATH.
  """
  logging.basicConfig(level=logging.INFO)

  combined_df = pd.DataFrame(columns=["country", "site", "date", "treatment", "count"])

  for country in COUNTRIES:
    logging.info(f"Processing {country}...")
    df_country = process_country(country, SOUND, LOGIT_CUTOFF)
    combined_df = pd.concat([combined_df, df_country], ignore_index=True)
  
  # sort the order into something sensible and write csv
  combined_df.sort_values(["country", "treatment", "site", "date"], inplace=True)
  combined_df.to_csv(OUTPUT_PATH, index=False)
  logging.info(f"Saved combined counts to {OUTPUT_PATH}")

if __name__ == "__main__":
  main()


# #!/usr/bin/env python3
# """
# Combine inference CSV data by country, treatment, site, and date into a single CSV.

# Steps:
# 1. For each country, load its "raw_file_list.csv" (full set of possible recordings).
#    - Parse each filename to extract site, date (with time offset), and treatment.
#    - Collect unique combinations of (country, site, date, treatment).
# 2. Load the inference CSV in the agile_outputs folder for each country (if it exists).
#    - Parse filenames in the same way (site, date with offset, treatment).
#    - Group by (country, site, date, treatment), count how many inferences per group.
#    - Multiply the count by the duty_cycle for that country.
# 3. Perform a left-merge of the raw_file_list combos with the inference counts.
#    - Any missing combos in the inference dataset get a count of 0.
# 4. Append all countries' results into one DataFrame.
# 5. Save the combined data to OUTPUT_PATH.
# """

# import os
# import logging
# import pandas as pd
# from datetime import datetime, timedelta

# # Global variables
# SOUND = "snaps"
# LOGIT_CUTOFF = 1.0

# # Dictionary holding time offsets (hrs) and duty cycles
# COUNTRY_CONFIG = {
#   "australia": {"offset": -10, "duty_cycle": 4},
#   "kenya": {"offset": -3, "duty_cycle": 4},
#   "indonesia": {"offset": 0,  "duty_cycle": 2},
#   "maldives": {"offset": +5, "duty_cycle": 4},
#   "mexico": {"offset": +7, "duty_cycle": 4}
# }
# COUNTRIES = list(COUNTRY_CONFIG.keys())

# BASE_DIR = os.getenv("BASE_DIR")
# if not BASE_DIR:
#   raise ValueError("BASE_DIR environment variable is not set.")

# OUTPUT_PATH = os.path.join(
#   BASE_DIR,
#   "marrs_acoustics/data/results/functions",
#   f"{SOUND}_combined_count.csv"
# )

# def parse_treatment(filename_part: str) -> str:
#   """
#   Return treatment from the 5th character (0-based index 4).
#   E.g. 'ind_D2_20220830_130600.WAV':
#        0 1 2 3 4 5 ...
#        i n d _ D ...
#        => 'D' => "degraded"
#   """
#   if len(filename_part) <= 4:
#     return "unknown"
#   char = filename_part[4]
#   if char == "H":
#     return "healthy"
#   if char == "D":
#     return "degraded"
#   if char == "R":
#     return "restored"
#   if char == "N":
#     return "newly_restored"
#   return "unknown"

# def parse_site(filename_part: str) -> str:
#   """
#   Parse site from the chunk after the first underscore.
#   E.g. 'ind_D2_20220830_130600.WAV' -> site='D2'
#   """
#   parts = filename_part.split("_")
#   return parts[1] if len(parts) > 1 else "unknown"

# def parse_date(filename_part: str, country: str) -> str:
#   """
#   Parse date/time from filename, apply the offset, return date in YYYYMMDD format.
#   E.g. 'ind_D2_20220830_130600.WAV' -> '20220830_130600'
#   """
#   dt_str = filename_part[7:7+15]  # '20220830_130600'
#   dt_obj = datetime.strptime(dt_str, "%Y%m%d_%H%M%S")

#   offset_hrs = COUNTRY_CONFIG[country]["offset"]
#   corrected_dt = dt_obj + timedelta(hours=offset_hrs)
#   return corrected_dt.strftime("%Y%m%d")

# def load_raw_file_list(country: str) -> pd.DataFrame:
#   """
#   Load and parse the raw_file_list.csv for the given country.
#   Return a DataFrame of unique combos: country, site, date, treatment.
#   """
#   raw_list_path = os.path.join(
#     BASE_DIR,
#     "marrs_acoustics/data",
#     f"output_dir_{country}",
#     "raw_file_list.csv"
#   )

#   if not os.path.isfile(raw_list_path):
#     logging.warning(f"raw_file_list.csv not found for {country}. Path: {raw_list_path}")
#     # Return an empty DataFrame but with expected cols
#     return pd.DataFrame(columns=["country", "site", "date", "treatment"])

#   df_raw = pd.read_csv(raw_list_path)
#   rows = []
#   for _, row in df_raw.iterrows():
#     filename_full = row["filename"]
#     filename_part = filename_full.split("/", 1)[-1] if "/" in filename_full else filename_full

#     treatment = parse_treatment(filename_part)
#     site = parse_site(filename_part)
#     date_str = parse_date(filename_part, country)

#     rows.append((country, site, date_str, treatment))

#   # Unique combos
#   df_out = pd.DataFrame(rows, columns=["country", "site", "date", "treatment"])
#   df_out.drop_duplicates(inplace=True)
#   return df_out

# def load_inference_counts(country: str, sound: str, logit_cutoff: float) -> pd.DataFrame:
#   """
#   Read the CSV from agile_outputs for the given country/sound.
#   Parse filenames to get (country, site, date, treatment).
#   Filter out rows with logit < 1.0, group, and multiply by duty_cycle.
#   """
#   csv_path = os.path.join(
#     BASE_DIR,
#     "marrs_acoustics/data",
#     f"output_dir_{country}",
#     f"agile_outputs/{sound}",
#     f"{sound}_inference.csv"
#   )

#   if not os.path.isfile(csv_path):
#     logging.warning(f"CSV does not exist for {country}, {sound}. Tried: {csv_path}")
#     return pd.DataFrame(columns=["country", "site", "date", "treatment", "count"])

#   df_infer = pd.read_csv(csv_path)
#   # Filter out rows with logit < 1.0, BEWARE OF LEADING SPACE IN ' logit'
#   df_infer = df_infer[df_infer[" logit"] >= logit_cutoff]

#   rows = []
#   for _, row in df_infer.iterrows():
#     filename_full = row["filename"]
#     filename_part = filename_full.split("/", 1)[-1] if "/" in filename_full else filename_full

#     treatment = parse_treatment(filename_part)
#     site = parse_site(filename_part)
#     date_str = parse_date(filename_part, country)

#     rows.append((country, site, date_str, treatment))

#   df_temp = pd.DataFrame(rows, columns=["country", "site", "date", "treatment"])
#   df_counts = (
#     df_temp.groupby(["country", "site", "date", "treatment"], as_index=False)
#     .size()
#     .rename(columns={"size": "count"})
#   )

#   duty_cycle = COUNTRY_CONFIG[country]["duty_cycle"]
#   df_counts["count"] = df_counts["count"] * duty_cycle

#   return df_counts


# def process_country(country: str, sound: str, logit_cutoff: float) -> pd.DataFrame:
#   """
#   1) Load raw_file_list combos -> (country, site, date, treatment).
#   2) Load inference counts -> (country, site, date, treatment, count).
#   3) Merge raw combos with counts (left join), fill missing counts with 0.
#   """
#   df_raw_combos = load_raw_file_list(country)
#   df_infer_counts = load_inference_counts(country, sound, logit_cutoff)

#   # Merge
#   df_merged = pd.merge(
#     df_raw_combos,
#     df_infer_counts,
#     how="left",
#     on=["country", "site", "date", "treatment"]
#   )
#   df_merged["count"] = df_merged["count"].fillna(0).astype(int)

#   return df_merged

# def main() -> None:
#   """
#   Build combined DataFrame across all countries, then save to OUTPUT_PATH.
#   """
#   logging.basicConfig(level=logging.INFO)

#   combined_df = pd.DataFrame(columns=["country", "site", "date", "treatment", "count"])

#   for country in COUNTRIES:
#     logging.info(f"Processing {country}...")
#     df_country = process_country(country, SOUND, LOGIT_CUTOFF)
#     combined_df = pd.concat([combined_df, df_country], ignore_index=True)
  
#   # sort the order into something sensible and write csv
#   combined_df.sort_values(["country", "treatment", "site", "date"], inplace=True)
#   combined_df.to_csv(OUTPUT_PATH, index=False)
#   logging.info(f"Saved combined counts to {OUTPUT_PATH}")

# if __name__ == "__main__":
#   main()