#!/usr/bin/env python3
"""
Combine inference CSV data by country, treatment, site, and date into a single CSV.

This script:
1. Reads a CSV of inference data for each country (for a given sound type).
2. Corrects timestamps based on known offsets per country.
3. Extracts treatment and site from each filename.
4. Appends all countries' data into one summary DataFrame with counts,
   multiplied by a duty cycle specific to each country.
5. Saves the combined data to OUTPUT_PATH.
"""

import os
import logging
import pandas as pd
from datetime import datetime, timedelta

# Global variables
SOUND = "snaps"

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
  Return treatment from the 5th character of the filename_part.
  E.g. 'ind_D2_20220830_130600.WAV' -> the 5th char might be 'D' -> 'degraded'.
  """
  treatment_char = filename_part[4] if len(filename_part) > 4 else ""
  if treatment_char == "H":
    return "healthy"
  if treatment_char == "D":
    return "degraded"
  if treatment_char == "R":
    return "restored"
  if treatment_char == "N":
    return "newly_restored"
  return "unknown"

def parse_site(filename_part: str) -> str:
  """
  Parse site from the chunk after the first underscore, e.g. 'D2' in 'ind_D2_20220830_130600'.
  """
  parts = filename_part.split("_")
  return parts[1] if len(parts) > 1 else "unknown"

def parse_date(filename_part: str, country: str) -> str:
  """
  Parse date/time from filename, apply the offset, return date in YYYYMMDD format.
  
  Example filename_part: 'ind_D2_20220830_130600.WAV'
  The date/time part starts at index 7 and is 15 chars: '20220830_130600'.
  """
  dt_str = filename_part[7:7+15]
  dt_obj = datetime.strptime(dt_str, "%Y%m%d_%H%M%S")

  # Apply offset based on COUNTRY_CONFIG
  hours_to_shift = COUNTRY_CONFIG[country]["offset"]
  corrected_dt = dt_obj + timedelta(hours=hours_to_shift)

  return corrected_dt.strftime("%Y%m%d")

def process_inference_csv(country: str, sound: str) -> pd.DataFrame:
  """
  Read the CSV for the given country/sound, parse data, group, 
  and return a summary DataFrame with counts multiplied by duty cycle.
  """
  csv_path = os.path.join(
    BASE_DIR,
    "marrs_acoustics/data",
    f"output_dir_{country}/agile_outputs/{sound}",
    f"{sound}_inference.csv"
  )

  if not os.path.isfile(csv_path):
    logging.warning(f"CSV does not exist for {country}, {sound}. Tried: {csv_path}")
    return pd.DataFrame(columns=["country", "treatment", "site", "date", "count"])

  df = pd.read_csv(csv_path)
  rows = []
  for _, row in df.iterrows():
    filename_full = row["filename"]
    filename_part = filename_full.split("/", 1)[-1] if "/" in filename_full else filename_full

    treatment = parse_treatment(filename_part)
    site = parse_site(filename_part)
    date_str = parse_date(filename_part, country)

    rows.append((country, treatment, site, date_str))

  out_df = pd.DataFrame(rows, columns=["country", "treatment", "site", "date"])
  out_df = (
    out_df.groupby(["country", "treatment", "site", "date"], as_index=False)
    .size()
    .rename(columns={"size": "count"})
  )

  # Multiply 'count' by duty_cycle from COUNTRY_CONFIG
  duty_cycle = COUNTRY_CONFIG[country]["duty_cycle"]
  out_df["count"] = out_df["count"] * duty_cycle

  return out_df

def main() -> None:
  """
  Loop over all countries for the chosen SOUND, combine them, and save to OUTPUT_PATH.
  """
  logging.basicConfig(level=logging.INFO)

  combined_df = pd.DataFrame(columns=["country", "treatment", "site", "date", "count"])

  for country in COUNTRIES:
    logging.info(f"Processing {country}...")
    country_df = process_inference_csv(country, SOUND)
    if not country_df.empty:
      combined_df = pd.concat([combined_df, country_df], ignore_index=True)

  combined_df.to_csv(OUTPUT_PATH, index=False)
  logging.info(f"Saved combined counts to {OUTPUT_PATH}")

if __name__ == "__main__":
  main()













# #!/usr/bin/env python3
# """
# Combine inference CSV data by country, treatment, site, and date into a single CSV.

# This script:
# 1. Reads a CSV of inference data for each country (for a given sound type).
# 2. Corrects timestamps based on known offsets per country.
# 3. Extracts treatment and site from each filename.
# 4. Appends all countries' data into one summary DataFrame with counts.
# 5. Saves the combined data to OUT_PATH.
# """

# import os
# import logging
# import pandas as pd
# from datetime import datetime, timedelta

# # Global variables
# SOUND = "scrape"
# COUNTRIES = ["australia", "kenya", "indonesia", "maldives", "mexico"]
# TIME_OFFSETS = {
#   "australia": -10,  # filename is 10 hrs behind, so subtract 10 from the parsed time
#   "kenya": -3,
#   "indonesia": 0,
#   "maldives": +5,
#   "mexico": +7
# }

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
#   Return treatment from the 5th character after the first '/'.
#   """
#   treatment_char = filename_part[4] if len(filename_part) > 4 else ""
#   if treatment_char == "H":
#     return "healthy"
#   elif treatment_char == "D":
#     return "degraded"
#   elif treatment_char == "R":
#     return "restored"
#   elif treatment_char == "N":
#     return "newly_restored"
#   return "unknown"

# def parse_site(filename_part: str) -> str:
#   """
#   Parse site from the chunk after the first underscore, e.g 'D2' in 'ind_D2_20220830_130600'.
#   """
#   parts = filename_part.split("_")
#   return parts[1] if len(parts) > 1 else "unknown"

# def parse_date(filename_part: str, country: str) -> str:
#   """
#   Parse date/time from filename, apply the offset, return date in YYYYMMDD format.
#   """
#   # Example filename_part: 'ind_D2_20220830_130600.WAV'
#   # The date/time part starts at index 7 and is 15 chars: '20220830_130600'
#   dt_str = filename_part[7:7+15]  # '20220830_130600'
#   dt_obj = datetime.strptime(dt_str, "%Y%m%d_%H%M%S")
#   # Apply offset
#   hours_to_shift = TIME_OFFSETS[country]
#   corrected_dt = dt_obj + timedelta(hours=hours_to_shift)
#   return corrected_dt.strftime("%Y%m%d")

# def process_inference_csv(country: str, sound: str) -> pd.DataFrame:
#   """
#   Read the CSV for the given country/sound, parse and group data, return summary DataFrame.
#   """
#   csv_path = os.path.join(
#     BASE_DIR,
#     "marrs_acoustics/data",
#     f"output_dir_{country}/agile_outputs/{sound}/",
#     f"{sound}_inference.csv"
#   )

#   if not os.path.isfile(csv_path):
#     logging.warning(f"CSV does not exist for {country}, {sound}. Tried: {csv_path}")
#     return pd.DataFrame(columns=["country", "treatment", "site", "date", "count"])

#   df = pd.read_csv(csv_path)

#   rows = []
#   for _, row in df.iterrows():
#     filename_full = row["filename"]
#     filename_part = filename_full.split("/", 1)[-1] if "/" in filename_full else filename_full

#     treatment = parse_treatment(filename_part)
#     site = parse_site(filename_part)
#     date_str = parse_date(filename_part, country)

#     rows.append((country, treatment, site, date_str))

#   out_df = pd.DataFrame(rows, columns=["country", "treatment", "site", "date"])
#   out_df = (
#     out_df.groupby(["country", "treatment", "site", "date"], as_index=False)
#     .size()
#     .rename(columns={"size": "count"})
#   )

#   return out_df

# def main() -> None:
#   """
#   Loop over all countries for the chosen SOUND, combine, and save to OUTPUT_PATH.
#   """
#   logging.basicConfig(level=logging.INFO)

#   combined_df = pd.DataFrame(columns=["country", "treatment", "site", "date", "count"])

#   for country in COUNTRIES:
#     logging.info(f"Processing {country}...")
#     country_df = process_inference_csv(country, SOUND)
#     if not country_df.empty:
#       combined_df = pd.concat([combined_df, country_df], ignore_index=True)

#   # Save combined
#   combined_df.to_csv(OUTPUT_PATH, index=False)
#   logging.info(f"Saved combined counts to {OUTPUT_PATH}")

# if __name__ == "__main__":
#   main()
