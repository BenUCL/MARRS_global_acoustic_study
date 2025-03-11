#!/usr/bin/env python3
"""
Script to fix timestamps in audio filenames based on COUNTRY_CONFIG offsets.
WARNING: Run only sites one by one. Do not run a full raw_audio folder as it adds a digit to the end
of some files which would otherwise be duplicated.
"""

import os
import re
import logging
from datetime import datetime, timedelta
from typing import Dict, List

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")

FOLDER_PATH = "/media/bwilliams/New Volume/mars_global_acoustic_study/australia_acoustics/raw_audio"

# Time offset. What the audio should be relative to the original.
# So if -3 that means the audio timestamp should be shifted from 4am to 7am for example.
COUNTRY_CONFIG: Dict[str, Dict[str, int]] = {
  "australia": {"offset": -10},
  "kenya": {"offset": -3},
  "maldives": {"offset": -4},
  "mexico": {"offset": 7}
}

SHORT_CODES: Dict[str, str] = {
  "aus": "australia",
  "ken": "kenya",
  "mal": "maldives",
  "mex": "mexico"
}


def correct_timestamp(filename: str) -> str:
  """
  Returns the corrected filename after adjusting the date/time.
  """
  # Split out parts: e.g. mal_D1_20211023_230000.WAV
  name_no_ext, ext = os.path.splitext(filename)  # "mal_D1_20211023_230000", ".WAV"
  parts = name_no_ext.split("_")
  
  # Expecting [prefix, site, YYYYMMDD, HHMMSS]
  if len(parts) != 4:
    return filename  # Can't parse properly, skip
  
  prefix, site_key, date_str, time_str = parts
  short_code = prefix.lower()  # e.g. "mal"

  # Map the 3-letter code to a country
  country = SHORT_CODES.get(short_code)
  if not country:
    return filename  # Unknown short code, skip
  
  offset = COUNTRY_CONFIG[country]["offset"]
  
  try:
    dt_original = datetime.strptime(f"{date_str}{time_str}", "%Y%m%d%H%M%S")
  except ValueError:
    return filename  # Bad format
  
  # Adjust time: subtracting offset hours (e.g. offset=-3 adds 3 hours)
  dt_new = dt_original - timedelta(hours=offset)
  
  new_date_str = dt_new.strftime("%Y%m%d")
  new_time_str = dt_new.strftime("%H%M%S")
  
  new_filename = f"{prefix}_{site_key}_{new_date_str}_{new_time_str}{ext}"
  return new_filename


def main() -> None:
  """
  Reads all WAV files in FOLDER_PATH, corrects their timestamps, and renames them.
  Avoids filename collisions by appending a counter if necessary.
  """
  for fname in os.listdir(FOLDER_PATH):
    if fname.lower().endswith(".wav"):
      old_path = os.path.join(FOLDER_PATH, fname)
      new_fname = correct_timestamp(fname)
      
      if new_fname == fname:
        continue
      
      # Check for collision and append counter if needed.
      unique_fname = new_fname
      new_path = os.path.join(FOLDER_PATH, unique_fname)
      counter = 1
      while os.path.exists(new_path):
        name_no_ext, ext = os.path.splitext(new_fname)
        unique_fname = f"{name_no_ext}_{counter}{ext}"
        new_path = os.path.join(FOLDER_PATH, unique_fname)
        counter += 1
      
      logging.info(f"Renaming {fname} -> {unique_fname}")
      os.rename(old_path, new_path)


def finalise_filenames() -> None:
  """
  Checks for duplicates (ignoring the counter and extension) and:
  1. Prints a message if duplicates are found.
  2. Removes the counter bit from the filename if there's only one file in that group.
  """
  # Regex to capture base filename and optional counter: e.g. ken_D1_20230301_201600 or ken_D1_20230301_201600_1
  pattern = re.compile(r"^(?P<base>[^_]+_[^_]+_\d{8}_\d{6})(?:_(?P<counter>\d+))?(?P<ext>\.wav)$", re.IGNORECASE)
  files = os.listdir(FOLDER_PATH)
  groups: Dict[str, List[str]] = {}

  for f in files:
    if not f.lower().endswith(".wav"):
      continue
    m = pattern.match(f)
    if m:
      base = m.group("base")
      groups.setdefault(base, []).append(f)
    else:
      logging.warning(f"File does not match expected pattern: {f}")
  
  for base, file_list in groups.items():
    if len(file_list) == 1:
      f = file_list[0]
      m = pattern.match(f)
      # If a counter exists, rename to remove it.
      if m and m.group("counter"):
        new_name = f"{m.group('base')}{m.group('ext')}"
        old_path = os.path.join(FOLDER_PATH, f)
        new_path = os.path.join(FOLDER_PATH, new_name)
        # Only rename if the target name doesn't already exist.
        if not os.path.exists(new_path):
          #logging.info(f"Removing counter: renaming {f} -> {new_name}")
          os.rename(old_path, new_path)
    else:
      logging.info(f"Duplicate group found: {base} has {len(file_list)} files")


if __name__ == "__main__":
  main()
  finalise_filenames()
