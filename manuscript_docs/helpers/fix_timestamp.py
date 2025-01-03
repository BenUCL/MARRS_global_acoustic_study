#!/usr/bin/env python3
"""
Script to fix timestamps in audio filenames based on COUNTRY_CONFIG offsets.
"""

import os
import logging
from datetime import datetime, timedelta

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')

FOLDER_PATH = '/media/mars_5tb_drive/mars_global_acoustic_study/maldives_acoustics/raw_audio'

COUNTRY_CONFIG = {
  'australia': {'offset': -10},
  'kenya': {'offset': -3},
  'maldives': {'offset': -4},
  'mexico': {'offset': 7}
}

SHORT_CODES = {
  'aus': 'australia',
  'ken': 'kenya',
  'mal': 'maldives',
  'mex': 'mexico'
}


def correct_timestamp(filename: str) -> str:
  """
  Returns the corrected filename after adjusting the date/time.
  """
  # Split out parts: e.g. mal_D1_20211023_230000.WAV
  name_no_ext, ext = os.path.splitext(filename)  # "mal_D1_20211023_230000", ".WAV"
  parts = name_no_ext.split('_')
  
  # Expecting [prefix, site, YYYYMMDD, HHMMSS]
  if len(parts) != 4:
    return filename  # Can't parse properly, skip
  
  prefix, site_key, date_str, time_str = parts
  short_code = prefix.lower()  # e.g. 'mal'

  # Map the 3-letter code to a country
  country = SHORT_CODES.get(short_code)
  if not country:
    return filename  # Unknown short code, skip
  
  offset = COUNTRY_CONFIG[country]['offset']
  
  # Parse date/time
  # date_str e.g. "20211023", time_str e.g. "230000"
  try:
    dt_original = datetime.strptime(f"{date_str}{time_str}", "%Y%m%d%H%M%S")
  except ValueError:
    return filename  # Bad format
  
  # Adjust time: we interpret offset > 0 as time in filename is ahead -> subtract hours
  dt_new = dt_original - timedelta(hours=offset)
  
  # Build new strings
  new_date_str = dt_new.strftime('%Y%m%d')
  new_time_str = dt_new.strftime('%H%M%S')
  
  # Construct new filename
  new_filename = f"{prefix}_{site_key}_{new_date_str}_{new_time_str}{ext}"
  return new_filename

def main() -> None:
  """
  Reads all WAV files in FOLDER_PATH, corrects their timestamps, and renames them.
  """
  for fname in os.listdir(FOLDER_PATH):
    if fname.lower().endswith('.wav'):
      old_path = os.path.join(FOLDER_PATH, fname)
      new_fname = correct_timestamp(fname)
      new_path = os.path.join(FOLDER_PATH, new_fname)
      
      if new_fname != fname:
        logging.info(f"Renaming {fname} -> {new_fname}")
        os.rename(old_path, new_path)

if __name__ == '__main__':
  main()
