"""Rename indonesia files with correct prefix"""

import os
import logging
from typing import Dict

# Global constants
BASE_DIR = "/media/bwilliams/New Volume/marrs_acoustics/indonesia_acoustics/raw_audio"

# Site mapping to two-character codes
TWO_CHAR_SITE_MAPPING: Dict[str, str] = {
    "Degraded M02 Salisi Kecil": "D1",
    "Degraded M08 Gosong Bontosua": "D6",
    "Degraded M13 Salisi Besar NCS1": "D2",
    "Degraded M14 Salisi Besar CES": "D4",
    "Degraded M16 Salisi Besar SES": "D5",
    "Degraded M17 Bad Control 3": "D3",
    "Healthy M01 Good Control 3": "H1",
    "Healthy M04 Control 2": "H2", 
    "Healthy M07 Salisi Kecil": "H3",
    "Healthy M05 Gosong Bontosua North": "H4",
    "Healthy M10 Gosong Bontosua Middle": "H5",
    "Healthy M11 Gosong Bontosua South": "H6",
    "Restored M03 Block 4": "R1",
    "Restored M12 Salisi Kecil": "R5",
    "Restored M06 Block 6": "R3",
    "Restored M09 Block 7": "R4",
    "Restored M15 Salisi Besar Hope CWS": "R6",
    "Restored M18 Block 3": "R2",
    "New Restored M19 Block K": "N1",
    "New Restored M20 Block G": "N2",
    "New Restored M21 Block D": "N3",
}

def rename_audio_files() -> None:
  """Rename audio files by adding a site-specific prefix."""
  for site_dir in os.listdir(BASE_DIR):
    site_path = os.path.join(BASE_DIR, site_dir)
    if not os.path.isdir(site_path):
      continue
    site_code = TWO_CHAR_SITE_MAPPING.get(site_dir)
    if not site_code:
      logging.warning("No mapping for site: %s", site_dir)
      continue
    prefix = f"ind_{site_code}_"
    for filename in os.listdir(site_path):
      file_path = os.path.join(site_path, filename)
      if not os.path.isfile(file_path):
        continue
      # Skip if already renamed
      if filename.startswith(prefix):
        continue
      new_name = prefix + filename
      new_path = os.path.join(site_path, new_name)
      logging.info("Renaming '%s' to '%s'", file_path, new_path)
      os.rename(file_path, new_path)

if __name__ == "__main__":
  logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
  rename_audio_files()
