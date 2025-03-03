"""Rename R2-> N1, rename R3 -> R2"""
import os
import logging
from typing import List

# Global constants
RAW_AUDIO_DIR = "/media/bwilliams/New Volume/marrs_acoustics/mexico_acoustics/raw_audio"

def rename_files() -> None:
  """Rename audio files by replacing site codes in two passes.

  First pass: Replace files with site code 'R2' with 'N1'.
  Second pass: Replace files with site code 'R3' with 'R2'.
  """
  # First pass: Replace R2 with N1
  for filename in os.listdir(RAW_AUDIO_DIR):
    file_path = os.path.join(RAW_AUDIO_DIR, filename)
    if not os.path.isfile(file_path) or not filename.upper().endswith(".WAV"):
      continue
    parts: List[str] = filename.split("_")
    if len(parts) < 3:
      continue
    if parts[1] == "R2":
      parts[1] = "N1"
      new_filename = "_".join(parts)
      new_path = os.path.join(RAW_AUDIO_DIR, new_filename)
      logging.info("Renaming '%s' to '%s'", file_path, new_path)
      os.rename(file_path, new_path)

  # Second pass: Replace R3 with R2
  for filename in os.listdir(RAW_AUDIO_DIR):
    file_path = os.path.join(RAW_AUDIO_DIR, filename)
    if not os.path.isfile(file_path) or not filename.upper().endswith(".WAV"):
      continue
    parts = filename.split("_")
    if len(parts) < 3:
      continue
    if parts[1] == "R3":
      parts[1] = "R2"
      new_filename = "_".join(parts)
      new_path = os.path.join(RAW_AUDIO_DIR, new_filename)
      logging.info("Renaming '%s' to '%s'", file_path, new_path)
      os.rename(file_path, new_path)

if __name__ == "__main__":
  logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
  rename_files()
