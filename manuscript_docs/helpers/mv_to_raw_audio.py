import os
import shutil
import logging
from typing import List

# Global constants
BASE_DIR = "/media/bwilliams/New Volume/marrs_acoustics/australia_acoustics"
RAW_AUDIO_DIR = os.path.join(BASE_DIR, "raw_audio")

def move_audio_files() -> None:
  """Move all .WAV audio files from subdirectories in BASE_DIR into the raw_audio folder.

  Only directories (other than raw_audio) are processed. If a file already exists in
  raw_audio, it is skipped.
  """
  for entry in os.listdir(BASE_DIR):
    entry_path = os.path.join(BASE_DIR, entry)
    # Only process directories excluding raw_audio
    if os.path.isdir(entry_path) and entry != "raw_audio":
      for filename in os.listdir(entry_path):
        if filename.upper().endswith(".WAV"):
          source_path = os.path.join(entry_path, filename)
          dest_path = os.path.join(RAW_AUDIO_DIR, filename)
          if os.path.exists(dest_path):
            logging.warning("File %s already exists in raw_audio. Skipping.", filename)
            continue
          logging.info("Moving '%s' to '%s'", source_path, dest_path)
          shutil.move(source_path, dest_path)

if __name__ == "__main__":
  logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
  move_audio_files()