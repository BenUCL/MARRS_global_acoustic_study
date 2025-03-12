#!/usr/bin/env python3
"""
Use this to write out detections for the final false positive check.
This script processes inference CSVs for multiple countries and sounds.
For each country, it finds each sound folder that contains a CSV (e.g. croak_inference.csv),
filters rows with logit >= 1.0, selects eligible rows, and writes out 5-second audio clips.
The new filename starts with the logit score, then the timestamp in seconds (with "s"),
and finally the original filename. This can be done at random or for set logit thresholds.
"""

import os
import logging
from pathlib import Path
from typing import Literal, Optional, List
import pandas as pd
import librosa
import soundfile as sf
from tqdm import tqdm

# Global constants
SELECT_MODE: Literal["random", "ordered"] = "ordered"  # 'random' or 'ordered'
COUNTRIES: List[str] = ["maldives", "kenya", "mexico", "indonesia", "australia"]
LOGIT_CUTOFF: float = 1.0  # Minimum logit score to write out. Lower will write more detections but increase the false positive rate.
NUM_FILES: Optional[int] = None  # Set to None to process all eligible files.
SEED: int = 42

# Global paths (that don't change per country)
BASE_AUDIO_DIR: Path = Path("/media/bwilliams/New Volume/mars_global_acoustic_study")
DETECTIONS_DIR: Path = BASE_AUDIO_DIR / "detections"

# Path patterns that iterate over country and sound during loop
OUTPUT_DIR_PATTERN: str = "marrs_acoustics/data/output_dir_{country}/agile_outputs"
INFERENCE_CSV_PATTERN: str = "{sound}/{sound}_inference.csv"

def process_sound_folder(country: str, sound: str, base_dir: str) -> None:
  """
  Process a single sound folder for a given country.

  Args:
      country: Country name.
      sound: Sound folder name.
      base_dir: Base directory from environment variable.
  """
  # Build paths using global constants and country-specific parts
  audio_base_path: Path = BASE_AUDIO_DIR / f"{country}_acoustics"
  save_dir: Path = DETECTIONS_DIR
  save_dir.mkdir(parents=True, exist_ok=True)
  csv_path: Path = Path(base_dir) / OUTPUT_DIR_PATTERN.format(country=country) / INFERENCE_CSV_PATTERN.format(sound=sound)
  if not csv_path.exists():
    logging.info(f"CSV not found: {csv_path}. Skipping {sound} in {country}.")
    return

  df = pd.read_csv(csv_path)
  df.columns = df.columns.str.strip()
  df_filtered = df[df["logit"] >= LOGIT_CUTOFF]
  if df_filtered.empty:
    logging.info(f"No rows with logit >= {LOGIT_CUTOFF} in {csv_path}.")
    return

  if SELECT_MODE == "random":
    df_sample = (
      df_filtered.sample(n=NUM_FILES, random_state=SEED)
      if NUM_FILES and len(df_filtered) > NUM_FILES
      else df_filtered
    )
  elif SELECT_MODE == "ordered":
    df_sample = (
      df_filtered.sort_values("logit").head(NUM_FILES)
      if NUM_FILES
      else df_filtered
    )
  else:
    logging.error(f"Unknown selection mode: {SELECT_MODE}. Defaulting to all rows.")
    df_sample = df_filtered

  for _, row in tqdm(df_sample.iterrows(), total=len(df_sample), desc=f"Processing {country}/{sound}"):
    try:
      filename: str = row["filename"].strip()
      timestamp_s: float = float(row["timestamp_s"])
      logit: float = float(row["logit"])
      full_audio_path: Path = audio_base_path / filename

      # Load a 5-second clip starting at timestamp_s
      y, sr = librosa.load(str(full_audio_path), sr=None, offset=timestamp_s, duration=5.0)
      timestamp_str = f"start-{int(timestamp_s):02d}s"
      new_filename = f"logit-{logit:.2f}_{timestamp_str}_{os.path.basename(filename)}"
      save_path: Path = save_dir / new_filename
      sf.write(str(save_path), y, sr)
    except Exception as e:
      logging.error(f"Error processing {filename} in {country}/{sound}: {e}")

def process_country(country: str, base_dir: str) -> None:
  """
  Process all sound folders for a given country.

  Args:
      country: Country name.
      base_dir: Base directory from environment variable.
  """
  agile_dir: Path = Path(base_dir) / f"marrs_acoustics/data/output_dir_{country}/agile_outputs"
  if not agile_dir.exists():
    logging.info(f"Agile outputs directory not found for {country}: {agile_dir}. Skipping.")
    return

  for entry in os.listdir(agile_dir):
    folder_path = agile_dir / entry
    if folder_path.is_dir():
      csv_file: Path = folder_path / f"{entry}_inference.csv"
      if csv_file.exists():
        process_sound_folder(country, entry, base_dir)
      else:
        logging.info(f"Inference CSV not found for sound {entry} in {country}.")

def main() -> None:
  """Main function to process all countries and their sound folders."""
  logging.basicConfig(level=logging.INFO, format="%(message)s")
  BASE_DIR: Optional[str] = os.getenv("BASE_DIR")
  if not BASE_DIR:
    raise ValueError("BASE_DIR environment variable is not set.")

  for country in COUNTRIES:
    logging.info(f"Processing country: {country}")
    process_country(country, BASE_DIR)

if __name__ == "__main__":
  main()
