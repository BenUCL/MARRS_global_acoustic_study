#!/usr/bin/env python3
"""
Use this to write out detections for the final false positive check.
This script processes inference CSVs for multiple countries and sounds.
For each country, it finds each sound folder that contains a CSV (e.g. croak_inference.csv),
filters rows with logit >= 1.0, randomly selects 100 eligible rows (seeded),
and writes out 5-second audio clips. The new filename starts with the logit score,
then the timestamp in seconds (with "s"), and finally the original filename.
"""

import os
import logging
from pathlib import Path
from typing import Optional
import pandas as pd
import librosa
import soundfile as sf
from tqdm import tqdm

# Global constants
SELECT_MODE = "random"  # 'random' or 'ordered'. Change to "ordered" to select by logit ascending from LOGIT_CUTOFF
COUNTRIES = ["maldives", "kenya", "mexico", "indonesia", "australia"] # List of countries to process
LOGIT_CUTOFF = 1.0 # The minimum logit score to write out
NUM_FILES = 100 # Number of files to write out per sound folder
SEED = 42

def process_sound_folder(country: str, sound: str, base_dir: str) -> None:
  """
  Process a single sound folder for a given country.

  Args:
      country: Country name.
      sound: Sound folder name.
      base_dir: Base directory from environment variable.
  """
  csv_path = os.path.join(
      base_dir,
      f"marrs_acoustics/data/output_dir_{country}/agile_outputs/{sound}/{sound}_inference.csv"
  )
  if not os.path.exists(csv_path):
    logging.info(f"CSV not found: {csv_path}. Skipping {sound} in {country}.")
    return

  df = pd.read_csv(csv_path)
  df.columns = df.columns.str.strip()
  df_filtered = df[df["logit"] >= LOGIT_CUTOFF]
  if df_filtered.empty:
    logging.info(f"No rows with logit >= {LOGIT_CUTOFF} in {csv_path}.")
    return

  # Randomly sample NUM_FILES rows (or all if less than NUM_FILES)
  if SELECT_MODE == "random":
    df_sample = df_filtered.sample(n=NUM_FILES, random_state=SEED) if len(df_filtered) > NUM_FILES else df_filtered
  elif SELECT_MODE == "ordered":
    df_sample = df_filtered.sort_values("logit").head(NUM_FILES)
  else:
    logging.error(f"Unknown selection mode: {SELECT_MODE}. Defaulting to random.")
    df_sample = df_filtered.sample(n=NUM_FILES, random_state=SEED) if len(df_filtered) > NUM_FILES else df_filtered

  audio_base_path = Path(f"/media/bwilliams/New Volume/mars_global_acoustic_study/{country}_acoustics")
  save_dir = Path(os.path.join(base_dir, f"marrs_acoustics/data/pred_audio_samples/{country}/{sound}"))
  save_dir.mkdir(parents=True, exist_ok=True)

  for _, row in tqdm(df_sample.iterrows(), total=len(df_sample), desc=f"Processing {country}/{sound}"):
    try:
      filename = row["filename"].strip()
      timestamp_s = float(row["timestamp_s"])
      logit = float(row["logit"])
      full_audio_path = audio_base_path / filename

      # Load a 5-second clip starting at timestamp_s
      y, sr = librosa.load(full_audio_path, sr=None, offset=timestamp_s, duration=5.0)

      timestamp_str = f"{int(timestamp_s):02d}s"
      new_filename = f"{logit:.2f}_{timestamp_str}_{os.path.basename(filename)}"
      save_path = save_dir / new_filename
      sf.write(save_path, y, sr)
    except Exception as e:
      logging.error(f"Error processing {filename} in {country}/{sound}: {e}")

def process_country(country: str, base_dir: str) -> None:
  """
  Process all sound folders for a given country.

  Args:
      country: Country name.
      base_dir: Base directory from environment variable.
  """
  agile_dir = os.path.join(base_dir, f"marrs_acoustics/data/output_dir_{country}/agile_outputs")
  if not os.path.exists(agile_dir):
    logging.info(f"Agile outputs directory not found for {country}: {agile_dir}. Skipping.")
    return

  for entry in os.listdir(agile_dir):
    folder_path = os.path.join(agile_dir, entry)
    if os.path.isdir(folder_path):
      csv_file = os.path.join(folder_path, f"{entry}_inference.csv")
      if os.path.exists(csv_file):
        process_sound_folder(country, entry, base_dir)
      else:
        logging.info(f"Inference CSV not found for sound {entry} in {country}.")

def main() -> None:
  """Main function to process all countries and their sound folders."""
  logging.basicConfig(level=logging.INFO, format="%(message)s")
  BASE_DIR = os.getenv("BASE_DIR")
  if not BASE_DIR:
    raise ValueError("BASE_DIR environment variable is not set.")

  for country in COUNTRIES:
    logging.info(f"Processing country: {country}")
    process_country(country, BASE_DIR)

if __name__ == "__main__":
  main()
