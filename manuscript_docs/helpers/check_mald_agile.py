"""
After removing above water periods from Mald, need to exclude any detections
that may have fallen in these periods. This script uses raw_file_list.csv which contains
the full list of 1 min files that are below water. It then reproduces the inference.csv
for each sound with entries from files not in raw_file_list.csv excluded.
"""

import os
import csv
import logging
from typing import Set

# Global constants
OUTPUT_DIR = "/home/bwilliams/ucl_projects/marrs_acoustics/data/output_dir_maldives/agile_outputs"
RAW_FILE_LIST_PATH = "/home/bwilliams/ucl_projects/marrs_acoustics/data/output_dir_maldives/raw_file_list.csv"
PREFIX = "raw_audio/"

def load_raw_file_set(file_path: str) -> Set[str]:
  """Load raw file list and return a set of filenames."""
  raw_files: Set[str] = set()
  with open(file_path, "r", newline="") as f:
    reader = csv.DictReader(f)
    for row in reader:
      filename = row["filename"].strip()
      raw_files.add(filename)
  return raw_files

def process_folder(folder: str, raw_file_set: Set[str]) -> None:
  """Process a folder: filter the _inference_old.csv using the raw file list."""
  folder_path = os.path.join(OUTPUT_DIR, folder)
  old_csv_path = os.path.join(folder_path, f"{folder}_inference_old.csv")
  if not os.path.exists(old_csv_path):
    logging.info(f"Skipping {folder}: {old_csv_path} not found.")
    return

  with open(old_csv_path, "r", newline="") as f:
    reader = csv.DictReader(f)
    rows = list(reader)

  n_old = len(rows)
  filtered_rows = []
  for row in rows:
    original_filename = row.get("filename", "").strip()
    # Remove the 'raw_audio/' prefix if present
    filename = original_filename[len(PREFIX):] if original_filename.startswith(PREFIX) else original_filename
    if filename in raw_file_set:
      filtered_rows.append(row)

  n_new = len(filtered_rows)
  new_csv_path = os.path.join(folder_path, f"{folder}_inference.csv")
  with open(new_csv_path, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=reader.fieldnames)
    writer.writeheader()
    writer.writerows(filtered_rows)

  excluded_count = n_old - n_new
  logging.info(f"{folder}: old entries: {n_old}, excluded: {excluded_count}")

def main() -> None:
  """Main function to process all subfolders in the agile outputs directory."""
  logging.basicConfig(level=logging.INFO, format="%(message)s")
  raw_file_set = load_raw_file_set(RAW_FILE_LIST_PATH)
  for entry in os.listdir(OUTPUT_DIR):
    folder_path = os.path.join(OUTPUT_DIR, entry)
    if os.path.isdir(folder_path):
      process_folder(entry, raw_file_set)

if __name__ == "__main__":
  main()
