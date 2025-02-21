#!/usr/bin/env python3
"""
Script to add an age column to several CSV files using a mapping from sites_age.csv.

The script reads a mapping CSV file containing country, site, and age values.
For each target CSV in the results folder, it adds an "age" column (using country and site as keys)
and writes the updated CSV as a new file with "_age" appended to the original filename.
If no match is found, a warning is logged.
"""

import csv
import logging
import os
from pathlib import Path
from typing import Dict, Tuple

#@title Set Paths
base_dir = os.getenv('BASE_DIR')
if not base_dir:
    raise ValueError("BASE_DIR environment variable is not set.")

# Global file paths
MAPPING_CSV = os.path.join(base_dir, "marrs_acoustics/data/results/functions/sites_age.csv")
RESULTS_DIR = os.path.join(base_dir, "marrs_acoustics/data/results/functions")
FILES = [
    "settlement_cuescape.csv",
    "graze_count.csv",
    "phonic_richness.csv",
    "snaps_count.csv"
]

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")


def load_age_mapping(mapping_path: Path) -> Dict[Tuple[str, str], str]:
    """Load the age mapping from a CSV file.

    Args:
        mapping_path: Path to the sites_age CSV file.

    Returns:
        A dictionary mapping (country, site) to age.
    """
    mapping: Dict[Tuple[str, str], str] = {}
    with mapping_path.open("r", newline="") as file:
        reader = csv.DictReader(file)
        for row in reader:
            key = (row["country"].strip().lower(), row["site"].strip().lower())
            mapping[key] = row["age"].strip()
    return mapping


def add_age_column_to_file(file_path: Path, mapping: Dict[Tuple[str, str], str]) -> None:
  """Add the age column to the CSV file based on site name.

  If the site starts with 'R' or 'N', use mapping to add the age; otherwise,
  set age as "NA". Logs a warning if a mapping for an eligible site is not found.

  Args:
      file_path: Path to the original CSV file.
      mapping: Dictionary mapping (country, site) to age.
  """
  new_file_path = file_path.with_name(file_path.stem + "_age.csv")
  with file_path.open("r", newline="") as infile, new_file_path.open("w", newline="") as outfile:
    reader = csv.DictReader(infile)
    fieldnames = reader.fieldnames + ["age"] if reader.fieldnames else ["age"]
    writer = csv.DictWriter(outfile, fieldnames=fieldnames)
    writer.writeheader()
    for row in reader:
      site = row["site"].strip()
      if site.startswith("R") or site.startswith("N"):
        key = (row["country"].strip().lower(), site.lower())
        if key in mapping:
          row["age"] = mapping[key]
        else:
          row["age"] = "NA"
          logging.warning("No age mapping found for country: %s, site: %s", row["country"], row["site"])
      else:
        row["age"] = "NA"
        logging.info("Site %s does not start with 'R' or 'N'. Setting age as NA.", row["site"])
      writer.writerow(row)
  logging.info("Processed file: %s", file_path.name)



def main() -> None:
    """Main function to add age column to each CSV file."""
    age_mapping = load_age_mapping(MAPPING_CSV)
    for filename in FILES:
        file_path = RESULTS_DIR / filename
        if file_path.exists():
            add_age_column_to_file(file_path, age_mapping)
        else:
            logging.error("File not found: %s", file_path)


if __name__ == "__main__":
    main()
