#!/usr/bin/env python3
"""
Generate SHA-256 sidecars and a manifest for detections zips.

- Input root: E:\mars_global_acoustic_study\detections
- Expects:   detections\{country}\detections_{country}.zip
- Outputs:   detections\{country}\detections_{country}.zip.sha256
             detections\manifest_detections.csv
"""

from __future__ import annotations
import argparse
import csv
import hashlib
import logging
from pathlib import Path
from typing import Iterator, List, Tuple
import zipfile
import time

MANIFEST_NAME = "manifest_detections.csv"

def sha256_file(path: Path, chunk_size: int = 1024 * 1024) -> str:
  """Return SHA-256 hex digest for a file."""
  h = hashlib.sha256()
  with path.open("rb") as f:
    for chunk in iter(lambda: f.read(chunk_size), b""):
      h.update(chunk)
  return h.hexdigest()

def list_detection_zips(root: Path) -> Iterator[Path]:
  """Yield paths like root/country/detections_country.zip"""
  for country_dir in sorted(p for p in root.iterdir() if p.is_dir()):
    for zip_path in country_dir.glob("detections_*.zip"):
      yield zip_path

def count_zip_members(path: Path) -> Tuple[int, int]:
  """Return (file_count, total_uncompressed_bytes)."""
  with zipfile.ZipFile(path, "r") as zf:
    infos = zf.infolist()
    files = [i for i in infos if not i.is_dir()]
    total = sum(i.file_size for i in files)
    return len(files), total

def human_bytes(n: int) -> str:
  """Human-readable bytes."""
  units = ["B", "KB", "MB", "GB", "TB"]
  size = float(n)
  for u in units:
    if size < 1024 or u == units[-1]:
      return f"{size:.2f} {u}"
    size /= 1024

def write_sidecar(zip_path: Path, digest: str) -> Path:
  """Write .sha256 next to the zip."""
  sidecar = zip_path.with_suffix(zip_path.suffix + ".sha256")
  sidecar.write_text(f"{digest}  {zip_path.name}\n", encoding="utf-8")
  return sidecar

def run(root: Path) -> int:
  """Main workflow."""
  if not root.exists():
    logging.error("Root does not exist: %s", root)
    return 2

  rows: List[List[str]] = []
  start = time.perf_counter()

  for zip_path in list_detection_zips(root):
    t0 = time.perf_counter()
    logging.info("Processing %s", zip_path)

    digest = sha256_file(zip_path)
    write_sidecar(zip_path, digest)

    file_count, total_uncompressed = count_zip_members(zip_path)

    logging.info("  %s | files: %d | size (unzipped): %s | sha256: %s",
                 zip_path.name, file_count, human_bytes(total_uncompressed), digest[:12] + "...")

    rows.append([
      zip_path.name,
      zip_path.parent.name,  # country folder
      str(zip_path.relative_to(root)),
      str(file_count),
      str(total_uncompressed),
      digest,
    ])

  manifest = root / MANIFEST_NAME
  with manifest.open("w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["zip_name", "country", "relative_path", "file_count", "total_uncompressed_bytes", "sha256"])
    w.writerows(rows)

  logging.info("Wrote manifest: %s", manifest)
  logging.info("Done in %.1fs", time.perf_counter() - start)
  return 0

def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description="Create SHA-256 sidecars and a manifest for detections zips.")
  parser.add_argument("--root", required=True, type=Path, help="Root detections folder (e.g., E:\\mars_global_acoustic_study\\detections)")
  parser.add_argument("--log-level", default="INFO", choices=["DEBUG","INFO","WARNING","ERROR"])
  return parser.parse_args()

if __name__ == "__main__":
  args = parse_args()
  logging.basicConfig(level=getattr(logging, args.log_level), format="%(levelname)s - %(message)s")
  raise SystemExit(run(args.root))
