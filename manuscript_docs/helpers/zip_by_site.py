#!/usr/bin/env python3
"""
Zip WAVs by country and site code using 7-Zip (multithreaded), then write SHA-256 checksums
and a CSV manifest. Progress logs show which zip is in progress and how long it took.

Usage:
  python zip_by_site.py --root "E:\\mars_global_acoustic_study" --out "E:\\mars_global_acoustic_study_zips" \
    [--compression store|fast|normal|max] [--sevenzip "C:\\Program Files\\7-Zip\\7z.exe"] \
    [--overwrite] [--dry-run] [--log-level INFO]

I ran with this for some compression:
python zip_by_site.py --root "E:\mars_global_acoustic_study" --out "E:\mars_global_acoustic_study_zips" `
  --compression fast --sevenzip "C:\Program Files\7-Zip\7z.exe" --log-level INFO


Design:
- Group by (country_code, site_code) based on filename convention:
  <ccc>_<Sx>_YYYYMMDD_HHMMSS[...].wav  e.g., ind_R1_20230313_222200.wav
  where <ccc> is aus|ind|ken|mal|mex and <Sx> is one letter + one digit (e.g., R1).
- Non-audio files in raw_audio (e.g., config.txt) are placed into that country's H1 zip.
- Output one zip per (country, site): {out}\\{ccc}_{Sx}.zip, plus a .sha256 sidecar and manifest.csv.

Notes:
- Uses external 7-Zip for speed (multithreaded). Requires 7z.exe on PATH or specified via --sevenzip.
- Stores only base filenames in each zip (no directories).
- Assumes all files for a given (country, site) live in the same raw_audio directory.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import logging
import re
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

# -------------------------
# CONSTANTS / GLOBAL CONFIG
# -------------------------

COUNTRY_DIR_TO_CODE = {
 "australia_acoustics": "aus",
 "indonesia_acoustics": "ind",
 "kenya_acoustics": "ken",
 "maldives_acoustics": "mal",
 "mexico_acoustics": "mex",
}

RAW_SUBDIR = "raw_audio"
MANIFEST_NAME = "manifest.csv"

# Filename pattern: <ccc>_<Sx>_YYYYMMDD_HHMMSS...
NAME_RE = re.compile(
 r"^(?P<ccc>[a-z]{3})_(?P<site>[A-Za-z]\d)_(?P<date>\d{8})_(?P<time>\d{6})",
 re.IGNORECASE,
)

# -------------------------
# DATA STRUCTURES
# -------------------------

@dataclass
class FileEntry:
 """Represents one file to include in a zip.

 Attributes:
  country: 3-letter country code (e.g., "ind").
  site: Site code (e.g., "R1").
  path: Full path to the source file.
  size: File size in bytes.
  arcname: Name to store inside the archive (no directories).
 """
 country: str
 site: str
 path: Path
 size: int
 arcname: str

@dataclass
class ZipResult:
 """Summary of a created zip for manifest/logging.

 Attributes:
  zip_path: Path to resulting zip (or planned path in dry-run).
  country: 3-letter code.
  site: Site code.
  file_count: Number of files included.
  total_bytes: Sum of original file sizes in bytes.
  sha256: SHA-256 of the zip (None in dry-run or if skipped).
 """
 zip_path: Path
 country: str
 site: str
 file_count: int
 total_bytes: int
 sha256: Optional[str]

# -------------------------
# HELPERS
# -------------------------

def is_wav(path: Path) -> bool:
 """Return True if the file extension looks like WAV (case-insensitive)."""
 return path.suffix.lower() == ".wav"


def parse_name(path: Path) -> Optional[Tuple[str, str]]:
 """Parse filename to (country_code, site_code) if it matches convention.

 Args:
  path: File path.

 Returns:
  (country, site) or None if not matched.
 """
 m = NAME_RE.match(path.name)
 if not m:
  return None
 country = m.group("ccc").lower()
 site = m.group("site").upper()
 return country, site


def infer_country_from_dir(raw_dir: Path) -> Optional[str]:
 """Infer <ccc> from the parent acoustics folder name via COUNTRY_DIR_TO_CODE."""
 for dirname, ccc in COUNTRY_DIR_TO_CODE.items():
  if raw_dir.parent.name.lower() == dirname.lower():
   return ccc
 return None


def human_bytes(n: int) -> str:
 """Render bytes as a human-friendly string (e.g., '12.3 GB')."""
 units = ["B", "KB", "MB", "GB", "TB"]
 size = float(n)
 for u in units:
  if size < 1024 or u == units[-1]:
   return f"{size:.2f} {u}"
  size /= 1024


def human_duration(seconds: float) -> str:
 """Render seconds as H:MM:SS."""
 seconds = int(round(seconds))
 h = seconds // 3600
 m = (seconds % 3600) // 60
 s = seconds % 60
 if h:
  return f"{h}:{m:02d}:{s:02d}"
 return f"{m}:{s:02d}"


def find_raw_dirs(root: Path) -> List[Path]:
 """Find all country raw_audio directories under root."""
 dirs: List[Path] = []
 for child in root.iterdir():
  if not child.is_dir():
   continue
  if child.name.lower() in COUNTRY_DIR_TO_CODE:
   raw_dir = child / RAW_SUBDIR
   if raw_dir.is_dir():
    dirs.append(raw_dir)
   else:
    logging.warning("Missing raw_audio: %s", raw_dir)
 return dirs


def collect_entries(raw_dir: Path, non_audio_to_h1: bool = True) -> List[FileEntry]:
 """Collect FileEntry objects for one country's raw_audio dir.

 Rules:
 - WAVs are grouped by (ccc, site) from filename.
 - Non-audio files go to that country's H1 zip (if requested).
 - If a file's prefix ccc mismatches folder country, trust the filename prefix.

 Returns:
  List of FileEntry.
 """
 entries: List[FileEntry] = []
 dir_country = infer_country_from_dir(raw_dir)

 for p in raw_dir.iterdir():
  if not p.is_file():
   continue

  if is_wav(p):
   parsed = parse_name(p)
   if not parsed:
    logging.warning("Skipping WAV with unexpected name: %s", p.name)
    continue
   ccc, site = parsed
   entries.append(FileEntry(country=ccc, site=site, path=p, size=p.stat().st_size, arcname=p.name))
  else:
   if not non_audio_to_h1:
    logging.info("Skipping non-audio: %s", p.name)
    continue
   ccc = dir_country or "unk"
   entries.append(FileEntry(country=ccc, site="H1", path=p, size=p.stat().st_size, arcname=p.name))

 return entries


def group_by_site(entries: Iterable[FileEntry]) -> Dict[Tuple[str, str], List[FileEntry]]:
 """Group entries by (country, site)."""
 groups: Dict[Tuple[str, str], List[FileEntry]] = {}
 for e in entries:
  key = (e.country, e.site)
  groups.setdefault(key, []).append(e)
 return groups


def sha256_file(path: Path, chunk_size: int = 1024 * 1024) -> str:
 """Compute SHA-256 hash of a file efficiently."""
 h = hashlib.sha256()
 with path.open("rb") as f:
  for chunk in iter(lambda: f.read(chunk_size), b""):
   h.update(chunk)
 return h.hexdigest()


def write_sha256_sidecar(zip_path: Path, digest: str) -> Path:
 """Write a .sha256 sidecar file next to the zip."""
 sidecar = zip_path.with_suffix(zip_path.suffix + ".sha256")
 sidecar.write_text(f"{digest}  {zip_path.name}\n", encoding="utf-8")
 return sidecar


def resolve_7z_path(user_path: Optional[Path]) -> Optional[Path]:
 """Resolve path to 7z.exe. Try user-provided path, then common install locations, then PATH."""
 if user_path:
  p = user_path
  if p.exists():
   return p
  logging.error("Provided --sevenzip not found: %s", p)
  return None

 # Try common locations
 candidates = [
  Path(r"C:\Program Files\7-Zip\7z.exe"),
  Path(r"C:\Program Files (x86)\7-Zip\7z.exe"),
 ]
 for c in candidates:
  if c.exists():
   return c

 # Try PATH
 exe = shutil.which("7z")
 if exe:
  return Path(exe)

 return None


def compression_to_7z_args(mode: str) -> List[str]:
 """Map compression mode to 7z -mx and method flags.

 Modes:
  - store  -> -mx=0 (no compression; fastest)
  - fast   -> -mx=1 -mm=Deflate
  - normal -> -mx=5 -mm=Deflate
  - max    -> -mx=9 -mm=Deflate
 """
 mode = mode.lower()
 if mode == "store":
  return ["-mx=0"]
 if mode == "fast":
  return ["-mx=1", "-mm=Deflate"]
 if mode == "max":
  return ["-mx=9", "-mm=Deflate"]
 # default normal
 return ["-mx=5", "-mm=Deflate"]


def run_7z_create_zip(
 sevenzip: Path,
 out_zip: Path,
 files: List[FileEntry],
 overwrite: bool,
 compression_mode: str,
) -> int:
 """Create a zip with 7z, storing only base filenames (no directories).

 Uses a temporary @listfile to avoid Windows command-line length limits.
 Runs 7z with cwd set to the source folder so only basenames are stored.
 """
 if not files:
  return 0

 # Ensure output directory exists
 out_zip.parent.mkdir(parents=True, exist_ok=True)

 # Overwrite policy
 if out_zip.exists():
  if overwrite:
   try:
    out_zip.unlink()
   except Exception as exc:
    logging.error("Failed to remove existing zip: %s (%s)", out_zip, exc)
    return 1
  else:
   logging.info("Exists, skipping (use --overwrite): %s", out_zip)
   return 0

 # Ensure a single source directory for clean arcnames
 try:
  parent_dirs = {f.path.parent.resolve() for f in files}
 except Exception as exc:
  logging.error("Failed resolving parent dirs: %s", exc)
  return 2

 if len(parent_dirs) != 1:
  logging.error("Files span multiple folders; cannot guarantee flat arcnames for: %s", out_zip.name)
  logging.error("Parents: %s", ", ".join(str(p) for p in sorted(parent_dirs)))
  return 2

 src_dir = next(iter(parent_dirs))
 if not src_dir.exists():
  logging.error("Source directory does not exist: %s", src_dir)
  return 2

 # Build compression args
 comp_args = compression_to_7z_args(compression_mode)

 # Create a listfile with basenames to avoid long command lines
 try:
  with tempfile.NamedTemporaryFile("w", delete=False, dir=str(src_dir), suffix=".list", encoding="utf-8") as lf:
   list_path = Path(lf.name)
   # One filename per line (basenames only so no directories are stored)
   for fe in files:
    lf.write(f"{fe.arcname}\n")
 except Exception as exc:
  logging.error("Failed creating listfile in %s: %s", src_dir, exc)
  return 2

 # 7z a -tzip [comp] -mmt=on "out.zip" @listfile
 cmd = [str(sevenzip), "a", "-tzip", "-mmt=on", *comp_args, str(out_zip), f"@{list_path.name}"]

 try:
  proc = subprocess.run(
   cmd,
   cwd=str(src_dir),          # resolve basenames
   stdout=subprocess.PIPE,
   stderr=subprocess.STDOUT,
   text=True,
   check=False,
  )
 except FileNotFoundError as exc:
  # Could be either the exe or the cwd; be explicit
  if not sevenzip.exists():
   logging.error("7z.exe not found at: %s", sevenzip)
  elif not src_dir.exists():
   logging.error("Working directory missing: %s", src_dir)
  else:
   logging.error("Failed to start 7z (unexpected FileNotFoundError): %s", exc)
  try:
   list_path.unlink(missing_ok=True)  # cleanup
  except Exception:
   pass
  return 3
 except OSError as exc:
  # Includes long path/cmdline errors etc.
  logging.error("OS error running 7z for %s: %s", out_zip.name, exc)
  try:
   list_path.unlink(missing_ok=True)
  except Exception:
   pass
  return 3

 # Cleanup listfile regardless of success
 try:
  list_path.unlink(missing_ok=True)
 except Exception:
  pass

 if proc.returncode != 0:
  logging.error("7z failed (%d) for %s\n%s", proc.returncode, out_zip.name, proc.stdout)
  return proc.returncode

 logging.debug("7z output for %s:\n%s", out_zip.name, proc.stdout)
 return 0


def write_manifest(out_dir: Path, results: List[ZipResult]) -> Path:
 """Write manifest.csv in out_dir with summary of all zips."""
 manifest_path = out_dir / MANIFEST_NAME
 headers = ["zip_name", "country", "site_code", "file_count", "total_bytes", "sha256"]
 with manifest_path.open("w", newline="", encoding="utf-8") as f:
  writer = csv.writer(f)
  writer.writerow(headers)
  for r in sorted(results, key=lambda x: (x.country, x.site)):
   writer.writerow([r.zip_path.name, r.country, r.site, r.file_count, r.total_bytes, r.sha256 or ""])
 return manifest_path


def run(root: Path, out: Path, overwrite: bool, dry_run: bool, sevenzip_path: Optional[Path],
        compression: str) -> int:
 """Main workflow with progress logs, using 7-Zip for speed."""
 if not root.exists():
  logging.error("Root does not exist: %s", root)
  return 2

 sevenzip = resolve_7z_path(sevenzip_path)
 if not sevenzip:
  logging.error("Could not find 7z.exe. Install 7-Zip or pass --sevenzip PATH.")
  return 4
 logging.info("Using 7-Zip: %s", sevenzip)

 raw_dirs = find_raw_dirs(root)
 if not raw_dirs:
  logging.error("No raw_audio dirs found under: %s", root)
  return 3

 all_entries: List[FileEntry] = []
 for rd in raw_dirs:
  logging.info("Scanning: %s", rd)
  entries = collect_entries(rd, non_audio_to_h1=True)
  all_entries.extend(entries)

 groups = group_by_site(all_entries)
 if not groups:
  logging.warning("No files matched.")
  return 0

 results: List[ZipResult] = []
 keys_sorted = sorted(groups.keys(), key=lambda k: (k[0], k[1]))
 total_groups = len(keys_sorted)

 logging.info("Preparing %d zips...", total_groups)

 t_all = time.perf_counter()
 for i, key in enumerate(keys_sorted, start=1):
  country, site = key
  files = groups[key]
  total_bytes = sum(f.size for f in files)
  approx_gb = total_bytes / (1024**3)
  out_zip = out / f"{country}_{site}.zip"

  logging.info("(%d/%d) Starting %s_%s.zip — %d files, ~%.2f GB",
               i, total_groups, country, site, len(files), approx_gb)

  if dry_run:
   logging.info("[DRY-RUN] Would create: %s  (%d files, ~%s)",
                out_zip, len(files), human_bytes(total_bytes))
   res = ZipResult(zip_path=out_zip, country=country, site=site,
                   file_count=len(files), total_bytes=total_bytes, sha256=None)
   results.append(res)
   continue

  t0 = time.perf_counter()
  rc = run_7z_create_zip(sevenzip, out_zip, files, overwrite, compression)
  t_zip = time.perf_counter() - t0
  if rc != 0:
   logging.error("Failed creating %s (rc=%d). Skipping checksum.", out_zip.name, rc)
   continue

  # Compute checksum and write sidecar
  t1 = time.perf_counter()
  digest = sha256_file(out_zip)
  write_sha256_sidecar(out_zip, digest)
  t_hash = time.perf_counter() - t1

  logging.info("Finished %s (zipped in %s, hashed in %s) — %d files, ~%s",
               out_zip.name, human_duration(t_zip), human_duration(t_hash),
               len(files), human_bytes(total_bytes))

  res = ZipResult(zip_path=out_zip, country=country, site=site,
                  file_count=len(files), total_bytes=total_bytes, sha256=digest)
  results.append(res)

 # Manifest (only after non-dry run)
 if not dry_run:
  manifest = write_manifest(out, results)
  logging.info("Manifest written: %s", manifest)

 total_files = sum(r.file_count for r in results)
 total_bytes = sum(r.total_bytes for r in results)
 logging.info("All done in %s. Zips: %d | Files: %d | ~%.2f GB",
              human_duration(time.perf_counter() - t_all), len(results), total_files, total_bytes / (1024**3))
 return 0


def parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
 """Parse CLI arguments."""
 parser = argparse.ArgumentParser(description="Zip WAVs by country/site via 7-Zip, with checksums and manifest.")
 parser.add_argument("--root", required=True, type=Path, help="Root folder (e.g., E:\\mars_global_acoustic_study)")
 parser.add_argument("--out", required=True, type=Path, help="Output folder for zips and manifest (e.g., E:\\mars_global_acoustic_study_zips)")
 parser.add_argument("--overwrite", action="store_true", help="Overwrite existing zips")
 parser.add_argument("--dry-run", action="store_true", help="Show actions without writing files")
 parser.add_argument("--log-level", default="INFO", choices=["DEBUG", "INFO", "WARNING", "ERROR"], help="Logging verbosity")
 parser.add_argument("--sevenzip", type=Path, default=None, help="Path to 7z.exe (if not on PATH)")
 parser.add_argument("--compression", default="store", choices=["store", "fast", "normal", "max"],
                     help="Compression mode: 'store' fastest (no compression), 'fast', 'normal', 'max'.")
 return parser.parse_args(argv)


def configure_logging(level: str) -> None:
 """Configure root logger."""
 logging.basicConfig(
  level=getattr(logging, level, logging.INFO),
  format="%(asctime)s - %(levelname)s - %(message)s",
 )


if __name__ == "__main__":
 args = parse_args()
 configure_logging(args.log_level)
 sys.exit(run(args.root, args.out, overwrite=args.overwrite, dry_run=args.dry_run,
              sevenzip_path=args.sevenzip, compression=args.compression))
