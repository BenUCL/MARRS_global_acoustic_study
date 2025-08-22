#!/usr/bin/env python3
"""
Zip detections by country, preserving subfolder structure.

- Input: E:\mars_global_acoustic_study\detections\{country}\{sound_type}\*.wav
- Output: detections_{country}.zip written inside that {country} folder.
"""

import argparse
import logging
import sys
import time
from pathlib import Path
import subprocess
import shutil

# -------------------------
# HELPERS
# -------------------------

def resolve_7z_path(user_path: Path | None) -> Path | None:
 """Resolve path to 7z.exe."""
 if user_path and user_path.exists():
  return user_path
 exe = shutil.which("7z")
 if exe:
  return Path(exe)
 common = [
  Path(r"C:\Program Files\7-Zip\7z.exe"),
  Path(r"C:\Program Files (x86)\7-Zip\7z.exe"),
 ]
 for c in common:
  if c.exists():
   return c
 return None

def run_7z(sevenzip: Path, out_zip: Path, src_dir: Path, overwrite: bool) -> int:
 """Run 7z to zip a folder recursively, preserving structure."""
 if out_zip.exists() and not overwrite:
  logging.info("Skipping %s (already exists)", out_zip)
  return 0
 if out_zip.exists():
  out_zip.unlink()

 cmd = [
  str(sevenzip), "a", "-tzip", "-mmt=on", str(out_zip), "."
 ]
 t0 = time.perf_counter()
 proc = subprocess.run(cmd, cwd=str(src_dir), capture_output=True, text=True)
 dt = time.perf_counter() - t0
 if proc.returncode != 0:
  logging.error("7z failed for %s: %s", src_dir, proc.stdout)
  return proc.returncode
 logging.info("Created %s in %.1fs", out_zip, dt)
 return 0

# -------------------------
# MAIN
# -------------------------

def main(root: Path, sevenzip: Path, overwrite: bool) -> int:
 if not root.exists():
  logging.error("Root not found: %s", root)
  return 2

 for country_dir in root.iterdir():
  if not country_dir.is_dir():
   continue
  out_zip = country_dir / f"detections_{country_dir.name}.zip"
  logging.info("Zipping %s -> %s", country_dir.name, out_zip.name)
  rc = run_7z(sevenzip, out_zip, country_dir, overwrite)
  if rc != 0:
   return rc
 return 0

def parse_args(argv=None):
 p = argparse.ArgumentParser(description="Zip detections by country, preserving subfolders.")
 p.add_argument("--root", required=True, type=Path, help="Root detections folder")
 p.add_argument("--sevenzip", type=Path, help="Path to 7z.exe (if not on PATH)")
 p.add_argument("--overwrite", action="store_true", help="Overwrite existing zips")
 p.add_argument("--log-level", default="INFO", choices=["DEBUG","INFO","WARNING","ERROR"])
 return p.parse_args(argv)

if __name__ == "__main__":
 args = parse_args()
 logging.basicConfig(level=getattr(logging, args.log_level))
 sevenzip = resolve_7z_path(args.sevenzip)
 if not sevenzip:
  logging.error("7z.exe not found. Install 7-Zip or pass --sevenzip PATH")
  sys.exit(1)
 sys.exit(main(args.root, sevenzip, args.overwrite))
