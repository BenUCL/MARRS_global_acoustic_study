import os

ROOT_DIR = "/media/bwilliams/New Volume/mars_global_acoustic_study/detections"

def count_files(path: str) -> int:
  """Recursively counts all files in the directory and its subdirectories."""
  return sum(len(files) for _, _, files in os.walk(path))

if __name__ == "__main__":
  total = count_files(ROOT_DIR)
  print(f"Total files: {total}")
