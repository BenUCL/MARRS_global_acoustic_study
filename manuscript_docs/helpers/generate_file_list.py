import os
import pandas as pd

# Define paths
AUDIO_DIR = "/media/bwilliams/New Volume/mars_global_acoustic_study/mexico_acoustics/raw_audio"
OUTPUT_CSV = "/media/bwilliams/New Volume/mars_global_acoustic_study/mexico_acoustics/raw_file_list.csv"

def list_audio_files(directory: str) -> list[str]:
  """Returns a list of all .wav and .WAV files in the given directory."""
  return [f for f in os.listdir(directory) if f.lower().endswith(".wav")]

def save_to_csv(file_list: list[str], output_path: str) -> None:
  """Saves the list of filenames to a CSV file."""
  df = pd.DataFrame(file_list, columns=["filename"])
  df.to_csv(output_path, index=False)

def main():
  file_list = list_audio_files(AUDIO_DIR)
  save_to_csv(file_list, OUTPUT_CSV)

if __name__ == "__main__":
  main()
