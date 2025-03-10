import logging
import librosa
import soundfile as sf
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path
from typing import List
from tqdm import tqdm  # pip install tqdm

# Global variables
SOURCE_DIR: Path = Path("/media/bwilliams/New Volume/mars_global_acoustic_study/maldives_acoustics/raw_audio")
TARGET_SAMPLE_RATE: int = 16000
NUM_WORKERS: int = 8  # Adjust as needed

logging.basicConfig(level=logging.ERROR, format="%(asctime)s - %(levelname)s - %(message)s")

def process_file(file_path: Path) -> None:
  """Downsamples a WAV file to 16kHz using librosa and soundfile, replacing the original file.

  Args:
    file_path: Path to the input WAV file.
  """
  try:
    # Load file and resample to TARGET_SAMPLE_RATE
    audio, _ = librosa.load(file_path, sr=TARGET_SAMPLE_RATE)
    tmp_path: Path = file_path.with_suffix(file_path.suffix + ".tmp")
    # Write with explicit format to avoid extension issues
    sf.write(tmp_path, audio, TARGET_SAMPLE_RATE, format="WAV")
    tmp_path.replace(file_path)
  except Exception as err:
    logging.error(f"Error processing {file_path.name}: {err}")

def main() -> None:
  """Finds all WAV files in the source folder and downsamples them concurrently with progress tracking."""
  wav_files: List[Path] = list(SOURCE_DIR.glob("*.WAV"))
  if not wav_files:
    logging.error("No WAV files found in the folder.")
    return

  with ProcessPoolExecutor(max_workers=NUM_WORKERS) as executor:
    futures = {executor.submit(process_file, wav): wav for wav in wav_files}
    with tqdm(total=len(wav_files), desc="Processing files", unit="file") as pbar:
      for future in as_completed(futures):
        try:
          future.result()
        except Exception as exc:
          logging.error(f"Error processing {futures[future].name}: {exc}")
        pbar.update(1)

if __name__ == "__main__":
  main()
