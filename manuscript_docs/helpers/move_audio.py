"""Move audio from sub dirs to raw_audio dir"""

import os
import shutil
from typing import List

BASE_PATH = r'E:\mexico_acoustics'
RAW_AUDIO_DIR = os.path.join(BASE_PATH, 'raw_audio')

def get_audio_files_in_subdirectories(base_path: str, exclude_dir: str) -> List[str]:
    """Returns a list of audio files in subdirectories, excluding the given directory."""
    audio_files = []
    for root, _, files in os.walk(base_path):
        if root.startswith(exclude_dir):
            continue
        for file in files:
            if file.lower().endswith(('.wav')):  # Add other audio file extensions if needed
                audio_files.append(os.path.join(root, file))
    return audio_files

def move_files_to_directory(files: List[str], destination_dir: str):
    """Moves the given files to the destination directory
    Args:
        files: List of file paths to move
        destination_dir: Destination directory to move the files to
    """
    if not os.path.exists(destination_dir):
        os.makedirs(destination_dir)
    for file in files:
        try:
            shutil.move(file, destination_dir)
            print(f"Moved {file} to {destination_dir}")
        except Exception as e:
            print(f"Error moving {file}: {e}")

def main():
    # Get all audio files in subdirectories, excluding the 'raw_audio' directory
    audio_files = get_audio_files_in_subdirectories(BASE_PATH, RAW_AUDIO_DIR)
    print(f"Found {len(audio_files)} audio files to move.")

    # Move files to the 'raw_audio' directory
    move_files_to_directory(audio_files, RAW_AUDIO_DIR)

if __name__ == "__main__":
    main()