dir = '/media/bwilliams/New Volume/marrs_acoustics/australia_acoustics/raw_audio'

# for every audio file in dir and any sub dirs, in filename change 'd' to 'D', 'h' to 'H', 'r' to 'R' where present
# e.g. 'ind_d5_20211023_230000.wav' -> 'ind_D5_20211023_230000.wav'
import os
import shutil
from typing import List

def get_audio_files_in_subdirectories(base_path: str, exclude_dir: str) -> List[str]:
    audio_files = []
    for root, _, files in os.walk(base_path):
        print(f"Root: {root}")
        print(f'Length of files: {len(files)}')
        if root == exclude_dir:
            continue
        for file in files:
            if file.lower().endswith('.wav'):
                audio_files.append(os.path.join(root, file))
    return audio_files
        

def move_files_to_directory(files: List[str], destination_dir: str):
    if not os.path.exists(destination_dir):
        os.makedirs(destination_dir)
    for file in files:  
        try:
            dir_path = os.path.dirname(file)
            filename = os.path.basename(file)
            new_filename = filename.replace('d', 'D').replace('h', 'H').replace('r', 'R')
            new_name = os.path.join(dir_path, new_filename)
            os.rename(file, new_name)
        except Exception as e:
            print(f"Error renaming {file}: {e}")

def main():
    # Get all audio files in subdirectories, excluding the 'raw_audio' directory
    audio_files = get_audio_files_in_subdirectories(dir, dir)
    print(f"Found {len(audio_files)} audio files to move.")

    # Move files to the 'raw_audio' directory
    move_files_to_directory(audio_files, dir)

if __name__ == "__main__":
    main()


