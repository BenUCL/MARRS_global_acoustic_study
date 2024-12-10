"""
Samples NUM_FILES random audio files from the source directory.
This for selecting raw audio source files, which were 1min in length in this study,
from which these could be screened for source sonotypes alongside the clustering method. 
"""

import os
import random
import shutil
from typing import List


# Set directories
COUNTRY = 'kenya'
SRC_DIR = '/media/mars_5tb_drive/mars_global_acoustic_study/' + COUNTRY + '_acoustics/raw_audio'
NUM_FILES = 1000

base_dir = os.getenv('BASE_DIR')
if not base_dir:
    raise ValueError("BASE_DIR environment variable is not set.")

dest_dir = os.path.join(base_dir,'marrs_acoustics/aqoustics/' + COUNTRY + '/random_samples')

# Function to copy random files
def copy_random_files(src_dir: str, dest_dir: str, num_files: int) -> None:
    """
    Copies a specified number of random .wav files from the source directory to the destination directory.
    
    Parameters:
        src_dir (str): Source directory containing .wav files.
        dest_dir (str): Destination directory to copy files to.
        num_files (int): Number of files to copy.
    """
    if not os.path.exists(src_dir):
        raise FileNotFoundError(f"Source directory does not exist: {src_dir}")
    
    if not os.path.exists(dest_dir):
        os.makedirs(dest_dir)
        print(f"Destination directory created: {dest_dir}")
    
    files = [f for f in os.listdir(src_dir) if f.lower().endswith(".wav")]
    
    if len(files) < num_files:
        raise ValueError(f"Not enough files in the source directory. Found {len(files)}, but need {num_files}.")
    
    selected_files = random.sample(files, num_files)
    
    for file in selected_files:
        src_file_path = os.path.join(src_dir, file)
        dest_file_path = os.path.join(dest_dir, file)
        shutil.copy(src_file_path, dest_file_path)
        print(f"Copied {file} to {dest_dir}")

if __name__ == "__main__":
    try:
        copy_random_files(SRC_DIR, dest_dir, NUM_FILES)
        print("File copying completed successfully.")
    except Exception as e:
        print(f"An error occurred: {e}")
