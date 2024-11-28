"""Rename files not taken in the right timezone. This may happen when they set up in the UK and turned on abroad."""

import os
from datetime import datetime, timedelta
from tqdm import tqdm

# Directory containing the audio files
DIRECTORY = "/media/mars_5tb_drive/mars_global_acoustic_study/mexico_acoustics/raw_audio"
TIME_DIFF = -7

# Create path
base_dir = os.getenv('BASE_DIR')
if not base_dir:
    raise ValueError("BASE_DIR environment variable is not set.")

def adjust_timestamps(directory: str, time_diff: int):
    # Get all .wav files in the directory
    files = [f for f in os.listdir(directory) if f.lower().endswith(".wav")]
    
    # Use tqdm to track progress
    for filename in tqdm(files, desc="Processing files"):
        parts = filename.split("_")
        
        # Extract the timestamp part
        date_time_str = parts[2] + "_" + parts[3].split(".")[0]
        original_datetime = datetime.strptime(date_time_str, "%Y%m%d_%H%M%S")
        
        # Subtract the specified hours
        adjusted_datetime = original_datetime + timedelta(hours=time_diff)
        
        # Format the adjusted datetime back to the original format
        adjusted_date_time_str = adjusted_datetime.strftime("%Y%m%d_%H%M%S")
        
        # Construct the new filename
        new_filename = f"{parts[0]}_{parts[1]}_{adjusted_date_time_str}.wav"
        
        # Rename the file
        original_path = os.path.join(directory, filename)
        new_path = os.path.join(directory, new_filename)
        os.rename(original_path, new_path)


if __name__ == "__main__":
    adjust_timestamps(DIRECTORY, TIME_DIFF)