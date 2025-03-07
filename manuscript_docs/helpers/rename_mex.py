"""The original mexico R1 has been removed as it is only 13 moths old.
So now renaming R1 to R2."""

folder_path = '/media/bwilliams/New Volume/marrs_acoustics/mexico_acoustics/raw_audio'

# find audio files in dir where 4-5th char is R2. Change to R1
import os
from tqdm import tqdm

def rename_files(folder_path: str) -> None:
    try:
        files = [f for f in os.listdir(folder_path) if f.endswith(".WAV")]
        for filename in tqdm(files, desc="Renaming Files", unit="file"):
            if len(filename) > 4 and filename[4:6] == 'R2':
                # Rename by replacing 'R2' with 'R1'
                new_filename = filename[:4] + 'R1' + filename[6:]
                old_file_path = os.path.join(folder_path, filename)
                new_file_path = os.path.join(folder_path, new_filename)
                print(f"Renaming {filename} to {new_filename}")
                os.rename(old_file_path, new_file_path)
    except Exception as e:
        print(f"Error: {e}")


rename_files(folder_path)

