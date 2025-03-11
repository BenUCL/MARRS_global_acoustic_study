"""Rename R to N in Mald or kenya file names
The Maldives R sites are restored sites. However, they are newly restored.
I therefore renamed these to match the Indonesia format which had a mix of both,
where N was used to denote newly restored sites.
"""

import os
from tqdm import tqdm

folder_path = "/media/bwilliams/New Volume/mars_global_acoustic_study/kenya_acoustics/raw_audio"

def rename_files(folder_path: str) -> None:
    try:
        files = [f for f in os.listdir(folder_path) if f.endswith(".WAV")]
        for filename in tqdm(files, desc="Renaming Files", unit="file"):
            if len(filename) > 4 and filename[4] == 'R':
                # Rename by replacing 'R' with 'N'
                new_filename = filename[:4] + 'N' + filename[5:]
                old_file_path = os.path.join(folder_path, filename)
                new_file_path = os.path.join(folder_path, new_filename)
                os.rename(old_file_path, new_file_path)
    except Exception as e:
        print(f"Error: {e}")

# Specify the folder path
rename_files(folder_path)
