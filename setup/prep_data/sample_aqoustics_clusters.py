import os
import random
import shutil
from pathlib import Path

COUNTRY = "australia"
COUNT_PER_CLUSTER = 10

base_dir = os.getenv('BASE_DIR')
if not base_dir:
    raise ValueError("BASE_DIR environment variable is not set.")

# set src_directory to base + country
parent_dir= os.path.join(base_dir, 'marrs_acoustics/aqoustics/' + COUNTRY)
clusters_dir = os.path.join(parent_dir, COUNTRY + "_clusters")
output_dir = os.path.join(parent_dir, "for_review")

def select_and_copy_files(src_dir: str, dest_dir: str, num_files: int = COUNT_PER_CLUSTER) -> None:
    # Ensure the destination directory exists
    Path(dest_dir).mkdir(parents=True, exist_ok=True)
    
    # List all subdirectories in the source directory
    for subdir in os.listdir(src_dir):
        subdir_path = os.path.join(src_dir, subdir)
        
        # Ensure we are working with directories
        if os.path.isdir(subdir_path):
            # List all files in the subdirectory
            files = [f for f in os.listdir(subdir_path) if f.lower().endswith(".wav")]

            # Select random files (ensure there are at least num_files available)
            selected_files = random.sample(files, min(len(files), COUNT_PER_CLUSTER))
            
            for file in selected_files:
                src_file_path = os.path.join(subdir_path, file)
                
                # Prefix the file name with the subfolder name
                new_file_name = f"{subdir}_{file}"
                dest_file_path = os.path.join(dest_dir, new_file_name)
                
                # Copy the file to the new location
                shutil.copy(src_file_path, dest_file_path)
                print(f"Copied {file} to {dest_file_path}")


# Run the function
select_and_copy_files(clusters_dir, output_dir)
