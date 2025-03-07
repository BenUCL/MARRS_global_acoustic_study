"""Rename audio to have country and site prefix e.g add 'ind_D5_' prefix for Indonesia degraded site 5.
Parse the directory name to extract the class and the number, sub dirs must have '_M' in them"""

import os
from typing import List, Tuple

BASE_PATH = r'/media/bwilliams/New Volume/marrs_acoustics/australia_acoustics'
PREFIX = 'aus'

def get_directories(base_path: str) -> List[str]:
    """Returns a list of directories in the given base path."""
    return [d for d in os.listdir(base_path) if os.path.isdir(os.path.join(base_path, d))]

def parse_directory_name(dir_name: str) -> Tuple[str, int]:
    """Parses the directory name to extract the class and the number."""
    parts = dir_name.split('_M')
    class_name = parts[0][0]
    print(f'Folder name parts: {parts}')
    number = int(parts[1])
    return class_name, number

def generate_site_mapping(directories: List[str]) -> dict:
    """Generates a mapping of site prefixes based on sorted directory names."""
    site_mapping = {}
    for dir_name in directories:
        class_name, number = parse_directory_name(dir_name)
        if class_name not in site_mapping:
            site_mapping[class_name] = []
        site_mapping[class_name].append((number, dir_name))
    
    for class_name in site_mapping:
        site_mapping[class_name].sort()
        for idx, (number, dir_name) in enumerate(site_mapping[class_name]):
            site_mapping[class_name][idx] = (f"{class_name}{idx + 1}", dir_name)
    
    flat_mapping = {dir_name: site for class_entries in site_mapping.values() for site, dir_name in class_entries}
    print(flat_mapping)
    return flat_mapping

def rename_files(base_path: str, site_mapping: dict) -> None:
    """Renames the files in each directory based on the site mapping."""
    for dir_name, site in site_mapping.items():
        dir_path = os.path.join(base_path, dir_name)
        for file_name in os.listdir(dir_path):
            if file_name.lower().endswith('.wav'):
                new_file_name = f"{PREFIX}_{site}_{file_name}"
                os.rename(
                    os.path.join(dir_path, file_name),
                    os.path.join(dir_path, new_file_name)
                )

def main() -> None:
    base_path = BASE_PATH
    directories = get_directories(base_path)
    site_mapping = generate_site_mapping(directories)
    rename_files(base_path, site_mapping)

if __name__ == "__main__":
    main()