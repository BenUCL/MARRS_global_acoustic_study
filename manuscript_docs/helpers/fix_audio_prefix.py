import os
directory = r'D:\mars_global_acoustic_study\maldives_acoustics'
new_prefix = 'mal'

def rename_files_in_directory(directory: str) -> None:
    for root, _, files in os.walk(directory):
        for file in files:
            if file.lower().endswith('.wav') and not file.startswith(new_prefix):
                new_name = new_prefix + file[3:]
                os.rename(os.path.join(root, file), os.path.join(root, new_name))
                print(f'Renamed: {file} to {new_name}')

rename_files_in_directory(directory)