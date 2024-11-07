"""This script can be used to write out the audio clips that were detected by the model.
1. It reads the inference csv, finds the raw audio sample and writes out each 5sec clip that 
was predicted to contain the target sound. 
2. The new filename will begin with the logit score, then the timestamp in seconds, and 
finally the original filename. TODO: Set so that new folders for each country and sound are
created.
3. Set the logit cutoff accordingly to filter by confidence. It will first write the 
predictions with the lowest logit score, so that  these can be reviewed first if not running 
the script until completion. """

# Imports
from pathlib import Path
import pandas as pd
import librosa
import os
import soundfile as sf
from tqdm import tqdm
from typing import Optional

# Only write out files where logits are equal to above this score
TARGET_COUNTRY = 'indonesia'
TARGET_SOUND = 'scrape_fullband'
# Only samples with a logit score above the value set below will be written. Set to 0 to write all samples. 
LOGIT_CUTOFF = 1.0 
# If set to None, all files will be written, otherwise will stop after this number of files.
MAX_COUNT: Optional[int] = 100

# Validate MAX_COUNT type
assert MAX_COUNT is None or isinstance(MAX_COUNT, int), 'MAX_COUNT must be None or an integer'


# Set base dir for filepaths
base_dir = os.getenv('BASE_DIR')
if not base_dir:
    raise ValueError('BASE_DIR environment variable is not set.')

# Define file paths
csv_path = os.path.join(base_dir, 'marrs_acoustics/data/output_dir_' + TARGET_COUNTRY 
                        + '/agile_outputs/' + TARGET_SOUND + '/' + TARGET_SOUND + '_inference.csv')
audio_base_path = Path('/media/mars_5tb_drive/mars_global_acoustic_study/' + TARGET_COUNTRY + '_acoustics/')
save_dir = Path(os.path.join(base_dir, 'marrs_acoustics/data/pred_audio_samples/' + TARGET_COUNTRY + '/' + TARGET_SOUND))

# Create the save directory if it doesn't exist
save_dir.mkdir(parents=True, exist_ok=True)

# Load CSV
df = pd.read_csv(csv_path)

# Filter by logit cutoff and sort by logit value in ascending order
df = df[df[' logit'] >= LOGIT_CUTOFF].sort_values(by=' logit')

# Process each row with tqdm for progress tracking
for i, (_, row) in enumerate(tqdm(df.iterrows(), total=len(df), desc='Processing audio clips')):
    # Exit loop if MAX_COUNT is reached
    if MAX_COUNT is not None and i >= MAX_COUNT:
        print(f'Stopping early because MAX_COUNT ({MAX_COUNT}) was reached.')
        break  
    # Beware leading white space for some column headers in the csv.
    filename, timestamp_s, label, logit = row['filename'], row[' timestamp_s'], row[' label'], row[' logit']
    full_audio_path = audio_base_path / filename

    # Load audio
    y, sr = librosa.load(full_audio_path, sr=None, offset=timestamp_s, duration=5.0)

    # Construct new filename
    timestamp_str = f"{int(timestamp_s):02d}sec"
    new_filename = f"{logit}_{timestamp_str}_{filename.split('/')[-1]}"
    save_path = save_dir / new_filename

    # Save the 5-second clip
    sf.write(save_path, y, sr)