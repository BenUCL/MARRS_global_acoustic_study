dir = '/media/mars_5tb_drive/mars_global_acoustic_study/mexico_acoustics/raw_audio'
import os
# change all files in dir from eneding in .wav to .WAV
for f in os.listdir(dir):
    if f.endswith('.wav'):
        os.rename(os.path.join(dir, f), os.path.join(dir, f[:-4] + '.WAV'))