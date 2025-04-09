import os
import pandas as pd
#@title Set Paths
base_dir = os.getenv('BASE_DIR')
if not base_dir:
    raise ValueError("BASE_DIR environment variable is not set.")

CSV_PATH = os.path.join(base_dir, "marrs_acoustics/data/results/extra_outputs/agile_auc_roc_scores.csv")

# read in CSV_PATH
DATA = pd.read_csv(CSV_PATH, index_col=0)

# print mean and stdev of roc_auc column
mean_auc = DATA['roc_auc'].mean()
stdev_auc = DATA['roc_auc'].std()
print(f"Mean AUC: {mean_auc}")          
print(f"Standard Deviation AUC: {stdev_auc}")

