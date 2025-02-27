#!/usr/bin/env python3
"""
Compute temporal kernels and aggregated plots for each sound in a country.
For each country, only detections from site–date pairs with at least 95% coverage are used.
For each sound, a non-parametric kernel density is computed for each treatment over a 24-hour period.
Aggregated plots are produced only if the sound has at least 100 detections in each treatment for either:
  - Group1: healthy, degraded, restored, or
  - Group2: healthy, degraded, newly_restored.
If both conditions are met, a single plot is produced that includes the union of treatments.
Plots are saved with filenames prefixed with "aggreg_" (no suffix is added).
"""

import os
import logging
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import gaussian_kde
from typing import List, Tuple, Set, Dict, Any

# Import functions and variables from count_ecofunctions
from count_ecofunctions import (
    BASE_DIR,
    COUNTRY_CONFIG,
    FILE_COVERAGE,
    LOGIT_CUTOFF,
    parse_date,
    parse_site,
    parse_treatment,
    load_raw_file_list,
    get_expected_daily_recordings,
    parse_date_time,
)

# Global constants
COVERAGE_THRESHOLD = 0.95    # 95% coverage required per site–date pair
SMOOTHING = 0.5              # Smoothing factor for kernel density estimation
KERNEL_PLOTS_DIR = os.path.join(
    BASE_DIR, "marrs_acoustics/data/results/functions/kernels/plots"
)
TREATMENT_COLOURS = {
    "healthy": "green",
    "degraded": "red",
    "restored": "blue",
    "newly_restored": "orange"
}
# All treatments in desired order
TREATMENTS = ["healthy", "degraded", "restored", "newly_restored"]

# Define groupings for aggregated plotting
GROUP1 = ["healthy", "degraded", "restored"]
GROUP2 = ["healthy", "degraded", "newly_restored"]

def get_valid_site_dates(country: str, threshold: float = COVERAGE_THRESHOLD) -> Set[Tuple[str, str]]:
    """
    Return a set of (site, date) pairs for the given country with at least the required coverage.
    """
    raw_list_path = os.path.join(
        BASE_DIR, "marrs_acoustics/data", f"output_dir_{country}", "raw_file_list.csv"
    )
    if not os.path.isfile(raw_list_path):
        logging.warning(f"raw_file_list.csv not found for {country}. Path: {raw_list_path}")
        return set()
    df_raw = pd.read_csv(raw_list_path)
    df_raw["filename_part"] = df_raw["filename"].apply(lambda x: x.split("/", 1)[-1])
    df_raw["date"] = df_raw["filename_part"].apply(parse_date)
    df_raw["site"] = df_raw["filename_part"].apply(parse_site)
    df_counts = df_raw.groupby(["site", "date"]).size().reset_index(name="n_files")
    duty_cycle = COUNTRY_CONFIG[country]["duty_cycle"]
    expected_daily = get_expected_daily_recordings(duty_cycle)
    valid = df_counts[df_counts["n_files"] >= expected_daily * threshold]
    valid_pairs = set(valid.apply(lambda row: (row["site"], row["date"]), axis=1))
    return valid_pairs

def process_sound_country(country: str, valid_pairs: Set[Tuple[str, str]]) -> None:
    """
    For each sound in the country's agile_outputs folder, produce an aggregated kernel plot 
    if the sound meets detection criteria.
    
    Aggregated plots are produced if the sound has at least 100 detections in each treatment 
    for either:
      - Group1: healthy, degraded, restored, or
      - Group2: healthy, degraded, newly_restored.
    If both criteria are met, the plot will include the union of treatments (all four).
    The aggregated plot is saved with filename "aggreg_<sound>.png".
    """
    agile_dir = os.path.join(
        BASE_DIR, "marrs_acoustics/data", f"output_dir_{country}", "agile_outputs"
    )
    if not os.path.isdir(agile_dir):
        logging.warning(f"Agile outputs directory not found for {country}: {agile_dir}")
        return

    country_plot_dir = os.path.join(KERNEL_PLOTS_DIR, country)
    os.makedirs(country_plot_dir, exist_ok=True)

    for sound_folder in os.listdir(agile_dir):
        folder_path = os.path.join(agile_dir, sound_folder)
        if not os.path.isdir(folder_path):
            continue
        csv_path = os.path.join(folder_path, f"{sound_folder}_inference.csv")
        if not os.path.isfile(csv_path):
            logging.info(f"No CSV found for sound {sound_folder} in {country}. Skipping.")
            continue

        logging.info(f"Processing sound: {sound_folder} in {country}")
        try:
            df_infer = pd.read_csv(csv_path)
        except Exception as e:
            logging.error(f"Error reading CSV for sound {sound_folder} in {country}: {e}")
            continue

        logit_col = " logit" if " logit" in df_infer.columns else "logit"
        df_infer = df_infer[df_infer[logit_col] >= 1.0]
        if df_infer.empty:
            logging.info(f"No detections for sound {sound_folder} in {country} after logit filtering.")
            continue

        records = []
        for _, row in df_infer.iterrows():
            filename_full = row["filename"]
            filename_part = filename_full.split("/", 1)[-1]
            site = parse_site(filename_part)
            date_str = parse_date(filename_part)
            if (site, date_str) not in valid_pairs:
                continue
            try:
                dt = parse_date_time(filename_part)
            except Exception as e:
                logging.error(f"Error parsing datetime from {filename_part}: {e}")
                continue
            time_decimal = dt.hour + dt.minute/60 + dt.second/3600
            treatment = parse_treatment(filename_part)
            records.append({"treatment": treatment, "time": time_decimal})
        
        if not records:
            logging.info(f"No valid detections for sound {sound_folder} in {country} on valid site-date pairs.")
            continue

        df_records = pd.DataFrame(records)
        # Count detections per treatment
        counts = df_records.groupby("treatment").size().to_dict()
        group1_ok = all(counts.get(t, 0) >= 100 for t in GROUP1)
        group2_ok = all(counts.get(t, 0) >= 100 for t in GROUP2)
        if not group1_ok and not group2_ok:
            logging.info(f"Sound {sound_folder} does not meet aggregated criteria for either grouping. Skipping plot.")
            continue

        # If qualifies for both, take union (i.e., all treatments)
        if group1_ok and group2_ok:
            treatments_to_plot = TREATMENTS
        elif group1_ok:
            treatments_to_plot = GROUP1
        else:
            treatments_to_plot = GROUP2

        # Produce a single plot using the chosen treatments
        time_grid = np.linspace(0, 24, 240)
        plt.figure(figsize=(8, 6))
        for treatment in treatments_to_plot:
            times = df_records[df_records["treatment"] == treatment]["time"].values
            if times.size == 0:
                plt.plot([], [], color=TREATMENT_COLOURS[treatment], label=treatment)
                continue
            try:
                kde = gaussian_kde(times, bw_method=SMOOTHING)
                density = kde(time_grid)
                plt.plot(time_grid, density, color=TREATMENT_COLOURS[treatment], label=treatment)
            except Exception as e:
                logging.error(f"Error computing KDE for sound {sound_folder} treatment {treatment} in {country}: {e}")
                plt.plot([], [], color=TREATMENT_COLOURS[treatment], label=treatment)
        plt.xlabel("Hour of Day")
        plt.ylabel("Density")
        plt.title(f"Temporal Kernel for {sound_folder} in {country}")
        plt.xlim(0, 24)
        plt.legend()
        plot_path = os.path.join(country_plot_dir, f"aggreg_{sound_folder}.png")
        plt.savefig(plot_path)
        plt.close()
        logging.info(f"Saved aggregated plot for sound {sound_folder} in {country} to {plot_path}")

def main() -> None:
    """Process each country and compute aggregated temporal kernels for each sound."""
    logging.basicConfig(level=logging.INFO)
    for country in COUNTRY_CONFIG.keys():
        logging.info(f"Processing country: {country}")
        valid_pairs = get_valid_site_dates(country, COVERAGE_THRESHOLD)
        if not valid_pairs:
            logging.info(f"No site-date pairs with sufficient coverage for {country}. Skipping.")
            continue
        process_sound_country(country, valid_pairs)

if __name__ == "__main__":
    main()
