
# Description:
#   This script processes detection CSV files from various countries to compute
#   per-sound overlap coefficients and perform Watson's two-sample tests between 
#   pairs of treatments (healthy, degraded, restored, newly_restored). For each 
#   sound and each pair of treatments, the script:
#     - Computes an overlap estimate using the 'overlap' package.
#     - Bootstraps the overlap estimate to obtain a 95% confidence interval.
#     - Performs Watson's two-sample test of homogeneity and extracts the test 
#       statistic and p-value.
#     - Flags comparisons with a sample size (n1 or n2) below 100 by setting an 
#       'exclude' column to "yes"; otherwise, it is set to "no".
#
#   Aggregated results (mean values across sounds) are computed only for those
#   comparisons where both groups have n >= 100.
#
# Inputs:
#   - CSV files named *_raw_detection_times.csv 
#     Each CSV must contain at least the columns: "treatment" and "time" (time in hours).
#
# Outputs:
#   - A text file containing both per-sound results and aggregated (filtered) results.
#
# Note, the circular package only gives p-value thresholds (e.g., 0.1, 0.001). This is 
# a limitation of the package.
#
# Also run these commands from the linux terminal to install overlap dependencies:
#   sudo apt-get install libudunits2-dev
#   sudo apt-get install libgdal-dev
#

# Required libraries
if (!require("overlap")) install.packages("overlap", repos = "http://cran.rstudio.com/")
library(overlap)
if (!require("circular")) install.packages("circular", repos = "http://cran.rstudio.com/")
library(circular)
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.rstudio.com/")
library(dplyr)

# Num CPU cores and bootstraps
NUM_CORES = 8
BOOTSTRAPS = 100
options(max.print = 10000000)

# Get BASE_DIR from environment variable
BASE_DIR <- Sys.getenv("BASE_DIR")
if(BASE_DIR == ""){
  stop("BASE_DIR environment variable is not set")
}

# Define countries (mirroring the Python configuration)
COUNTRY_CONFIG <- list(
  australia = list(duty_cycle = 4),
  maldives = list(duty_cycle = 4),
  kenya = list(duty_cycle = 4),
  indonesia = list(duty_cycle = 2),
  mexico = list(duty_cycle = 4)
)

# Define treatment levels (order matters for pairwise comparisons)
TREATMENTS <- c("healthy", "degraded", "restored", "newly_restored")

# Define output file path using BASE_DIR
result_file <- file.path(BASE_DIR, "marrs_acoustics/data/results/functions/kernels/kernel_v1_results.txt")

# Initialize list to store per-sound results
results_list <- list()

# Loop over each country
for(country in names(COUNTRY_CONFIG)){
  cat("\n==================================================================================\n")
  cat("Processing country:", country, "\n")
  
  # Folder where the raw detection CSV files are saved
  country_folder <- file.path(BASE_DIR, "marrs_acoustics/data/results/functions/kernels/plots", country)
  if(!dir.exists(country_folder)){
    cat("Folder does not exist for", country, "\n")
    next
  }
  
  # List CSV files ending with "_raw_detection_times.csv"
  csv_files <- list.files(country_folder, pattern = "_raw_detection_times\\.csv$", full.names = TRUE)
  if(length(csv_files) == 0){
    cat("No raw detection CSV files found for", country, "\n")
    next
  }
  
  # Process each sound's CSV file
  for(csv_file in csv_files){
    sound_name <- sub("_raw_detection_times\\.csv$", "", basename(csv_file))
    cat("\nProcessing sound:", sound_name, "\n")
    
    data <- read.csv(csv_file, stringsAsFactors = FALSE)
    
    # Check required columns
    if(!all(c("treatment", "time") %in% names(data))){
      cat("CSV file", csv_file, "does not have required columns. Skipping.\n")
      next
    }
    
    # Loop over each pair of treatments
    for(i in 1:(length(TREATMENTS)-1)){
      for(j in (i+1):length(TREATMENTS)){
        treat1 <- TREATMENTS[i]
        treat2 <- TREATMENTS[j]
        data1 <- data$time[data$treatment == treat1]
        data2 <- data$time[data$treatment == treat2]
        n1 <- length(data1)
        n2 <- length(data2)
        
        cat("\nComparison:", treat1, "vs", treat2, " | n1:", n1, " | n2:", n2, "\n")
        
        # Set exclude flag: "yes" if either group has < 100 detections, "no" otherwise.
        exclude_val <- ifelse(n1 < 100 | n2 < 100, "yes", "no")
        
        if(n1 == 0 || n2 == 0){
          cat("Skipping due to missing data\n")
          result <- data.frame(
            country = country,
            sound = sound_name,
            treatment1 = treat1,
            treatment2 = treat2,
            n1 = n1,
            n2 = n2,
            estimator = NA,
            overlap = NA,
            ci_lower = NA,
            ci_upper = NA,
            watson_p = NA,
            watson_stat = NA,
            exclude = exclude_val,
            stringsAsFactors = FALSE
          )
          results_list[[length(results_list) + 1]] <- result
          next
        }
        
        # Convert times to radians for circular analysis.
        x_rad <- circular(data1 * 2 * pi / 24, units = "radians", modulo = "2pi")
        y_rad <- circular(data2 * 2 * pi / 24, units = "radians", modulo = "2pi")
        
        min_n <- min(n1, n2)
        estimator <- ifelse(min_n >= 50, "Dhat4", "Dhat1")
        
        cat("Running overlapEst...\n")
        overlap_est <- overlapEst(as.numeric(x_rad), as.numeric(y_rad), type = estimator)
        cat("Overlap estimate:", overlap_est, "\n")
        
        cat("Running bootstrap...\n")
        bt <- bootstrap(as.numeric(x_rad), as.numeric(y_rad), nb = BOOTSTRAPS, type = estimator, cores = NUM_CORES)
        
        if(length(bt) > 0){
          cat("Running bootCI...\n")
          ci_mat <- bootCI(overlap_est, bt, conf = 0.95)
          ci_lower <- ci_mat[1,1]
          ci_upper <- ci_mat[1,2]
          cat("Confidence Interval:", ci_lower, "-", ci_upper, "\n")
        } else {
          cat("Bootstrapping failed! Setting CI to NA\n")
          ci_lower <- NA
          ci_upper <- NA
        }
        
        cat("Running Watson's test...\n")
        watson_result <- tryCatch({
          watson_test <- watson.two.test(x_rad, y_rad)
          p_val <- watson_test$p.value
          if(length(p_val) == 0 || is.null(p_val)){
            watson_text <- capture.output(print(watson_test))
            p_line <- watson_text[grep("P-value", watson_text)]
            m <- regmatches(p_line, regexpr("[0-9\\.]+", p_line))
            p_val_numeric <- as.numeric(m)
          } else {
            p_val_numeric <- as.numeric(p_val)
          }
          stat_val <- watson_test$statistic
          if(length(stat_val) == 0 || is.null(stat_val)){
            watson_text <- capture.output(print(watson_test))
            stat_line <- watson_text[grep("Test Statistic", watson_text)]
            m_stat <- regmatches(stat_line, regexpr("[0-9\\.]+", stat_line))
            stat_numeric <- as.numeric(m_stat)
          } else {
            stat_numeric <- as.numeric(stat_val)
          }
          list(p_value = p_val_numeric, statistic = stat_numeric)
        }, error = function(e) {
          cat("Watson's test failed! Error message:", e$message, "\nAssigning NA\n")
          list(p_value = NA_real_, statistic = NA_real_)
        })
        cat("Watson p-value:", watson_result$p_value, "\n")
        cat("Watson test statistic:", watson_result$statistic, "\n")
        
        result <- data.frame(
          country = country,
          sound = sound_name,
          treatment1 = treat1,
          treatment2 = treat2,
          n1 = n1,
          n2 = n2,
          estimator = estimator,
          overlap = overlap_est,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          watson_p = watson_result$p_value,
          watson_stat = watson_result$statistic,
          exclude = exclude_val,
          stringsAsFactors = FALSE
        )
        results_list[[length(results_list) + 1]] <- result
        cat("Result stored successfully\n")
      }
    }
  }
}

results_df <- do.call(rbind, results_list)

# For aggregated results, only include rows where exclude == "no"
filtered_df <- results_df %>% filter(exclude == "no")
aggregated <- filtered_df %>%
  group_by(country, treatment1, treatment2) %>%
  summarise(mean_overlap = mean(overlap, na.rm = TRUE),
            mean_ci_lower = mean(ci_lower, na.rm = TRUE),
            mean_ci_upper = mean(ci_upper, na.rm = TRUE),
            mean_watson_p = mean(watson_p, na.rm = TRUE),
            mean_watson_stat = mean(watson_stat, na.rm = TRUE),
            n_sounds = n(),
            .groups = "drop")

# Convert aggregated results to data.frame
aggregated_df <- as.data.frame(aggregated)

# ------------------- Output Section -------------------
# Format tables for fixed-width printing
results_formatted <- format(results_df, justify = "right")
aggregated_formatted <- format(aggregated_df, justify = "right")

sink(result_file)
cat("Per-sound overlap results:\n")
header1 <- paste(colnames(results_df), collapse = "   ")
writeLines(header1)
writeLines(apply(results_formatted, 1, paste, collapse = "   "))

cat("\n\nAggregated overlap results (mean across sounds; only include comparisons with both n1 and n2 >= 100):\n")
header2 <- paste(colnames(aggregated_df), collapse = "   ")
writeLines(header2)
writeLines(apply(aggregated_formatted, 1, paste, collapse = "   "))
sink()

cat("\nResults saved to", result_file, "\n")
