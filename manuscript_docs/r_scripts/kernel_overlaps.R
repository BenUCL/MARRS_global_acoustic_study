#!/usr/bin/env Rscript
# This script computes per-sound overlap coefficients and Watson's two-sample tests for each treatment pair
# for each country. It then pools the results across sounds (treating each sound equally) and saves everything
# to a text file for downstream analysis in R.


# Required libraries
if (!require("circular")) install.packages("circular", repos = "http://cran.rstudio.com/")
library(circular)
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.rstudio.com/")
library(dplyr)

### The overlap package would not install, instead we define the necessary functions here:
# Define a function to compute the overlap coefficient between two circular distributions.
calcOverlap <- function(x, y, n = 512, bw = "nrd") {
  # x and y are vectors (in radians)
  d1 <- density.circular(x, bw = bw, n = n)
  d2 <- density.circular(y, bw = bw, n = n)
  dx <- d1$x[2] - d1$x[1]
  overlap <- sum(pmin(d1$y, d2$y)) * dx
  return(overlap)
}

# Define a function to bootstrap the overlap coefficient for 95% confidence intervals.
bootstrapOverlap <- function(x, y, nboot = 10000, n = 512, bw = "nrd") {
  boot_vals <- numeric(nboot)
  n_x <- length(x)
  n_y <- length(y)
  for (i in 1:nboot) {
    sample_x <- sample(x, n_x, replace = TRUE)
    sample_y <- sample(y, n_y, replace = TRUE)
    boot_vals[i] <- calcOverlap(sample_x, sample_y, n = n, bw = bw)
  }
  return(quantile(boot_vals, probs = c(0.025, 0.975)))
}



# Get BASE_DIR from environment variable
BASE_DIR <- Sys.getenv("BASE_DIR")
if(BASE_DIR == ""){
  stop("BASE_DIR environment variable is not set")
}

# Define countries (mirroring the Python configuration)
COUNTRY_CONFIG <- list(
  australia = list(duty_cycle = 4),
  kenya = list(duty_cycle = 4),
  indonesia = list(duty_cycle = 2),
  maldives = list(duty_cycle = 4),
  mexico = list(duty_cycle = 4)
)

# Define treatment levels (order matters for pairwise comparisons)
TREATMENTS <- c("healthy", "degraded", "restored", "newly_restored")

# Define output file path using BASE_DIR
result_file <- file.path(BASE_DIR, "marrs_acoustics/data/results/functions/kernels/kernel_results.txt")

# Initialize list to store per-sound results
results_list <- list()

# Loop over each country
for(country in names(COUNTRY_CONFIG)){
  cat("Processing country:", country, "\n")
  
  # Folder where the raw detection CSV files (and plots) are saved
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
    # Sound name is derived by removing the suffix
    sound_name <- sub("_raw_detection_times\\.csv$", "", basename(csv_file))
    data <- read.csv(csv_file, stringsAsFactors = FALSE)
    
    # Check that required columns exist
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
        
        # If one treatment is missing, record NA and move on
        if(n1 == 0 || n2 == 0){
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
            stringsAsFactors = FALSE
          )
          results_list[[length(results_list) + 1]] <- result
          next
        }
        
        # Convert times from hours to radians for circular analysis.
        # (2*pi/24 converts hours to radians)
        x_rad <- circular(data1 * 2 * pi / 24, units = "radians", modulo = "2pi")
        y_rad <- circular(data2 * 2 * pi / 24, units = "radians", modulo = "2pi")
        
        # Decide which estimator to use:
        # Use Dhat1 if the smaller sample is between 20 and 50 detections,
        # and Dhat4 if the minimum sample size is >= 50.
        min_n <- min(n1, n2)
        estimator <- ifelse(min_n >= 50, "Dhat4", "Dhat1")
        
        # Compute the overlap coefficient using our custom function.
        overlap_est <- calcOverlap(as.numeric(x_rad), as.numeric(y_rad))
        
        # Bootstrap 95% confidence intervals using 10,000 iterations.
        ci <- bootstrapOverlap(as.numeric(x_rad), as.numeric(y_rad), nboot = 10000)
        
        # Run Watson's two-sample test for homogeneity
        watson_test <- watson.two.test(x_rad, y_rad)
        p_val <- watson_test$p.value
        
        # Store results
        result <- data.frame(
          country = country,
          sound = sound_name,
          treatment1 = treat1,
          treatment2 = treat2,
          n1 = n1,
          n2 = n2,
          estimator = estimator,
          overlap = overlap_est,
          ci_lower = ci[1],
          ci_upper = ci[2],
          watson_p = p_val,
          stringsAsFactors = FALSE
        )
        results_list[[length(results_list) + 1]] <- result
      }
    }
  }
}

# Combine all per-sound results into one data frame
results_df <- do.call(rbind, results_list)

# Pool results across sounds for each country and treatment pair
aggregated <- results_df %>%
  group_by(country, treatment1, treatment2) %>%
  summarise(mean_overlap = mean(overlap, na.rm = TRUE),
            mean_ci_lower = mean(ci_lower, na.rm = TRUE),
            mean_ci_upper = mean(ci_upper, na.rm = TRUE),
            mean_watson_p = mean(watson_p, na.rm = TRUE),
            n_sounds = n(),
            .groups = "drop")

# Write both the per-sound and aggregated results to the output file
sink(result_file)
cat("Per-sound overlap results:\n")
print(results_df)
cat("\nAggregated overlap results (mean across sounds):\n")
print(aggregated)
sink()

cat("Results saved to", result_file, "\n")
