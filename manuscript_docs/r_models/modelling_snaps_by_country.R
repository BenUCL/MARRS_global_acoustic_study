# Required libraries
if (!require("lme4")) install.packages("lme4", repos = "http://cran.rstudio.com/")
library(lme4)
if (!require("emmeans")) install.packages("emmeans", repos = "http://cran.rstudio.com/")
library(emmeans)

# Check environment variable
base_dir <- Sys.getenv("BASE_DIR")
if (base_dir == "") {
  stop("BASE_DIR is not set. Please set it in your environment variables.")
}

# Ecological function
eco_function <- "snaps_count"
valid_functions <- c("graze_count", "snaps_count")
if (!(eco_function %in% valid_functions)) {
  stop("Invalid eco_function. Please set it to one of: ", paste(valid_functions, collapse = ", "))
}

# Path to the per-country combined CSV (only FE results get appended)
country_combined_csv <- file.path(
  base_dir,
  "marrs_acoustics/data/results/functions/stats/summary_outputs",
  "country_combined_results.csv"
)

###############################################################################
# Helper to capture & append FE results
###############################################################################
capture_fe_results <- function(model, eco_fun, model_name, country_name,
                               coefs_log, coefs_exp, coefs_raw) {
  terms <- rownames(coefs_log)
  
  # Attempt to retrieve p-values
  if ("Pr(>|z|)" %in% colnames(coefs_raw)) {
    pvals <- coefs_raw[, "Pr(>|z|)"]
  } else if ("Pr(>|t|)" %in% colnames(coefs_raw)) {
    pvals <- coefs_raw[, "Pr(>|t|)"]
  } else {
    pvals <- rep(NA, length(terms))
  }
  
  # Build log-scale rows
  df_log <- data.frame(
    country = country_name,
    eco_function = eco_fun,
    model_name = model_name,
    scale = "Log-Scale",
    term = terms,
    Estimate = coefs_log$Estimate,
    Lower_95 = coefs_log$Lower_95,
    Upper_95 = coefs_log$Upper_95,
    Lower_75 = coefs_log$Lower_75,
    Upper_75 = coefs_log$Upper_75,
    p_value = pvals
  )
  
  # Build exponentiated rows
  df_exp <- data.frame(
    country = country_name,
    eco_function = eco_fun,
    model_name = model_name,
    scale = "Original-Scale",
    term = terms,
    Estimate = coefs_exp$Estimate,
    Lower_95 = coefs_exp$Lower_95,
    Upper_95 = coefs_exp$Upper_95,
    Lower_75 = coefs_exp$Lower_75,
    Upper_75 = coefs_exp$Upper_75,
    p_value = pvals
  )
  
  # Combine and append
  results_to_append <- rbind(df_log, df_exp)
  if (!file.exists(country_combined_csv)) {
    write.csv(results_to_append, country_combined_csv, row.names = FALSE)
  } else {
    write.table(results_to_append, country_combined_csv, sep = ",", 
                row.names = FALSE, col.names = FALSE, append = TRUE)
  }
}

###############################################################################
# Modified helper for printing results & optional CSV capture
###############################################################################
print_model_results <- function(model, model_name, country_name) {
  cat(paste0("\n### ", model_name, " Summary for ", toupper(country_name), " ###\n"),
      file = summary_file, append = TRUE)
  capture.output(print(summary(model)), file = summary_file, append = TRUE)
  
  # Extract fixed effects
  coefs_raw <- summary(model)$coefficients
  
  # Define z-scores for confidence intervals
  z_975 <- qnorm(0.975)  # 95%
  z_875 <- qnorm(0.875)  # 75%
  
  # Compute log-scale intervals
  coefs_log <- data.frame(
    Estimate  = coefs_raw[, "Estimate"],
    Lower_95  = coefs_raw[, "Estimate"] - z_975 * coefs_raw[, "Std. Error"],
    Upper_95  = coefs_raw[, "Estimate"] + z_975 * coefs_raw[, "Std. Error"],
    Lower_75  = coefs_raw[, "Estimate"] - z_875 * coefs_raw[, "Std. Error"],
    Upper_75  = coefs_raw[, "Estimate"] + z_875 * coefs_raw[, "Std. Error"]
  )
  rownames(coefs_log) <- rownames(coefs_raw)
  
  cat("\n### Log-Scale Estimates and Wald Confidence Intervals ###\n",
      file = summary_file, append = TRUE)
  capture.output(print(coefs_log), file = summary_file, append = TRUE)
  
  # Exponentiate for original scale
  coefs_exp <- exp(coefs_log)
  cat("\n### Original-Scale (Exponentiated) Estimates and Wald Confidence Intervals ###\n",
      file = summary_file, append = TRUE)
  capture.output(print(coefs_exp), file = summary_file, append = TRUE)
  
  # Append results to CSV if it isn't the RE-only model
  if (!grepl("RE-Only", model_name, ignore.case = TRUE)) {
    capture_fe_results(
      model,
      eco_function,
      model_name,
      country_name,
      coefs_log,
      coefs_exp,
      coefs_raw
    )
  }
}

###############################################################################
# Main Script
###############################################################################
# Construct file path dynamically
csv_path <- file.path(base_dir, "marrs_acoustics/data/results/functions", paste0(eco_function, ".csv"))
cat("Reading data from:", csv_path, "\n")
data <- read.csv(csv_path)
head(data)

# Exclude Kenya if graze_count
if (eco_function == "graze_count") {
  data <- subset(data, country != "kenya")
  cat("Kenya excluded from analysis for graze_count.\n")
}

# Get unique countries
countries <- unique(data$country)

# Paths for outputs
summary_dir <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/summary_outputs")
residual_dir <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection",
                          eco_function, paste0(eco_function, "_by_country"))
histogram_dir <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/histograms/histograms_by_country",
                           eco_function)
summary_file <- file.path(summary_dir, paste0(eco_function, "_summary_by_country.txt"))

# Ensure directories exist
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(residual_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(histogram_dir, recursive = TRUE, showWarnings = FALSE)

# Empty the summary file (if it exists)
cat("", file = summary_file)

###### Loop through each country ######
for (cntry in countries) {
  message(paste("Running analysis for:", cntry))
  
  # Subset country data
  country_data <- subset(data, data$country == cntry)
  
  if (nrow(country_data) == 0) {
    message(paste("No data for:", cntry))
    next
  }
  
  # Plot and save histogram
  hist_path <- file.path(histogram_dir, paste0(eco_function, "_", cntry, "_histogram.png"))
  png(hist_path)
  hist_title <- paste("Histogram of", eco_function, "in", cntry)
  hist(country_data$count,
       main = hist_title,
       xlab = paste("Count per day of", eco_function),
       col = "lightblue",
       border = "black")
  dev.off()
  
  # RE-Only Model
  re_only_model <- glmer.nb(
    count ~ 1 +
      (1 | site) +
      (1 | date),
    data = country_data
  )
  
  # Full Model with Treatment
  treatment_model <- glmer.nb(
    count ~ treatment +
      (1 | site) +
      (1 | date),
    data = country_data
  )
  
  # Print summaries to the big text file
  cat(paste0("####################### COUNTRY: ", toupper(cntry), " #######################\n"),
      file = summary_file, append = TRUE)
  print_model_results(re_only_model, "RE-Only Model", cntry)
  print_model_results(treatment_model, "Full Model with Treatment", cntry)
  
  # Residual Diagnostics for Treatment Model
  treatment_residuals <- residuals(treatment_model, type = "pearson")
  
  # Plot residuals vs fitted
  residual_vs_fitted_path <- file.path(residual_dir, paste0(cntry, "_residuals_vs_fitted.png"))
  png(residual_vs_fitted_path)
  plot(fitted(treatment_model), treatment_residuals,
       main = paste("Residuals vs Fitted for", eco_function, "in", cntry),
       xlab = "Fitted Values", ylab = "Residuals", col = "blue")
  abline(h = 0, col = "red", lwd = 2)
  dev.off()
  
  # Q-Q plot
  qq_plot_path <- file.path(residual_dir, paste0(cntry, "_qq_plot.png"))
  png(qq_plot_path)
  qqnorm(treatment_residuals,
         main = paste("Q-Q Plot of Residuals for", eco_function, "in", cntry))
  qqline(treatment_residuals, col = "red")
  dev.off()
}

cat("\nDone. Fixed-effects results appended to country_combined_results.csv.\n")
