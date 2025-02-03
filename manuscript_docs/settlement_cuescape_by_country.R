# Required libraries
if (!require("lme4")) install.packages("lme4", repos = "http://cran.rstudio.com/")
library(lme4)
if (!require("emmeans")) install.packages("emmeans", repos = "http://cran.rstudio.com/")
library(emmeans)

# Check if the environment variable is set
base_dir <- Sys.getenv("BASE_DIR")
if (base_dir == "") {
  stop("BASE_DIR is not set. Please set it in your environment variables.")
}

# Ecological function
eco_function <- "settlement_cuescape"
valid_functions <- c("graze_count", "snaps_count", "settlement_cuescape")
if (!(eco_function %in% valid_functions)) {
  stop("Invalid eco_function. Please set it to one of: ", paste(valid_functions, collapse = ", "))
}

# Path to the new per-country combined CSV
country_combined_csv <- file.path(
  "/home/bwilliams/ucl_projects/marrs_acoustics/data/results/functions/stats/summary_outputs",
  "country_combined_results.csv"
)

# Helper to capture and append FE results
capture_fe_results <- function(model, eco_fun, model_name, coefs_log, coefs_exp, coefs_raw, country_name) {
  # Extract row names (terms)
  terms <- rownames(coefs_log)
  
  # Pull p-values if present
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
  
  # Combine
  results_to_append <- rbind(df_log, df_exp)
  
  # Write out (append if exists)
  if (!file.exists(country_combined_csv)) {
    write.csv(results_to_append, country_combined_csv, row.names = FALSE)
  } else {
    write.table(results_to_append, country_combined_csv, sep = ",", row.names = FALSE,
                col.names = FALSE, append = TRUE)
  }
}

# Paths and directories for output
summary_dir <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/summary_outputs")
residual_dir <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection",
                          eco_function, paste0(eco_function, "_by_country"))
histogram_dir <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/histograms/histograms_by_country",
                           eco_function)
summary_file <- file.path(summary_dir, paste0(eco_function, "_summary_by_country.txt"))

dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(residual_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(histogram_dir, recursive = TRUE, showWarnings = FALSE)

# Empty the main summary file if it already exists
cat("", file = summary_file)

###### Helper function for printing & capturing model results ######
print_model_results <- function(model, model_name, country_name) {
  # Print model summary to the main text file
  cat(paste0("\n### ", model_name, " Summary for ", toupper(country_name), " ###\n"),
      file = summary_file, append = TRUE)
  capture.output(print(summary(model)), file = summary_file, append = TRUE)
  
  # Extract fixed effects
  coefs_raw <- summary(model)$coefficients
  
  # Z-scores
  z_975 <- qnorm(0.975)
  z_875 <- qnorm(0.875)
  
  # Log-scale intervals
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
  
  # Exponentiated intervals
  coefs_exp <- exp(coefs_log)
  
  cat("\n### Original-Scale (Exponentiated) Estimates and Wald Confidence Intervals ###\n",
      file = summary_file, append = TRUE)
  capture.output(print(coefs_exp), file = summary_file, append = TRUE)
  
  # If this is not the RE-Only model, append FE results to country_combined_results.csv
  # (i.e. skip if "RE-Only" in the model name)
  if (!grepl("RE-Only", model_name, ignore.case = TRUE)) {
    capture_fe_results(model,
                       eco_function,
                       model_name,
                       coefs_log,
                       coefs_exp,
                       coefs_raw,
                       country_name)
  }
}

############################################
# MAIN SCRIPT
############################################

# Read data
csv_path <- file.path(base_dir, "marrs_acoustics/data/results/functions", paste0(eco_function, ".csv"))
data <- read.csv(csv_path)

# Exclude Kenya if needed
if (eco_function == "graze_count") {
  data <- subset(data, country != "kenya")
  message("Kenya excluded from analysis for graze_count.")
}

# Get unique countries
countries <- unique(data$country)

for (cntry in countries) {
  message(paste("Running analysis for:", cntry))
  
  country_data <- subset(data, country == cntry)
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
    count ~ offset(log(max_poss_count)) +
      (1 | site) +
      (1 | date),
    data = country_data
  )
  
  # Full Model with Treatment
  treatment_model <- glmer.nb(
    count ~ treatment + offset(log(max_poss_count)) +
      (1 | site) +
      (1 | date),
    data = country_data
  )
  
  # Summaries
  cat(paste0("\n####################### COUNTRY: ",
             toupper(cntry),
             " #######################\n"),
      file = summary_file, append = TRUE)
  
  print_model_results(re_only_model, "RE-Only Model", cntry)
  print_model_results(treatment_model, "Full Model with Treatment", cntry)
  
  # Residual Diagnostics for the Treatment Model
  treatment_residuals <- residuals(treatment_model, type = "pearson")
  residual_vs_fitted_path <- file.path(residual_dir, paste0(cntry, "_residuals_vs_fitted.png"))
  
  png(residual_vs_fitted_path)
  plot(fitted(treatment_model), treatment_residuals,
       main = paste("Residuals vs Fitted for", eco_function, "in", cntry),
       xlab = "Fitted Values",
       ylab = "Residuals",
       col = "blue")
  abline(h = 0, col = "red", lwd = 2)
  dev.off()
  
  # QQ plot
  qq_plot_path <- file.path(residual_dir, paste0(cntry, "_qq_plot.png"))
  png(qq_plot_path)
  qqnorm(treatment_residuals, main = paste("Q-Q Plot of Residuals for", eco_function, "in", cntry))
  qqline(treatment_residuals, col = "red")
  dev.off()
}

message("Done. Fixed-effects results appended to country_combined_results.csv (excluding RE-only).")
