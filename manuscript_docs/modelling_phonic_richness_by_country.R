# Required Libraries
if (!require("lme4")) install.packages("lme4", repos = "http://cran.rstudio.com/")
library(lme4)

# Check environment variable
base_dir <- Sys.getenv("BASE_DIR")
if (base_dir == "") {
  stop("BASE_DIR is not set. Please set it in your environment variables.")
}

# Ecological function
eco_function <- "phonic_richness"
valid_functions <- c("graze_count", "snaps_count", "phonic_richness")
if (!(eco_function %in% valid_functions)) {
  stop("Invalid eco_function. Please set it to one of: ", paste(valid_functions, collapse = ", "))
}

# Path for the new per-country combined CSV
country_combined_csv <- file.path(
  base_dir,
  "marrs_acoustics/data/results/functions/stats/summary_outputs",
  "country_combined_results.csv"
)

############################################
# Helper to capture & append fixed-effects results
############################################
capture_fe_results <- function(model, eco_fun, model_name, country_name) {
  coefs_raw <- summary(model)$coefficients
  
  # We only proceed if we actually have coefficients
  if (is.null(coefs_raw) || ncol(coefs_raw) < 2) return()
  
  # Compute z-scores for intervals
  z_975 <- qnorm(0.975)
  z_875 <- qnorm(0.875)
  
  # Build log-scale intervals
  coefs_log <- data.frame(
    Estimate  = coefs_raw[, "Estimate"],
    Lower_95  = coefs_raw[, "Estimate"] - z_975 * coefs_raw[, "Std. Error"],
    Upper_95  = coefs_raw[, "Estimate"] + z_975 * coefs_raw[, "Std. Error"],
    Lower_75  = coefs_raw[, "Estimate"] - z_875 * coefs_raw[, "Std. Error"],
    Upper_75  = coefs_raw[, "Estimate"] + z_875 * coefs_raw[, "Std. Error"]
  )
  rownames(coefs_log) <- rownames(coefs_raw)
  
  # Exponentiated intervals
  coefs_exp <- exp(coefs_log)
  
  # Get p-values if available
  if ("Pr(>|z|)" %in% colnames(coefs_raw)) {
    pvals <- coefs_raw[, "Pr(>|z|)"]
  } else if ("Pr(>|t|)" %in% colnames(coefs_raw)) {
    pvals <- coefs_raw[, "Pr(>|t|)"]
  } else {
    pvals <- rep(NA, nrow(coefs_raw))
  }
  
  # Build the "log-scale" rows
  df_log <- data.frame(
    country = country_name,
    eco_function = eco_fun,
    model_name = model_name,
    scale = "Log-Scale",
    term = rownames(coefs_log),
    Estimate = coefs_log$Estimate,
    Lower_95 = coefs_log$Lower_95,
    Upper_95 = coefs_log$Upper_95,
    Lower_75 = coefs_log$Lower_75,
    Upper_75 = coefs_log$Upper_75,
    p_value = pvals
  )
  
  # Build the "exponentiated" rows
  df_exp <- data.frame(
    country = country_name,
    eco_function = eco_fun,
    model_name = model_name,
    scale = "Original-Scale",
    term = rownames(coefs_exp),
    Estimate = coefs_exp$Estimate,
    Lower_95 = coefs_exp$Lower_95,
    Upper_95 = coefs_exp$Upper_95,
    Lower_75 = coefs_exp$Lower_75,
    Upper_75 = coefs_exp$Upper_75,
    p_value = pvals
  )
  
  # Combine rows
  results_to_append <- rbind(df_log, df_exp)
  
  # Write out (append if file exists)
  if (!file.exists(country_combined_csv)) {
    write.csv(results_to_append, country_combined_csv, row.names = FALSE)
  } else {
    write.table(results_to_append, country_combined_csv, sep = ",",
                row.names = FALSE, col.names = FALSE, append = TRUE)
  }
}

############################################
# Wrapper to print + optionally capture FE
############################################
print_and_capture_fe <- function(model, model_label, country_name, summary_file_path) {
  # Print model summary to the big text file
  cat(paste0("\n### ", model_label, " (", toupper(country_name), ") ###\n"),
      file = summary_file_path, append = TRUE)
  capture.output(print(summary(model)), file = summary_file_path, append = TRUE)
  
  # Only capture FE if not "RE-Only" in name
  if (!grepl("RE-Only", model_label, ignore.case = TRUE)) {
    capture_fe_results(model, eco_function, model_label, country_name)
  }
}

############################################
# Main script logic
############################################
# Read data
csv_path <- file.path(base_dir, "marrs_acoustics/data/results/functions", paste0(eco_function, ".csv"))
data <- read.csv(csv_path)
head(data)

# Exclude Kenya for graze_count
if (eco_function == "graze_count") {
  data <- subset(data, country != "kenya")
  message("Kenya excluded from analysis for graze_count.")
}

# Unique countries
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

# Empty the summary file
cat("", file = summary_file)

# Loop through each country
for (cntry in countries) {
  message(paste("Running analysis for:", cntry))
  
  # Subset data
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
  
  # 1. RE-Only Model
  re_only_model <- glmer.nb(
    count ~ 1 + 
      (1 | site) + 
      (1 | date),
    data = country_data
  )
  
  # 2. Full Model with Treatment
  treatment_model <- glmer.nb(
    count ~ treatment + 
      (1 | site) + 
      (1 | date),
    data = country_data
  )
  
  # Append model summaries to the big text file
  cat(paste0("####################### COUNTRY: ", toupper(cntry), " #######################\n"),
      file = summary_file, append = TRUE)
  
  print_and_capture_fe(re_only_model, "RE-Only Model", cntry, summary_file)
  print_and_capture_fe(treatment_model, "Full Model with Treatment", cntry, summary_file)
  
  # Residual Diagnostics for Treatment Model
  treatment_residuals <- residuals(treatment_model, type = "pearson")
  
  # Plot residuals vs fitted
  residual_vs_fitted_path <- file.path(residual_dir, paste0(cntry, "_residuals_vs_fitted.png"))
  png(residual_vs_fitted_path)
  plot(fitted(treatment_model), treatment_residuals,
       main = paste("Residuals vs Fitted for", eco_function, "in", cntry),
       xlab = "Fitted Values",
       ylab = "Residuals",
       col = "blue")
  abline(h = 0, col = "red", lwd = 2)
  dev.off()
  
  # Q-Q plot
  qq_plot_path <- file.path(residual_dir, paste0(cntry, "_qq_plot.png"))
  png(qq_plot_path)
  qqnorm(treatment_residuals, main = paste("Q-Q Plot of Residuals for", eco_function, "in", cntry))
  qqline(treatment_residuals, col = "red")
  dev.off()
}

########### Drop Date #####################
for (cntry in countries) {
  message(paste("Running analysis (drop_date) for:", cntry))
  country_data <- subset(data, data$country == cntry)
  
  if (nrow(country_data) == 0) {
    message(paste("No data for:", cntry))
    next
  }
  
  # Plot and save histogram
  hist_path <- file.path(histogram_dir, paste0(eco_function, "_", cntry, "_histogram_drop_date.png"))
  png(hist_path)
  hist_title <- paste("Histogram of", eco_function, "in", cntry, "(drop_date)")
  hist(country_data$count,
       main = hist_title,
       xlab = paste("Count per day of", eco_function),
       col = "lightblue",
       border = "black")
  dev.off()
  
  # 3. RE-Only Model, no date
  re_only_model_dd <- glmer.nb(
    count ~ 1 + 
      (1 | site),
    data = country_data
  )
  
  # 4. Full Model w/ Treatment, no date
  treatment_model_dd <- glmer.nb(
    count ~ treatment + 
      (1 | site),
    data = country_data
  )
  
  cat(paste0("####################### COUNTRY: ", toupper(cntry), " (drop_date) #######################\n"),
      file = summary_file, append = TRUE)
  
  print_and_capture_fe(re_only_model_dd, "RE-Only Model (drop_date)", cntry, summary_file)
  print_and_capture_fe(treatment_model_dd, "Full Model with Treatment (drop_date)", cntry, summary_file)
  
  # Residual Diagnostics
  treatment_residuals_dd <- residuals(treatment_model_dd, type = "pearson")
  
  # Plot residuals vs fitted
  residual_vs_fitted_path_dd <- file.path(residual_dir, paste0(cntry, "_residuals_vs_fitted_drop_date.png"))
  png(residual_vs_fitted_path_dd)
  plot(fitted(treatment_model_dd), treatment_residuals_dd,
       main = paste("Residuals vs Fitted for", eco_function, "in", cntry, "(drop_date)"),
       xlab = "Fitted Values",
       ylab = "Residuals",
       col = "blue")
  abline(h = 0, col = "red", lwd = 2)
  dev.off()
  
  # Q-Q plot
  qq_plot_path_dd <- file.path(residual_dir, paste0(cntry, "_qq_plot_drop_date.png"))
  png(qq_plot_path_dd)
  qqnorm(treatment_residuals_dd, main = paste("Q-Q Plot of Residuals for", eco_function, "in", cntry, "(drop_date)"))
  qqline(treatment_residuals_dd, col = "red")
  dev.off()
}

message("Done. Any FE (non-'RE-Only') results have been appended to country_combined_results.csv.")
