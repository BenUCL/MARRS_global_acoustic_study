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

# Path to store combined results (appended each time models are run)
combined_results_csv <- "/home/bwilliams/ucl_projects/marrs_acoustics/data/results/functions/stats/summary_outputs/combined_results.csv"

# Set ecological function
eco_function <- "settlement_cuescape"
valid_functions <- c("snaps_count", "graze_count", "phonic_richness", "settlement_cuescape")
if (!(eco_function %in% valid_functions)) {
  stop("Invalid eco_function. Please set it to one of: ", paste(valid_functions, collapse = ", "))
}

# Read data
csv_path <- file.path(base_dir, "marrs_acoustics/data/results/functions", paste0(eco_function, ".csv"))
data <- read.csv(csv_path)

# Helper to capture and append FE results
capture_fe_results <- function(model, eco_fun, model_group, model_name, coefs_log, coefs_exp, coefs_raw) {
  # coefs_log is the log-scale table
  # coefs_exp is the exponentiated table
  # coefs_raw is summary(model)$coefficients (for p-values)
  
  terms <- rownames(coefs_log)
  
  # Extract p-values if present
  pvals <- if ("Pr(>|z|)" %in% colnames(coefs_raw)) {
    coefs_raw[, "Pr(>|z|)"]
  } else {
    rep(NA, length(terms))
  }
  
  # Build log-scale rows
  df_log <- data.frame(
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
  
  df_exp <- data.frame(
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
  if (!file.exists(combined_results_csv)) {
    write.csv(results_to_append, combined_results_csv, row.names = FALSE)
  } else {
    write.table(results_to_append, combined_results_csv, sep = ",", row.names = FALSE,
                col.names = FALSE, append = TRUE)
  }
}

# Function to compute and print model results
print_model_results <- function(model, model_name, model_group, eco_fun) {
  cat("\n###", model_name, "Summary ###\n")
  print(summary(model))
  
  # Extract fixed effects from model summary
  coefs_raw <- summary(model)$coefficients
  
  # z-values
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
  
  cat("\n### Log-Scale Estimates and Wald Confidence Intervals ###\n")
  print(coefs_log)
  
  # Exponentiated intervals
  coefs_exp <- exp(coefs_log)
  rownames(coefs_exp) <- rownames(coefs_log)
  
  cat("\n### Original-Scale (Exponentiated) Estimates and Wald Confidence Intervals ###\n")
  print(coefs_exp)
  
  # If it's a fixed-effects model, capture & append results
  if (grepl("BASE_MODEL|FE Model", model_name, ignore.case = TRUE)) {
    capture_fe_results(model, eco_fun, model_group, model_name, coefs_log, coefs_exp, coefs_raw)
  }
}

###### Start fitting models! ######
# RE-Only Model
re_only_model <- glmer.nb(
  count ~ offset(log(max_poss_count)) + 
    (1 | country) + (1 | country:site) + (1 | country:date),
  data = data
)

# Full Model with Treatment
treatment_model <- glmer.nb(
  count ~ treatment + offset(log(max_poss_count)) +
    (1 | country) + (1 | country:site),# + (1 | country:date), # DROP DATE
  data = data
)

# Path for summary output
summary_path <- file.path(
  base_dir,
  "marrs_acoustics/data/results/functions/stats/summary_outputs",
  paste0(eco_function, "_summary.txt")
)
sink(summary_path)
cat("################ RAW DATA MODELS ################\n\n")
print_model_results(re_only_model, "RE-Only Model", "RAW DATA MODELS", eco_function)
print_model_results(treatment_model, "BASE_MODEL", "RAW DATA MODELS", eco_function)
cat("\n################ POST-HOC TEST RESULTS ################\n\n")
posthoc_results <- emmeans(treatment_model, pairwise ~ treatment, adjust = "none")
print(posthoc_results$contrasts)
sink()

###### Residual Diagnostics for the Treatment Model ######
treatment_residuals <- residuals(treatment_model, type = "pearson")
data$treatment_residuals <- treatment_residuals

# Identify outliers
outliers_dir <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function)
dir.create(outliers_dir, recursive = TRUE, showWarnings = FALSE)
outliers_path <- file.path(outliers_dir, paste0(eco_function, "_fe_outliers.csv"))
write.csv(data[abs(treatment_residuals) > 3, ], outliers_path, row.names = FALSE)
cat(paste("Saved Treatment Model outliers to:", outliers_path, "\n"))

# Plot residuals vs fitted
residual_plot_path <- file.path(outliers_dir, paste0(eco_function, "_fe_residuals_vs_fitted.png"))
png(residual_plot_path)
plot(fitted(treatment_model), residuals(treatment_model),
     main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals", col = "blue")
abline(h = 0, col = "red", lwd = 2)
dev.off()

# QQ plot
qq_plot_path <- file.path(outliers_dir, paste0(eco_function, "_fe_qq_plot.png"))
png(qq_plot_path)
qqnorm(residuals(treatment_model), main = "Q-Q Plot of Treatment Model Residuals")
qqline(residuals(treatment_model), col = "red")
dev.off()

###### Fit and Evaluate Log-Transformed LMM ######
data$log_count <- log(data$count + 1)

# Log RE-Only Model
log_re_only_model <- lmer(
  log_count ~ offset(log(max_poss_count)) + 
    (1 | country) + (1 | country:site) + (1 | country:date),
  data = data
)

# Log Full Model with Treatment
log_treatment_model <- lmer(
  log_count ~ treatment + offset(log(max_poss_count)) + 
    (1 | country) + (1 | country:site) + (1 | country:date),
  data = data
)

sink(summary_path, append = TRUE)
cat("\n################ LOG-TRANSFORMED DATA MODELS ################\n\n")
print_model_results(log_re_only_model, "Log RE-Only Model", "LOG-TRANSFORMED DATA MODELS", eco_function)
print_model_results(log_treatment_model, "Log Full Model with Treatment", "LOG-TRANSFORMED DATA MODELS", eco_function)

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
posthoc_results_log <- emmeans(log_treatment_model, pairwise ~ treatment, adjust = "none")
print(posthoc_results_log$contrasts)
sink()

###### Residual Diagnostics for Log-Transformed Model ######
log_residuals <- residuals(log_treatment_model)
log_fitted <- fitted(log_treatment_model)

log_outliers_path <- file.path(outliers_dir, paste0(eco_function, "_transformed_fe_outliers_log.csv"))
write.csv(data[abs(log_residuals) > 3, ], log_outliers_path, row.names = FALSE)
cat(paste("Saved Log-Transformed Model outliers to:", log_outliers_path, "\n"))

# Plot residuals vs fitted
log_residuals_vs_fitted_path <- file.path(outliers_dir, paste0(eco_function, "_residuals_vs_fitted_log.png"))
png(log_residuals_vs_fitted_path)
plot(log_fitted, log_residuals,
     main = "Residuals vs Fitted (Log-Transformed)",
     xlab = "Fitted Values", ylab = "Residuals", col = "blue")
abline(h = 0, col = "red", lwd = 2)
dev.off()

# Q-Q plot
log_qq_plot_path <- file.path(outliers_dir, paste0(eco_function, "_qq_plot_log.png"))
png(log_qq_plot_path)
qqnorm(log_residuals, main = "Q-Q Plot of Log-Transformed Model Residuals")
qqline(log_residuals, col = "red")
dev.off()






###### Residual Diagnostics for drop outlier model ######
# Ensure the columns for joining are consistent
# Load the extreme residuals (outliers) from the saved CSV file 
outliers <- read.csv(outliers_path)

# Ensure the columns for joining are consistent
outliers$key <- paste(outliers$country, outliers$site, outliers$date, sep = "_")
data$key <- paste(data$country, data$site, data$date, sep = "_")

# Print the first few rows and number of rows of the filtered data
cat("First few rows of filtered data:\n")
print(head(data_no_outliers))
cat("\nNumber of rows in filtered data: ", nrow(data_no_outliers), "\n")

# Drop the key column to clean up
data_no_outliers$key <- NULL
data$key <- NULL

# Proceed with the models as before
# RE only
re_drop_outliers <- glmer.nb(
  count ~ offset(log(max_poss_count)) + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data_no_outliers
)

# FE model
fe_drop_outliers <- glmer.nb(
  count ~ treatment + offset(log(max_poss_count)) + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data_no_outliers
)

sink(summary_path, append = TRUE)
cat("\n################ MODELS AFTER DROPPING OUTLIERS ################\n\n")
print_model_results(re_drop_outliers, "RE Model (Drop Outliers)", "MODELS AFTER DROPPING OUTLIERS", eco_function)
print_model_results(fe_drop_outliers, "FE Model (Drop Outliers)", "MODELS AFTER DROPPING OUTLIERS", eco_function)

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
posthoc_results_drop_outliers <- emmeans(fe_drop_outliers, pairwise ~ treatment, adjust = "none")
print(posthoc_results_drop_outliers$contrasts)
sink()

# Residual diagnostics for modes with drop outliers
drop_outliers_residuals <- residuals(fe_drop_outliers)
drop_outliers_residuals_outliers <- data[abs(drop_outliers_residuals) > 3, ]

# Log QQ Plot
drop_outliers_qq_plot_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_qq_drop_outliers.png"))
png(drop_outliers_qq_plot_path)
qqnorm(drop_outliers_residuals, main = "Q-Q Plot (Drop Outliers)")
qqline(drop_outliers_residuals, col = "red")
dev.off()

# Log Residuals vs Fitted
drop_outliers_res_vs_fit_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_residuals_vs_fitted_drop_outliers.png"))
png(drop_outliers_res_vs_fit_path)
plot(fitted(fe_drop_outliers), drop_outliers_residuals, main = "Residuals vs Fitted (Drop Outliers)", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
dev.off()

###### Drop Date Random Effect ######
# RE-Only Model without the date random effect
re_no_date_model <- glmer.nb(
  count ~ offset(log(max_poss_count)) +
    (1 | country) + (1 | country:site),
  data = data
)

# Full Model with Treatment without the date random effect
fe_no_date_model <- glmer.nb(
  count ~ treatment + offset(log(max_poss_count)) +
    (1 | country) + (1 | country:site),
  data = data
)

sink(summary_path, append = TRUE)
cat("\n################ MODELS DROPPING DATE RANDOM EFFECT ################\n\n")
print_model_results(re_no_date_model, "RE Model (Drop Date)", "MODELS DROPPING DATE RANDOM EFFECT", eco_function)
print_model_results(fe_no_date_model, "FINAL_MODEL", "MODELS DROPPING DATE RANDOM EFFECT", eco_function)

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
posthoc_results_no_date <- emmeans(fe_no_date_model, pairwise ~ treatment, adjust = "none")
print(posthoc_results_no_date$contrasts)
sink()

# Residual diagnostics for modes with drop date effect
drop_date_residuals <- residuals(fe_no_date_model)
drop_date_residuals_outliers <- data[abs(drop_date_residuals) > 3, ]

# Log QQ Plot
drop_date_qq_plot_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_qq_drop_date.png"))
png(drop_date_qq_plot_path)
qqnorm(drop_date_residuals, main = "Q-Q Plot (Drop Date)")
qqline(drop_date_residuals, col = "red")
dev.off()

# Log Residuals vs Fitted
drop_date_res_vs_fit_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_residuals_vs_fitted_drop_date.png"))
png(drop_date_res_vs_fit_path)
plot(fitted(fe_no_date_model),drop_date_residuals, main = "Residuals vs Fitted (Drop Date)", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
dev.off()
