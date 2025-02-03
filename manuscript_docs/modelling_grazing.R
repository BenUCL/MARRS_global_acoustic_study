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
eco_function <- "graze_count"
valid_functions <- c("graze_count", "snaps_count")
if (!(eco_function %in% valid_functions)) {
  stop("Invalid eco_function. Please set it to one of: ", paste(valid_functions, collapse = ", "))
}

# Path to the combined CSV (same file used in the other scripts)
combined_results_csv <- "/home/bwilliams/ucl_projects/marrs_acoustics/data/results/functions/stats/summary_outputs/combined_results.csv"

# Helper function to append FE results to CSV
capture_fe_results <- function(model, eco_fun, model_name, coefs_log, coefs_exp, coefs_raw) {
  terms <- rownames(coefs_log)
  
  # Get p-values if present
  pvals <- if ("Pr(>|z|)" %in% colnames(coefs_raw)) {
    coefs_raw[, "Pr(>|z|)"]
  } else if ("Pr(>|t|)" %in% colnames(coefs_raw)) {
    coefs_raw[, "Pr(>|t|)"]
  } else {
    rep(NA, length(terms))
  }
  
  # Prepare log-scale rows
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
  
  # Prepare exponentiated rows
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
  
  # Combine rows
  results_to_append <- rbind(df_log, df_exp)
  
  # Write out (append if file exists)
  if (!file.exists(combined_results_csv)) {
    write.csv(results_to_append, combined_results_csv, row.names = FALSE)
  } else {
    write.table(results_to_append, combined_results_csv, sep = ",", 
                row.names = FALSE, col.names = FALSE, append = TRUE)
  }
}

# Construct file path dynamically
csv_path <- file.path(base_dir, "marrs_acoustics/data/results/functions", paste0(eco_function, ".csv"))
print(csv_path)

# Read the data
data <- read.csv(csv_path)
head(data)

# Exclude Kenya if eco_function is 'graze_count'
if (eco_function == "graze_count") {
  data <- subset(data, country != "kenya")
  print("Kenya excluded from analysis for graze_count as no grazing present.")
}

# Plot and save histogram
output_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/histograms", paste0(eco_function, ".png"))
print(paste("Saving histogram to:", output_path))
png(output_path)
hist_title <- paste("Histogram of", eco_function)
hist(data$count,
     main = hist_title,
     xlab = paste("Count per day of", eco_function),
     col = "lightblue",
     border = "black")
dev.off()

###### Define a helper function to print and optionally capture FE results ######
print_model_results <- function(model, model_name) {
  # 1. Print model summary
  cat("\n###", model_name, "Summary ###\n")
  print(summary(model))
  
  # 2. Extract fixed effects
  coefs_raw <- summary(model)$coefficients
  
  # 3. z-values for 95% and 75% CIs
  z_975 <- qnorm(0.975)
  z_875 <- qnorm(0.875)
  
  # 4. Compute log-scale CI bounds (Wald intervals)
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
  
  # 5. Exponentiate (original scale)
  coefs_exp <- exp(coefs_log)
  rownames(coefs_exp) <- rownames(coefs_log)
  
  cat("\n### Original-Scale (Exponentiated) Estimates and Wald Confidence Intervals ###\n")
  print(coefs_exp)
  
  # 6. If it's a fixed-effects model, capture results (skip if RE-only)
  #    (Check if "RE-Only" is not in the name)
  if (!grepl("RE-Only", model_name, ignore.case = TRUE)) {
    capture_fe_results(model, eco_function, model_name, coefs_log, coefs_exp, coefs_raw)
  }
}

###### Start fitting models! ######
print("Unique entries in the country column:")
print(unique(data$country))

# RE-Only Model
re_only_model <- glmer.nb(
  count ~ 1 +
    (1 | country) +
    (1 | country:site) +
    (1 | country:date),
  data = data
)

# Path for summary
summary_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/summary_outputs",
                          paste0(eco_function, "_summary.txt"))

# Save RE-Only Model Summary
sink(summary_path)
cat(paste0("################ RE-ONLY MODEL SUMMARY (", eco_function, ") ################\n"))
print_model_results(re_only_model, "RE-Only Model")
sink()

# Fit Model with Treatment (FE)
treatment_model <- glmer.nb(
  count ~ treatment +
    (1 | country) +
    (1 | country:site) +
    (1 | country:date),
  data = data
)
posthoc_results <- emmeans(treatment_model, pairwise ~ treatment, adjust = "tukey")

# Append Treatment Model Summary
sink(summary_path, append = TRUE)
cat(paste0("\n################ FULL MODEL SUMMARY (", eco_function, ") ################\n"))
print_model_results(treatment_model, "Full Model with Treatment")

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
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
log_lmm_model <- lmer(
  log_count ~ treatment +
    (1 | country) +
    (1 | country:site) +
    (1 | country:date),
  data = data
)
posthoc_results_log <- emmeans(log_lmm_model, pairwise ~ treatment, adjust = "none")

sink(summary_path, append = TRUE)
cat("\n################ LOG-TRANSFORMED LMM ANALYSIS ################\n")
cat("### Full Model Summary ###\n\n")
print_model_results(log_lmm_model, "Log-Transformed LMM")

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
print(posthoc_results_log$contrasts)
sink()

###### Residual Diagnostics for Log-Transformed Model ######
log_residuals <- residuals(log_lmm_model)
log_fitted <- fitted(log_lmm_model)

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

###### Fit and Evaluate Country & Treatment Interaction ######
interaction_model <- glmer.nb(
  count ~ treatment * country +
    (1 | country) +
    (1 | country:site) +
    (1 | country:date),
  data = data
)
posthoc_results_interaction <- emmeans(interaction_model, pairwise ~ treatment, adjust = "none")

sink(summary_path, append = TRUE)
cat("\n################ TREATMENT:COUNTRY INTERACTION MODEL ################\n")
print_model_results(interaction_model, "Interaction Model")

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
print(posthoc_results_interaction$contrasts)
sink()

###### Residuals for Interaction Model ######
interaction_residuals <- residuals(interaction_model, type = "pearson")
interaction_fitted <- fitted(interaction_model)
data$interaction_residuals <- interaction_residuals

interaction_outliers_path <- file.path(outliers_dir, paste0(eco_function, "_interaction_fe_outliers.csv"))
write.csv(data[abs(interaction_residuals) > 3, ], interaction_outliers_path, row.names = FALSE)
cat(paste("Saved Interaction Model outliers to:", interaction_outliers_path, "\n"))

# Plot residuals vs fitted (interaction model)
interaction_residuals_vs_fitted_path <- file.path(outliers_dir, paste0(eco_function, "_residuals_vs_fitted_interaction.png"))
png(interaction_residuals_vs_fitted_path)
plot(interaction_fitted, interaction_residuals,
     main = "Residuals vs Fitted (Interaction Model)",
     xlab = "Fitted Values", ylab = "Residuals", col = "blue")
abline(h = 0, col = "red", lwd = 2)
dev.off()

# Q-Q plot for interaction model
interaction_qq_plot_path <- file.path(outliers_dir, paste0(eco_function, "_qq_plot_interaction.png"))
png(interaction_qq_plot_path)
qqnorm(interaction_residuals, main = "Q-Q Plot of Interaction Model Residuals")
qqline(interaction_residuals, col = "red")
dev.off()
