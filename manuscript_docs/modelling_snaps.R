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

# Global path for combined CSV results
combined_results_csv <- "/home/bwilliams/ucl_projects/marrs_acoustics/data/results/functions/stats/summary_outputs/combined_results.csv"

# Helper to capture and append fixed-effects results
capture_fe_results <- function(model, eco_fun, model_name, coefs_log, coefs_exp, coefs_raw) {
  terms <- rownames(coefs_log)
  
  # Pull p-values if available
  pvals <- if ("Pr(>|z|)" %in% colnames(coefs_raw)) {
    coefs_raw[, "Pr(>|z|)"]
  } else if ("Pr(>|t|)" %in% colnames(coefs_raw)) {
    coefs_raw[, "Pr(>|t|)"]
  } else {
    rep(NA, length(terms))
  }
  
  # Log-scale rows
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
  
  # Exponentiated rows
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
  
  # Combine
  results_to_append <- rbind(df_log, df_exp)
  
  # Write out (append if exists)
  if (!file.exists(combined_results_csv)) {
    write.csv(results_to_append, combined_results_csv, row.names = FALSE)
  } else {
    write.table(results_to_append, combined_results_csv, sep = ",", row.names = FALSE,
                col.names = FALSE, append = TRUE)
  }
}

# Data path
csv_path <- file.path(base_dir, "marrs_acoustics/data/results/functions", paste0(eco_function, ".csv"))
print(csv_path)
data <- read.csv(csv_path)
head(data)

# Exclude Kenya if graze_count
if (eco_function == "graze_count") {
  data <- subset(data, country != "kenya")
  print("Kenya excluded from analysis for graze_count as no grazing present.")
}

# Plot histogram
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

###### Define helper function for intervals & optional CSV capture ######
print_model_results <- function(model, model_name) {
  cat("\n###", model_name, "Summary ###\n")
  print(summary(model))
  
  coefs <- summary(model)$coefficients
  
  # Z-scores
  z_975 <- qnorm(0.975)
  z_875 <- qnorm(0.875)
  
  # Log-scale intervals
  coefs_log <- data.frame(
    Estimate  = coefs[, "Estimate"],
    Lower_95  = coefs[, "Estimate"] - z_975 * coefs[, "Std. Error"],
    Upper_95  = coefs[, "Estimate"] + z_975 * coefs[, "Std. Error"],
    Lower_75  = coefs[, "Estimate"] - z_875 * coefs[, "Std. Error"],
    Upper_75  = coefs[, "Estimate"] + z_875 * coefs[, "Std. Error"]
  )
  rownames(coefs_log) <- rownames(coefs)
  
  cat("\n### Log-Scale Estimates and Wald Confidence Intervals ###\n")
  print(coefs_log)
  
  # Exponentiate
  coefs_exp <- exp(coefs_log)
  cat("\n### Original-Scale (Exponentiated) Estimates and Wald Confidence Intervals ###\n")
  print(coefs_exp)
  
  # Capture FE results if not an RE-Only model
  if (!grepl("RE-Only", model_name, ignore.case = TRUE)) {
    capture_fe_results(model, eco_function, model_name, coefs_log, coefs_exp, coefs)
  }
}

###### Start Fitting Models ######
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
sink(summary_path)
cat(paste0("################ RE-ONLY MODEL SUMMARY (", eco_function, ") ################\n"))
print_model_results(re_only_model, "RE-Only Model")
sink()

# Full Model with Treatment
treatment_model <- glmer.nb(
  count ~ treatment +
    (1 | country) +
    (1 | country:site) +
    (1 | country:date),
  data = data
)
posthoc_results <- emmeans(treatment_model, pairwise ~ treatment, adjust = "none")

sink(summary_path, append = TRUE)
cat(paste0("\n################ FULL MODEL SUMMARY (", eco_function, ") ################\n"))
print_model_results(treatment_model, "BASE_MODEL")

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
print(posthoc_results$contrasts)
sink()

###### Residual Diagnostics ######
treatment_residuals <- residuals(treatment_model, type = "pearson")
data$treatment_residuals <- treatment_residuals

outliers_dir <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function)
dir.create(outliers_dir, recursive = TRUE, showWarnings = FALSE)
outliers_path <- file.path(outliers_dir, paste0(eco_function, "_fe_outliers.csv"))
write.csv(data[abs(treatment_residuals) > 3, ], outliers_path, row.names = FALSE)

# Plot residuals vs fitted
residual_plot_path <- file.path(outliers_dir, paste0(eco_function, "_fe_residuals_vs_fitted.png"))
png(residual_plot_path)
plot(fitted(treatment_model), treatment_residuals,
     main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals", col = "blue")
abline(h = 0, col = "red")
dev.off()

# QQ plot
qq_plot_path <- file.path(outliers_dir, paste0(eco_function, "_fe_qq_plot.png"))
png(qq_plot_path)
qqnorm(treatment_residuals, main = "Q-Q Plot of Treatment Model Residuals")
qqline(treatment_residuals, col = "red")
dev.off()

###### Log-Transformed LMM ######
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
cat("\n################ LOG-TRANSFORMED LMM ################\n")
print_model_results(log_lmm_model, "Log-Transformed LMM")

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
print(posthoc_results_log$contrasts)
sink()
