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
eco_function <- "phonic_richness"
valid_functions <- c("snaps_count", "graze_count", "phonic_richness", "settlement_cuescape")
if (!(eco_function %in% valid_functions)) {
  stop("Invalid eco_function. Please set it to one of: ", paste(valid_functions, collapse = ", "))
}

# Same CSV file used for combined results
combined_results_csv <- "/home/bwilliams/ucl_projects/marrs_acoustics/data/results/functions/stats/summary_outputs/combined_results.csv"

# Helper to capture & append FE results to CSV
capture_fe_results <- function(model, eco_fun, model_name, coefs_log, coefs_exp, coefs_raw) {
  terms <- rownames(coefs_log)
  
  # Get p-values if available
  pvals <- if ("Pr(>|z|)" %in% colnames(coefs_raw)) {
    coefs_raw[, "Pr(>|z|)"]
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
  
  # Combine both
  results_to_append <- rbind(df_log, df_exp)
  
  # Write out (append if file exists)
  if (!file.exists(combined_results_csv)) {
    write.csv(results_to_append, combined_results_csv, row.names = FALSE)
  } else {
    write.table(results_to_append, combined_results_csv, sep = ",", row.names = FALSE,
                col.names = FALSE, append = TRUE)
  }
}

# Construct file path dynamically
csv_path <- file.path(base_dir, "marrs_acoustics/data/results/functions", paste0(eco_function, ".csv"))
print(csv_path)

data <- read.csv(csv_path)
head(data)

# Plot histogram for raw data
hist_output_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/histograms",
                              paste0(eco_function, ".png"))
png(hist_output_path)
hist_title <- paste("Histogram of", eco_function)
hist(data$count,
     main = hist_title,
     xlab = paste("Count per day of", eco_function),
     col = "lightblue",
     border = "black")
dev.off()

############### Define Model-Printing Function ###############
print_model_results <- function(model, model_name) {
  cat("\n###", model_name, "Summary ###\n")
  print(summary(model))
  
  coefs_raw <- summary(model)$coefficients
  
  # Z-values for CIs
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
  
  cat("\n### Log-Scale Estimates and Wald Confidence Intervals ###\n")
  print(coefs_log)
  
  # Exponentiated intervals
  coefs_exp <- exp(coefs_log)
  rownames(coefs_exp) <- rownames(coefs_raw)
  
  cat("\n### Original-Scale (Exponentiated) Estimates and Wald Confidence Intervals ###\n")
  print(coefs_exp)
  
  if (grepl("BASE_MODEL|DROP DATE", model_name, ignore.case = TRUE)) {
    capture_fe_results(model, eco_function, model_name, coefs_log, coefs_exp, coefs_raw)
  }
}


###### Fit Models ######
# 1. RE-Only Model
re_only_model <- glmer.nb(
  count ~ 1 +
    (1 | country) +
    (1 | country:site) +
    (1 | country:date),
  data = data
)

# 2. Full Model with Treatment (FE)
treatment_model <- glmer.nb(
  count ~ treatment +
    (1 | country) +
    (1 | country:site) +
    (1 | country:date),
  data = data
)
posthoc_results <- emmeans(treatment_model, pairwise ~ treatment, adjust = "none")

# Summaries to text
summary_path <- file.path(
  base_dir,
  "marrs_acoustics/data/results/functions/stats/summary_outputs",
  paste0(eco_function, "_summary.txt")
)
sink(summary_path)

cat("################ RAW DATA MODELS ################\n\n")
print_model_results(re_only_model, "RE-Only Model")
print_model_results(treatment_model, "BASE_MODEL")

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
print(posthoc_results$contrasts)

sink()

###### Inspect Model Fit ######
treatment_residuals <- residuals(treatment_model, type = "pearson")
data$treatment_residuals <- treatment_residuals

# Identify outliers
treatment_outliers <- data[abs(treatment_residuals) > 3, ]
outliers_path <- file.path(base_dir, 
                           "marrs_acoustics/data/results/functions/stats/model_inspection",
                           eco_function,
                           paste0(eco_function, "_fe_outliers.csv"))
write.csv(treatment_outliers, outliers_path, row.names = FALSE)

# QQ plot
qq_plot_path <- file.path(base_dir,
                          "marrs_acoustics/data/results/functions/stats/model_inspection",
                          eco_function,
                          paste0(eco_function, "_fe_qq.png"))
png(qq_plot_path)
qqnorm(treatment_residuals, main = "Q-Q Plot")
qqline(treatment_residuals, col = "red")
dev.off()

# Residuals vs Fitted
res_vs_fit_path <- file.path(base_dir,
                             "marrs_acoustics/data/results/functions/stats/model_inspection",
                             eco_function,
                             paste0(eco_function, "_fe_residuals_vs_fitted.png"))
png(res_vs_fit_path)
plot(fitted(treatment_model), treatment_residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
dev.off()

###### Alternate Model (Dropping Date Random Effect) ######
alternate_model <- glmer.nb(
  count ~ treatment +
    (1 | country) +
    (1 | country:site),
  data = data
)
posthoc_results_drop_date <- emmeans(alternate_model, pairwise ~ treatment, adjust = "none")

sink(summary_path, append = TRUE)
cat("\n################ ALTERNATE MODEL (NO DATE RANDOM EFFECT) ################\n\n")
print_model_results(alternate_model, "DROP DATE")

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
print(posthoc_results_drop_date$contrasts)
sink()

###### Residual Diagnostics for Alternate Model ######
alternate_residuals <- residuals(alternate_model, type = "pearson")
data$alternate_residuals <- alternate_residuals

# Identify outliers
alternate_outliers <- data[abs(alternate_residuals) > 3, ]
alternate_outliers_path <- file.path(base_dir,
                                     "marrs_acoustics/data/results/functions/stats/model_inspection",
                                     eco_function,
                                     paste0(eco_function, "_dropdate_outliers"))
write.csv(alternate_outliers, alternate_outliers_path, row.names = FALSE)

# QQ Plot
alternate_qq_plot_path <- file.path(base_dir,
                                    "marrs_acoustics/data/results/functions/stats/model_inspection",
                                    eco_function,
                                    paste0(eco_function, "_dropdate_qq.png"))
png(alternate_qq_plot_path)
qqnorm(alternate_residuals, main = "Q-Q Plot (drop date model)")
qqline(alternate_residuals, col = "red")
dev.off()

# Residuals vs Fitted
alternate_res_vs_fit_path <- file.path(base_dir,
                                       "marrs_acoustics/data/results/functions/stats/model_inspection",
                                       eco_function,
                                       paste0(eco_function, "_dropdate_residuals_vs_fitted.png"))
png(alternate_res_vs_fit_path)
plot(fitted(alternate_model), alternate_residuals,
     main = "Residuals vs Fitted (drop date model)",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
dev.off()

