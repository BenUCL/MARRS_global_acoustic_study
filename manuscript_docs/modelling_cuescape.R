# Global install of required libraries if not present
if (!require("lme4")) install.packages("lme4", repos = "http://cran.rstudio.com/")
library(lme4)
if (!require("emmeans")) install.packages("emmeans", repos = "http://cran.rstudio.com/")
library(emmeans)

# Check if the environment variable is set
base_dir <- Sys.getenv("BASE_DIR")
if (base_dir == "") {
  stop("BASE_DIR is not set. Please set it in your environment variables.")
}

# Set ecological function (choose one of the following: 'graze_count', 'snaps_count', 'phonic_richness', 'settlement_cuescape')
eco_function <- "settlement_cuescape"

# Validate eco_function
valid_functions <- c("snaps_count", "graze_count", "phonic_richness", "settlement_cuescape")
if (!(eco_function %in% valid_functions)) {
  stop("Invalid eco_function. Please set it to one of: ", paste(valid_functions, collapse = ", "))
}

# Construct file path dynamically
csv_path <- file.path(base_dir, "marrs_acoustics/data/results/functions", paste0(eco_function, ".csv"))
print(csv_path)

# Read the data
data <- read.csv(csv_path)
head(data)

# Plot and save histogram for raw data
hist_output_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/histograms", paste0(eco_function, ".png"))
png(hist_output_path)
hist_title <- paste("Histogram of", eco_function)
hist(data$count, 
     main = hist_title, 
     xlab = paste("Count per day of", eco_function),  
     col = "lightblue", 
     border = "black")
dev.off()

###### Start fitting models! ######
# Fit RE-Only Model
re_only_model <- glmer.nb(
  count ~ offset(log(max_poss_count)) + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data
)

# Fit Model with Treatment
treatment_model <- glmer.nb(
  count ~ treatment + offset(log(max_poss_count)) + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data
)

# Post hoc comparisons of treatments. We set to none, not Tukey as this was causing clear type 2 errors.
posthoc_results <- emmeans(treatment_model, pairwise ~ treatment, adjust = "none")

# Save all summaries to a single text file
summary_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/summary_outputs", paste0(eco_function, "_summary.txt"))
sink(summary_path)
cat("################ RAW DATA MODELS ################\n\n")
cat("### RE-Only Model Summary ###\n")
print(summary(re_only_model))

cat("\n### Full Model Summary ###\n")
print(summary(treatment_model))

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
print(posthoc_results$contrasts)  # Print pairwise comparisons
sink()

###### Inspect Model Fit ######
# Residual diagnostics for Treatment Model
treatment_residuals <- residuals(treatment_model, type = "pearson")
data$treatment_residuals <- treatment_residuals

# Identify outliers residuals
treatment_outliers <- data[abs(treatment_residuals) > 3, ]
outliers_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_outliers.csv"))
write.csv(treatment_outliers, outliers_path, row.names = FALSE)

# QQ Plot
qq_plot_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_qq.png"))
png(qq_plot_path)
qqnorm(treatment_residuals, main = "Q-Q Plot")
qqline(treatment_residuals, col = "red")
dev.off()

# Residuals vs Fitted
res_vs_fit_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_residuals_vs_fitted.png"))
png(res_vs_fit_path)
plot(fitted(treatment_model), treatment_residuals, main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
dev.off()

###### Log-Transformed Data ######
data$log_count <- log(data$count + 1)
log_hist_output_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/histograms", "settlement_cuescape_log.png")
png(log_hist_output_path)
hist_title <- paste("Histogram of Log-Transformed", eco_function)
hist(data$log_count, 
     main = hist_title, 
     xlab = paste("Log-Transformed Count per day of", eco_function),  
     col = "lightblue", 
     border = "black")
dev.off()

# Fit Log-Transformed RE-Only Model
log_re_only_model <- lmer(
  log_count ~ offset(log(max_poss_count)) + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data
)

# Fit Log-Transformed Model
log_treatment_model <- lmer(
  log_count ~ treatment + offset(log(max_poss_count)) + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data
)

# Post hoc comparisons of treatments. We set to none, not Tukey as this was causing clear type 2 errors.
posthoc_results_log <- emmeans(log_treatment_model, pairwise ~ treatment, adjust = "none")

# Append Log-Transformed Model Summaries
sink(summary_path, append = TRUE)
cat("\n################ LOG-TRANSFORMED DATA MODELS ################\n\n")
cat("### Log RE-Only Model Summary ###\n")
print(summary(log_re_only_model))

cat("\n### Log Full Model Summary ###\n")
print(summary(log_treatment_model))

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
print(posthoc_results_log$contrasts)  # Print pairwise comparisons
sink()

# Residual diagnostics for Log-Transformed Model
log_residuals <- residuals(log_treatment_model)
log_outliers <- data[abs(log_residuals) > 3, ]
log_outliers_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_outliers_log.csv"))
write.csv(log_outliers, log_outliers_path, row.names = FALSE)

# Log QQ Plot
log_qq_plot_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_qq_log.png"))
png(log_qq_plot_path)
qqnorm(log_residuals, main = "Q-Q Plot (Log-Transformed)")
qqline(log_residuals, col = "red")
dev.off()

# Log Residuals vs Fitted
log_res_vs_fit_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_residuals_vs_fitted_log.png"))
png(log_res_vs_fit_path)
plot(fitted(log_treatment_model), log_residuals, main = "Residuals vs Fitted (Log-Transformed)", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
dev.off()

########### Drop outliers and repeat ######################
# Ensure the columns for joining are consistent
# Load the extreme residuals (outliers) from the saved CSV file
outliers_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_outliers.csv"))
outliers <- read.csv(outliers_path)

# Ensure the columns for joining are consistent
outliers$key <- paste(outliers$country, outliers$site, outliers$date, sep = "_")
data$key <- paste(data$country, data$site, data$date, sep = "_")

# Filter data to exclude rows that match the outliers
data_no_outliers <- data[!data$key %in% outliers$key, ]

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

# Post hoc comparisons of treatments. We set to none, not Tukey as this was causing clear type 2 errors.
posthoc_results_drop_outliers <- emmeans(fe_drop_outliers, pairwise ~ treatment, adjust = "none")

# Append Log-Transformed Model Summaries
sink(summary_path, append = TRUE)
cat("\n################ LOG-TRANSFORMED DATA MODELS ################\n\n")
cat("### Log RE-Only Model Summary ###\n")
print(summary(log_re_only_model))

cat("\n### Log Full Model Summary ###\n")
print(summary(log_treatment_model))

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
print(posthoc_results_drop_outliers$contrasts)  # Print pairwise comparisons
sink()

# Residual diagnostics for models with drop outliers
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