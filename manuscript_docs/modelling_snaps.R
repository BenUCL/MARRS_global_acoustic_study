# Global install of lme4 if not present already
if (!require("lme4")) install.packages("lme4", repos = "http://cran.rstudio.com/")

# Imports
library(lme4)

# Check if the environment variable is set
base_dir <- Sys.getenv("BASE_DIR")
if (base_dir == "") {
  stop("BASE_DIR is not set. Please set it in your environment variables.")
}

# Set ecological function to 'graze_count' or 'snaps_count'
eco_function <- "snaps_count"

# Validate eco_function
valid_functions <- c("graze_count", "snaps_count")
if (!(eco_function %in% valid_functions)) {
  stop("Invalid eco_function. Please set it to one of: ", paste(valid_functions, collapse = ", "))
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

###### Start fitting models! ######
# Fit RE-Only Model
re_only_model <- glmer.nb(
  count ~ 1 + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date),
  data = data
)

# Save RE-Only Model Summary
summary_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/summary_outputs", paste0(eco_function, "_summary.txt"))
sink(summary_path) # Redirect console output to file
cat(paste0("################ RE-ONLY MODEL SUMMARY (", eco_function, ") ################\n"))
print(summary(re_only_model)) # Print summary to file
sink() # Stop redirecting

# Fit Model with Treatment
treatment_model <- glmer.nb(
  count ~ treatment + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date),
  data = data
)

# Post hoc comparisons of treatments
posthoc_results <- emmeans(treatment_model, pairwise ~ treatment, adjust = "tukey")

# Append Treatment Model Summary
sink(summary_path, append = TRUE) # Redirect console output to file (append mode)
cat(paste0("\n################ FULL MODEL SUMMARY (", eco_function, ") ################\n"))
print(summary(treatment_model)) # Print summary to file

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
print(posthoc_results$contrasts)  # Print pairwise comparisons
sink()

###### Residual Diagnostics for Models ######

# Extract residuals and save diagnostics for Treatment Model
treatment_residuals <- residuals(treatment_model, type = "pearson")
data$treatment_residuals <- treatment_residuals

# Identify outliers residuals for Treatment Model
outliers_dir <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function)
dir.create(outliers_dir, recursive = TRUE, showWarnings = FALSE)
outliers_path <- file.path(outliers_dir, paste0(eco_function, "_fe_outliers.csv"))
write.csv(data[abs(treatment_residuals) > 3, ], outliers_path, row.names = FALSE)
cat(paste("Saved Treatment Model outliers to:", outliers_path, "\n"))

# Plot residuals vs fitted
residual_plot_path <- file.path(outliers_dir, paste0(eco_function, "_fe_residuals_vs_fitted.png"))
png(residual_plot_path)
plot(fitted(treatment_model), residuals(treatment_model), main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals", col = "blue")
abline(h = 0, col = "red", lwd = 2)
dev.off()

# QQ plot
qq_plot_path <- file.path(outliers_dir, paste0(eco_function, "_fe_qq_plot.png"))
png(qq_plot_path)
qqnorm(residuals(treatment_model), main = "Q-Q Plot of Treatment Model Residuals")
qqline(residuals(treatment_model), col = "red")
dev.off()

###### Fit and Evaluate Log-Transformed LMM ######
# Log-transform the count variable (adding 1 to avoid log(0))
data$log_count <- log(data$count + 1)

# Fit LMM with log-transformed count
log_lmm_model <- lmer(
  log_count ~ treatment + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date),
  data = data
)

# Post hoc comparisons of treatments
posthoc_results_log <- emmeans(log_lmm_model, pairwise ~ treatment, adjust = "tukey")

# Append Log-Transformed Model Summary
sink(summary_path, append = TRUE) # Redirect console output to file (append mode)
cat("################ LOG-TRANSFORMED LMM ANALYSIS ################\n")

cat("### Full Model Summary ###\n\n")
print(summary(log_lmm_model)) # Print summary to file

cat("\n################ POST-HOC TEST RESULTS ################\n\n")
print(posthoc_results_log$contrasts)  # Print pairwise comparisons
sink()

###### Residual Diagnostics for Log-Transformed Model ######
# Extract residuals and fitted values for the log-transformed model
log_residuals <- residuals(log_lmm_model)
log_fitted <- fitted(log_lmm_model)

# Save residuals for outliers values inspection
log_outliers_path <- file.path(outliers_dir, paste0(eco_function, "_transformed_fe_outliers_log.csv"))
write.csv(data[abs(log_residuals) > 3, ], log_outliers_path, row.names = FALSE)
cat(paste("Saved Log-Transformed Model outliers to:", log_outliers_path, "\n"))

# Plot residuals vs fitted values
log_residuals_vs_fitted_path <- file.path(outliers_dir, paste0(eco_function, "_residuals_vs_fitted_log.png"))
png(log_residuals_vs_fitted_path)
plot(log_fitted, log_residuals, main = "Residuals vs Fitted (Log-Transformed)", xlab = "Fitted Values", ylab = "Residuals", col = "blue")
abline(h = 0, col = "red", lwd = 2)
dev.off()

# Plot Q-Q plot of residuals
log_qq_plot_path <- file.path(outliers_dir, paste0(eco_function, "_qq_plot_log.png"))
png(log_qq_plot_path)
qqnorm(log_residuals, main = "Q-Q Plot of Log-Transformed Model Residuals")
qqline(log_residuals, col = "red")
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
  count ~ 1 + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data_no_outliers
)

# FE model
fe_drop_outliers <- glmer.nb(
  count ~ treatment + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data_no_outliers
)

# Post hoc comparisons of treatments
posthoc_results_drop_outliers <- emmeans(fe_drop_outliers, pairwise ~ treatment, adjust = "tukey")

# Append Log-Transformed Model Summaries
sink(summary_path, append = TRUE)
cat("\n################ DROP OUTLIERS MODELS ################\n\n")
cat("### Log RE-Only Model Summary ###\n")
print(summary(re_drop_outliers))

cat("\n### Log Full Model Summary ###\n")
print(summary(fe_drop_outliers))

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
