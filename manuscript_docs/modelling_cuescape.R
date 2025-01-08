# Global install of lme4 and DHARMa if not present already
if (!require("lme4")) install.packages("lme4", repos = "http://cran.rstudio.com/")

# Imports
library(lme4)


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

# Plot and save histogram
output_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats", paste0(eco_function, ".png"))
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
summary_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/summary_outputs", paste0(eco_function, "_model_summary.txt"))
sink(summary_path) # Redirect console output to file
cat(paste0("################ RE-ONLY MODEL SUMMARY (", eco_function, ") ################\n"))
print(summary(re_only_model)) # Print summary to file
sink() # Stop redirecting

# Fit Model with Treatment
treatment_model <- NULL

if (eco_function == "settlement_cuescape") {
  # Model with offset for settlement_cuescape
  treatment_model <- glmer.nb(
    count ~ treatment + offset(log(max_poss_count)) + 
      (1 | country) + 
      (1 | country:site) + 
      (1 | country:date), 
    data = data
  )
} else {
  # Standard model for other eco_functions
  treatment_model <- glmer.nb(
    count ~ treatment + 
      (1 | country) + 
      (1 | country:site) + 
      (1 | country:date), 
    data = data
  )
}

# Append Treatment Model Summary
sink(summary_path, append = TRUE) # Redirect console output to file (append mode)
cat(paste0("\n################ FULL MODEL SUMMARY (", eco_function, ") ################\n"))
print(summary(treatment_model)) # Print summary to file
sink() # Stop redirecting

# Print summaries to console
summary(re_only_model)
summary(treatment_model)


###### Now inspect the model fit through checking residuals. ######
# Extract residuals and save diagnostics for the Treatment Model
treatment_residuals <- residuals(treatment_model, type = "pearson")
data$treatment_residuals <- treatment_residuals

# Identify extreme residuals for Treatment Model
treatment_extreme <- data[abs(treatment_residuals) > 3, ]
extreme_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", paste0(eco_function, "_fe_outliers.txt"))
write.csv(treatment_extreme, extreme_path, row.names = FALSE)
cat(paste("Saved Treatment Model outliers to:", extreme_path, "\n"))

# Plot residuals 
png(file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", paste0(eco_function, "_fe_residuals_plot.png")))
hist(treatment_residuals, main = "Histogram of Treatment Model Residuals", xlab = "Residuals", col = "skyblue", border = "black")
dev.off()

# Plot residuals vs fitted
png(file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", paste0(eco_function, "_fe_residuals_vs_fitted.png")))
plot(fitted(treatment_model), residuals(treatment_model), main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals", col = "blue")
abline(h = 0, col = "red", lwd = 2)
dev.off()

# QQ plot
png(file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", paste0(eco_function, "_fe_qq_plot.png")))
qqnorm(residuals(treatment_model))
qqline(residuals(treatment_model), col = "red")
dev.off()

###### Given model had sub-optimal fit, now trying withh log transformed data. ######
# Add Log-Transformation of Count, plot and save histogram
data$log_count <- log(data$count + 1)  # Add 1 to avoid issues with log(0)
log_output_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats", paste0(eco_function, "_log_transformed.png"))
print(paste("Saving log-transformed histogram to:", log_output_path))
png(log_output_path)
log_hist_title <- paste("Histogram of Log-Transformed", eco_function)
hist(data$log_count, 
     main = log_hist_title, 
     xlab = paste("Log-Transformed Count per day of", eco_function),  
     col = "lightblue", 
     border = "black")
dev.off()

###### Fit and Evaluate Model with Log-Transformed Data ######
# Fit the model with log-transformed count and offset
log_treatment_model <- lmer(
  log_count ~ treatment + offset(log(max_poss_count)) + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data
)

# Save Log-Transformed Model Summary
log_summary_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/summary_outputs", paste0(eco_function, "_log_transformed_model_summary.txt"))
sink(log_summary_path) # Redirect console output to file
cat(paste0("################ LOG-TRANSFORMED MODEL SUMMARY (", eco_function, ") ################\n"))
print(summary(log_treatment_model)) # Print summary to file
sink() # Stop redirecting

# Print Log-Transformed Model Summary to Console
summary(log_treatment_model)

###### Inspect Model Fit for Log-Transformed Data ######
# Extract residuals and fitted values for the log-transformed model
log_residuals <- residuals(log_treatment_model)
log_fitted <- fitted(log_treatment_model)

# Save residuals for extreme values inspection
log_extreme <- data[abs(log_residuals) > 3, ]
log_extreme_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", paste0(eco_function, "_log_transformed_outliers.txt"))
write.csv(log_extreme, log_extreme_path, row.names = FALSE)
cat(paste("Saved Log-Transformed Model outliers to:", log_extreme_path, "\n"))

# Plot and save histogram of residuals
log_residuals_hist_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", paste0(eco_function, "_log_transformed_residuals_hist.png"))
png(log_residuals_hist_path)
hist(log_residuals, main = "Histogram of Log-Transformed Model Residuals", xlab = "Residuals", col = "skyblue", border = "black")
dev.off()

# Plot residuals vs fitted values
log_residuals_vs_fitted_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", paste0(eco_function, "_log_transformed_residuals_vs_fitted.png"))
png(log_residuals_vs_fitted_path)
plot(log_fitted, log_residuals, main = "Residuals vs Fitted (Log-Transformed)", xlab = "Fitted Values", ylab = "Residuals", col = "blue")
abline(h = 0, col = "red", lwd = 2)
dev.off()

# Plot Q-Q plot of residuals
log_qq_plot_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", paste0(eco_function, "_log_transformed_qq_plot.png"))
png(log_qq_plot_path)
qqnorm(log_residuals, main = "Q-Q Plot of Log-Transformed Model Residuals")
qqline(log_residuals, col = "red")
dev.off()

