# Global install of lme4 if not present already
if (!require("lme4")) install.packages("lme4", repos = "http://cran.rstudio.com/")

# Imports
library(lme4)

# Check if the environment variable is set
base_dir <- Sys.getenv("BASE_DIR")
if (base_dir == "") {
  stop("BASE_DIR is not set. Please set it in your environment variables.")
}

# Set ecological function to 'graze_count'
eco_function <- "graze_count"

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
  data <- subset(data, country != "Kenya")
  print("Kenya excluded from analysis for graze_count as no grazing present.")
}

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
treatment_model <- glmer.nb(
  count ~ treatment + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date),
  data = data
)

# Append Treatment Model Summary
sink(summary_path, append = TRUE) # Redirect console output to file (append mode)
cat(paste0("\n################ FULL MODEL SUMMARY (", eco_function, ") ################\n"))
print(summary(treatment_model)) # Print summary to file
sink() # Stop redirecting

# Print summaries to console
summary(re_only_model)
summary(treatment_model)

###### Residual Diagnostics for Models ######

# Extract residuals and save diagnostics for Treatment Model
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
qqnorm(residuals(treatment_model), main = "Q-Q Plot of Treatment Model Residuals")
qqline(residuals(treatment_model), col = "red")
dev.off()
