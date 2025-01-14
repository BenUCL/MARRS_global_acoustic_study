# Global install of required libraries if not present
if (!require("lme4")) install.packages("lme4", repos = "http://cran.rstudio.com/")
library(lme4)

# Check if the environment variable is set
base_dir <- Sys.getenv("BASE_DIR")
if (base_dir == "") {
  stop("BASE_DIR is not set. Please set it in your environment variables.")
}

# Set ecological function (choose one of the following: 'graze_count', 'snaps_count', 'phonic_richness', '{eco_function}')
eco_function <- "phonic_richness"

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
  count ~ 1 + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data
)

# Fit Model with Treatment
treatment_model <- glmer.nb(
  count ~ treatment + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data
)

# Save all summaries to a single text file
summary_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/summary_outputs", paste0(eco_function, "_summary.txt"))
sink(summary_path)
cat("################ RAW DATA MODELS ################\n\n")
cat("### RE-Only Model Summary ###\n")
print(summary(re_only_model))

cat("\n### Full Model Summary ###\n")
print(summary(treatment_model))
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

###### Alternate Model (Dropping Date Random Effect) ######
alternate_model <- glmer.nb(
  count ~ treatment + 
    (1 | country) + 
    (1 | country:site), 
  data = data
)

# Append Alternate Model Summary
sink(summary_path, append = TRUE)
cat("\n################ ALTERNATE MODEL (NO DATE RANDOM EFFECT) ################\n\n")
cat("### Alternate Model Summary ###\n")
print(summary(alternate_model))
sink()

# Residual diagnostics for Alternate Model
alternate_residuals <- residuals(alternate_model, type = "pearson")
data$alternate_residuals <- alternate_residuals

# Identify outliers residuals for Alternate Model
alternate_outliers <- data[abs(alternate_residuals) > 3, ]
alternate_outliers_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_dropdate_outliers"))
write.csv(alternate_outliers, alternate_outliers_path, row.names = FALSE)

# QQ Plot for Alternate Model
alternate_qq_plot_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_dropdate_qq.png"))
png(alternate_qq_plot_path)
qqnorm(alternate_residuals, main = "Q-Q Plot (drop date model)")
qqline(alternate_residuals, col = "red")
dev.off()

# Residuals vs Fitted for Alternate Model
alternate_res_vs_fit_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_dropdate_residuals_vs_fitted.png"))
png(alternate_res_vs_fit_path)
plot(fitted(alternate_model), alternate_residuals, main = "Residuals vs Fitted (drop date model)", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
dev.off()

# No outliers in the PR residuals so we don't need to run a model with these excluded