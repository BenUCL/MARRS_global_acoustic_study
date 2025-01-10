# Global install of required libraries if not present
if (!require("lme4")) install.packages("lme4", repos = "http://cran.rstudio.com/")
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

# Identify extreme residuals
treatment_extreme <- data[abs(treatment_residuals) > 3, ]
extreme_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_outliers.txt"))
write.csv(treatment_extreme, extreme_path, row.names = FALSE)

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

# Append Log-Transformed Model Summaries
sink(summary_path, append = TRUE)
cat("\n################ LOG-TRANSFORMED DATA MODELS ################\n\n")
cat("### Log RE-Only Model Summary ###\n")
print(summary(log_re_only_model))

cat("\n### Log Full Model Summary ###\n")
print(summary(log_treatment_model))
sink()

# Residual diagnostics for Log-Transformed Model
log_residuals <- residuals(log_treatment_model)
log_extreme <- data[abs(log_residuals) > 3, ]
log_extreme_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/model_inspection", eco_function, paste0(eco_function, "_fe_outliers_log.txt"))
write.csv(log_extreme, log_extreme_path, row.names = FALSE)

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
