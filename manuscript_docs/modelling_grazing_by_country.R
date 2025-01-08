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

# Exclude Kenya for graze_count
if (eco_function == "graze_count") {
  data <- subset(data, country != "kenya")
  print("Kenya excluded from analysis for graze_count.")
}

# Get unique countries
countries <- unique(data$country)

# Paths for outputs
summary_dir <- "/home/bwilliams/ucl_projects/marrs_acoustics/data/results/functions/stats/summary_outputs/grazing_by_country"
residual_dir <- "/home/bwilliams/ucl_projects/marrs_acoustics/data/results/functions/stats/model_inspection/grazing_by_country"
histogram_dir <- "/home/bwilliams/ucl_projects/marrs_acoustics/data/results/functions/stats/histograms_by_country"

# Ensure directories exist
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(residual_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(histogram_dir, recursive = TRUE, showWarnings = FALSE)

# Loop through each country
for (cntry in countries) {
  message(paste("Running analysis for:", cntry))
  
  # Correct subsetting:
  country_data <- subset(data, data$country == cntry)
  
  if (nrow(country_data) == 0) {
    message(paste("No data for:", cntry))
    next
  }
  
  # Plot and save histogram
  hist_path <- file.path(histogram_dir, paste0(eco_function, "_", cntry, "_histogram.png"))
  png(hist_path)
  hist_title <- paste("Histogram of", eco_function, "in", cntry)
  hist(country_data$count, 
       main = hist_title, 
       xlab = paste("Count per day of", eco_function),  
       col = "lightblue", 
       border = "black")
  dev.off()
  
  # Fit RE-Only Model
  re_only_model <- glmer.nb(
    count ~ 1 + 
      (1 | site) + 
      (1 | date),
    data = country_data
  )
  
  # Fit Full Model with Treatment
  treatment_model <- glmer.nb(
    count ~ treatment + 
      (1 | site) + 
      (1 | date),
    data = country_data
  )
  
  # Save model summaries
  summary_path <- file.path(summary_dir, paste0(eco_function, "_", cntry, "_summary.txt"))
  sink(summary_path) # Redirect console output to file
  cat(paste0("################ RE-ONLY MODEL SUMMARY (", cntry, ") ################\n"))
  print(summary(re_only_model))
  cat(paste0("\n################ FULL MODEL SUMMARY (", cntry, ") ################\n"))
  print(summary(treatment_model))
  sink() # Stop redirecting
  
  # Residual Diagnostics for Treatment Model
  treatment_residuals <- residuals(treatment_model, type = "pearson")
  
  # Plot residual histogram
  residual_hist_path <- file.path(residual_dir, paste0(eco_function, "_", cntry, "_residuals_plot.png"))
  png(residual_hist_path)
  hist(treatment_residuals, 
       main = paste("Histogram of Residuals for", eco_function, "in", cntry), 
       xlab = "Residuals", 
       col = "skyblue", 
       border = "black")
  dev.off()
  
  # Plot residuals vs fitted
  residual_vs_fitted_path <- file.path(residual_dir, paste0(eco_function, "_", cntry, "_residuals_vs_fitted.png"))
  png(residual_vs_fitted_path)
  plot(fitted(treatment_model), treatment_residuals, 
       main = paste("Residuals vs Fitted for", eco_function, "in", cntry), 
       xlab = "Fitted Values", 
       ylab = "Residuals", 
       col = "blue")
  abline(h = 0, col = "red", lwd = 2)
  dev.off()
  
  # Q-Q plot
  qq_plot_path <- file.path(residual_dir, paste0(eco_function, "_", cntry, "_qq_plot.png"))
  png(qq_plot_path)
  qqnorm(treatment_residuals, 
         main = paste("Q-Q Plot of Residuals for", eco_function, "in", cntry))
  qqline(treatment_residuals, col = "red")
  dev.off()
}
