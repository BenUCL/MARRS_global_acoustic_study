# Global install of lme4 if not present already
if (!require("lme4")) install.packages("lme4", repos = "http://cran.rstudio.com/")

# Imports
library(lme4)

# Check if the environment variable is set
base_dir <- Sys.getenv("BASE_DIR")
if (base_dir == "") {
  stop("BASE_DIR is not set. Please set it in your environment variables.")
}

# Set ecological function (choose one of the following: 'graze_count', 'snaps_count', 'phonic_richness', 'settlement_cuescape')
eco_function <- "snaps_count"

# Validate eco_function
valid_functions <- c("snap_count", "graze_count", "phonic_richness", "settlement_cuescape")
if (!(eco_function %in% valid_functions)) {
  stop("Invalid eco_function. Please set it to one of: ", paste(valid_functions, collapse = ", "))
}

# Construct file path dynamically
csv_path <- file.path(base_dir, "marrs_acoustics/data/results/functions", paste0(eco_function, ".csv"))
print(csv_path)

# Read the data
data <- read.csv(csv_path)
head(data)

# Plot histogram for each country
countries <- unique(data$country)
for (country in countries) {
  country_data <- data[data$country == country, ]
  country_output_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/histograms_by_country", paste0(eco_function, "_", country, ".png"))
  print(paste("Saving histogram for country:", country, "to:", country_output_path))
  
  png(country_output_path)
  hist_title <- paste("Histogram of", eco_function, "in", country)
  hist(country_data$count, 
       main = hist_title, 
       xlab = paste("Count per day of", eco_function, "in", country),  
       col = "lightblue", 
       border = "black")
  dev.off()
}

# Fit and summarise RANDOM EFFECT ONLY models for each country
for (country in countries) {
  country_data <- data[data$country == country, ]
  
  # Check if count is constant (this will skip kenya graze, which is all 0)
  if (length(unique(country_data$count)) == 1) {
    print(paste("Skipping model for country:", country, "- Response is constant"))
    next
  }
  
  # Fit the GLMM with random effects for site and date
  model <- glmer.nb(
    count ~ 1 + (1 | site) + (1 | date),
    data = country_data
  )
  
  print(paste("Model summary for country:", country))
  print(summary(model))
}


# Fit and summarise models for each country with treatment as a fixed effect
for (country in countries) {
  country_data <- data[data$country == country, ]
  
  # Check if count is constant (this will skip kenya graze, which is all 0)
  if (length(unique(country_data$count)) == 1) {
    print(paste("Skipping model for country:", country, "- Response is constant"))
    next
  }
  
  # Fit the GLMM with treatment as a fixed effect and random effects for site and date
  model <- glmer.nb(
    count ~ treatment + (1 | site) + (1 | date),
    data = country_data
  )
  
  # Print a summary of the model
  print(paste("Model summary for country:", country))
  print(summary(model))
}

