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

# Fit the GLMM model
# Negative Poisson binomial error structure
model <- glmer.nb(
  count ~ 1 + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data
)

# Save Summary of the RE-Only Model in a readable format
summary_path <- file.path(base_dir, "marrs_acoustics/data/results/functions/stats/summary_outputs", paste0(eco_function, "_full_dataset.txt"))
sink(summary_path) # Redirect console output to file
cat(paste0("################ RE ONLY MODEL SUMMARY (", eco_function, ") ################\n"))
print(summary(model)) # Print summary to file
sink() # Stop redirecting
summary(model)


# Fit the GLMM model with treatment as a fixed effect
model_treatment <- glmer.nb(
  count ~ treatment + 
    (1 | country) + 
    (1 | country:site) + 
    (1 | country:date), 
  data = data
)

# Summary of the new model
# Append Summary of the Model with Treatment in a readable format
sink(summary_path, append = TRUE) # Redirect console output to file (append mode)
cat(paste0("\n################ FULL MODEL SUMMARY (", eco_function, ") ################\n"))
print(summary(model_treatment)) # Print summary to file
sink() # Stop redirecting
summary(model_treatment)


