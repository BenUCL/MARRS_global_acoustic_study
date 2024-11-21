# Global install of lme4 if not present already
if (!require("lme4")) install.packages("lme4", repos = "http://cran.rstudio.com/")

#imports
library(lme4)

# Check if the environment variable is set
base_dir <- Sys.getenv("BASE_DIR")
if (base_dir == "") {
  stop("BASE_DIR is not set. Please set it in your environment variables.")
}

# Use base_dir in a file path
csv_path <- file.path(base_dir, "code/scratch/mock_results_grazing.csv")
print(csv_path)

# Read the data
data <- read.csv(csv_path)
print(colnames(data))


# Fit the mixed model
model <- lmer(GrazingRate ~ Treatment + (1 | Country), data = data)

# Display the summary of the model
summary(model)

# Save model output to a text file
output_path <- "/home/bwilliams/ucl_projects/marrs_acoustics/code/scratch/model_summary.txt"
sink(output_path)
summary(model)
sink()

# Notify user
cat("Model summary saved to", output_path, "\n")



