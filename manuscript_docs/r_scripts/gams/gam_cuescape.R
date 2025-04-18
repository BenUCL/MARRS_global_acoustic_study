# -------------------------
# Set eco_function
# -------------------------
# Choose from: settlement_cuescape, phonic_richness, graze_count, snaps_count
eco_function <- "settlement_cuescape"

# -------------------------
# Required libraries
# -------------------------
if (!require("mgcv")) install.packages("mgcv", repos = "http://cran.rstudio.com/")
library(mgcv)
if (!require("readr")) install.packages("readr", repos = "http://cran.rstudio.com/")
library(readr)
if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.rstudio.com/")
library(ggplot2)

# -------------------------
# Set Base Directory
# -------------------------
BASE_DIR <- Sys.getenv("BASE_DIR")
if (BASE_DIR == "") {
  stop("BASE_DIR is not set. Please set it in your environment variables.")
}

# -------------------------
# Define Input & Output Paths
# -------------------------
# Input CSV: use eco_function (e.g., settlement_cuescape.csv)
csv_path <- file.path(BASE_DIR, "marrs_acoustics/data/results/functions", paste0(eco_function, ".csv"))
# Output folder for GAM stats for this eco_function
out_folder <- file.path(BASE_DIR, "marrs_acoustics/data/results/functions/stats/gams", eco_function)
if (!dir.exists(out_folder)) {
  dir.create(out_folder, recursive = TRUE)
}

# -------------------------
# Read the Data & Prepare Variables
# -------------------------
data <- read_csv(csv_path, col_types = cols())

# Ensure key variables are correctly typed
data$treatment <- as.factor(data$treatment)
data$country   <- as.factor(data$country)
data$site      <- as.factor(data$site)
data$date      <- as.factor(data$date)  # for random effects
data$age       <- as.numeric(data$age)

# Recode treatment: any "restored" or "newly_restored" becomes "restoration_intervention"
data$treatment_new <- as.character(data$treatment)
data$treatment_new[data$treatment_new %in% c("restored", "newly_restored")] <- "restoration_intervention"
data$treatment_new <- factor(data$treatment_new,
                             levels = c("degraded", "healthy", "restoration_intervention"))
data$treatment_new <- relevel(data$treatment_new, ref = "degraded")

# Create indicator for restoration intervention: 1 if restoration_intervention, 0 otherwise
data$rest_interv <- ifelse(data$treatment_new == "restoration_intervention", 1, 0)

# For healthy and degraded rows, set age to 0 (so the smooth s(age) is multiplied by 0)
data$age <- ifelse(is.na(data$age), 0, data$age)

# Create nested random effect factors
data$country_site <- interaction(data$country, data$site, sep = "_")
data$country_date <- interaction(data$country, data$date, sep = "_")

# -------------------------
# Fit the GAM Model
# -------------------------
# If eco_function is "phonic_richness", do not use the offset; else use offset(log(max_poss_count))
if (eco_function == "phonic_richness") {
  gam_model <- gam(
    count ~ treatment_new +
      s(age, k = 10, by = rest_interv) +
      s(country, bs = "re") +
      s(country_site, bs = "re") +
      s(country_date, bs = "re"),
    family = nb(),
    data = data,
    method = "REML"
  )
} else {
  gam_model <- gam(
    count ~ treatment_new +
      s(age, k = 10, by = rest_interv) +
      offset(log(max_poss_count)) +
      s(country, bs = "re") +
      s(country_site, bs = "re") +
      s(country_date, bs = "re"),
    family = nb(),
    data = data,
    method = "REML"
  )
}

# Save the model summary to a text file
summary_file <- file.path(out_folder, paste0(eco_function, "_gam_summary.txt"))
sink(summary_file)
cat("GAM Model Summary\n")
print(summary(gam_model))
sink()

# -------------------------
# Diagnostic Plots
# -------------------------
png(file.path(out_folder, "age_smooth.png"), width = 800, height = 600)
plot(gam_model, select = 1, shade = TRUE, main = "Smooth Effect of Age for Restoration Intervention")
dev.off()

png(file.path(out_folder, "residuals_vs_fitted.png"), width = 800, height = 600)
plot(gam_model$fitted.values, residuals(gam_model),
     xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red", lwd = 2)
dev.off()

png(file.path(out_folder, "qq_plot.png"), width = 800, height = 600)
qqnorm(residuals(gam_model), main = "QQ Plot of GAM Residuals")
qqline(residuals(gam_model), col = "red")
dev.off()

pdf(file.path(out_folder, "gam_check_plots.pdf"), width = 8, height = 6)
gam.check(gam_model)
dev.off()

# -------------------------
# Plot Restoration Age Effect using ggplot2
# -------------------------
age_seq <- seq(min(data$age[data$rest_interv == 1], na.rm = TRUE),
               max(data$age[data$rest_interv == 1], na.rm = TRUE),
               length.out = 100)
# For prediction, choose a typical max_poss_count (only used if offset is applied)
typical_max_poss <- if (eco_function == "phonic_richness") NA else median(data$max_poss_count, na.rm = TRUE)

pred_data <- data.frame(
  treatment_new = factor(rep("restoration_intervention", length(age_seq)),
                         levels = levels(data$treatment_new)),
  rest_interv = rep(1, length(age_seq)),
  age = age_seq,
  max_poss_count = if (eco_function == "phonic_richness") NA else rep(typical_max_poss, length(age_seq)),
  country = rep(levels(data$country)[1], length(age_seq)),
  site = rep(levels(data$site)[1], length(age_seq)),
  date = rep(levels(data$date)[1], length(age_seq))
)
pred_data$country_site <- interaction(pred_data$country, pred_data$site, sep = "_")
pred_data$country_date <- interaction(pred_data$country, pred_data$date, sep = "_")

# Predict on the link scale excluding random effects
pred_link <- predict(gam_model, newdata = pred_data, type = "link", se.fit = TRUE,
                     exclude = c("s(country)", "s(country_site)", "s(country_date)"))
pred_data$fit_link <- pred_link$fit
pred_data$se_link <- pred_link$se.fit
pred_data$upper_link <- pred_data$fit_link + 2 * pred_data$se_link
pred_data$lower_link <- pred_data$fit_link - 2 * pred_data$se_link

# Back-transform predictions if using a log link
pred_data$fit_resp <- exp(pred_data$fit_link)
pred_data$upper_resp <- exp(pred_data$upper_link)
pred_data$lower_resp <- exp(pred_data$lower_link)

# Plot with white background (linear y-scale)
p <- ggplot(pred_data, aes(x = age, y = fit_resp)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower_resp, ymax = upper_resp), fill = "blue", alpha = 0.2) +
  labs(title = "Effect of Age on Count for Restoration Intervention",
       x = "Restoration Age (months)",
       y = "Predicted Count (adjusted)") +
  theme_bw()
ggsave(filename = file.path(out_folder, "restoration_intervention_age_effect.png"),
       plot = p, width = 8, height = 6)

# Plot with log10 y-axis
p_log <- p + scale_y_log10() +
  labs(y = "Predicted Count (log scale)")
ggsave(filename = file.path(out_folder, "restoration_intervention_age_effect_log.png"),
       plot = p_log, width = 8, height = 6)

# -------------------------
# Save Smooth Terms Summary to CSV
# -------------------------
# Extract smooth terms table from summary
sm_table <- summary(gam_model)$s.table
# Create a new data frame row with the values we need
new_row <- data.frame(
  eco_function = eco_function,
  rest_interv_edf = sm_table["s(age):rest_interv", "edf"],
  rest_interv_df = sm_table["s(age):rest_interv", "Ref.df"],
  rest_interv_chisq = sm_table["s(age):rest_interv", "Chi.sq"],
  rest_interv_pvalue = sm_table["s(age):rest_interv", "p-value"],
  country_edf = sm_table["s(country)", "edf"],
  country_df = sm_table["s(country)", "Ref.df"],
  country_chisq = sm_table["s(country)", "Chi.sq"],
  country_pvalue = sm_table["s(country)", "p-value"],
  country_site_edf = sm_table["s(country_site)", "edf"],
  country_site_df = sm_table["s(country_site)", "Ref.df"],
  country_site_chisq = sm_table["s(country_site)", "Chi.sq"],
  country_site_pvalue = sm_table["s(country_site)", "p-value"],
  country_date_edf = sm_table["s(country_date)", "edf"],
  country_date_df = sm_table["s(country_date)", "Ref.df"],
  country_date_chisq = sm_table["s(country_date)", "Chi.sq"],
  country_date_pvalue = sm_table["s(country_date)", "p-value"],
  stringsAsFactors = FALSE
)

# Path for the master CSV file
all_gams_csv <- file.path(BASE_DIR, "marrs_acoustics/data/results/functions/stats/gams/all_gams.csv")
if (file.exists(all_gams_csv)) {
  all_gams <- read.csv(all_gams_csv, stringsAsFactors = FALSE)
  # If an entry for this eco_function exists, update it; else, append.
  if (eco_function %in% all_gams$eco_function) {
    all_gams[all_gams$eco_function == eco_function, ] <- new_row
  } else {
    all_gams <- rbind(all_gams, new_row)
  }
} else {
  all_gams <- new_row
}
write.csv(all_gams, file = all_gams_csv, row.names = FALSE)
