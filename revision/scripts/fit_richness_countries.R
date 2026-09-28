# Negative binomial project fits using the same fish-richness observations.
.libPaths(c('revision/.R-library', .libPaths()))
suppressPackageStartupMessages(library(lme4))
root <- 'revision'
data <- read.csv(file.path(root, 'data/richness_reconstructed.csv'))
data$treatment <- relevel(factor(data$treatment), 'degraded')
for (country in unique(data$country)) {
  label <- paste0('phonic_richness__nb_', country)
  cat(format(Sys.time()), label, '\n')
  local <- droplevels(data[data$country == country, ])
  warnings <- character()
  model <- withCallingHandlers(glmer.nb(
    count ~ treatment + (1 | site) + (1 | date), data = local,
    control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5))),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart('muffleWarning')
    })
  coefficients <- as.data.frame(coef(summary(model)))
  coefficients$term <- rownames(coefficients)
  names(coefficients)[1:4] <- c('estimate', 'se', 'z', 'p')
  coefficients$lower95 <- coefficients$estimate - qnorm(.975) * coefficients$se
  coefficients$upper95 <- coefficients$estimate + qnorm(.975) * coefficients$se
  coefficients$metric <- 'phonic_richness'
  coefficients$scenario <- paste0('nb_', country)
  write.csv(coefficients, file.path(root, 'outputs/analysis', paste0(label, '_coefficients.csv')), row.names = FALSE)
  writeLines(unique(warnings), file.path(root, 'outputs/analysis', paste0(label, '_warnings.txt')))
  capture.output(summary(model), file = file.path(root, 'outputs/analysis', paste0(label, '_summary.txt')))
  saveRDS(model, file.path(root, 'scratch', paste0(label, '.rds')))
  cat(label, 'done\n')
}
