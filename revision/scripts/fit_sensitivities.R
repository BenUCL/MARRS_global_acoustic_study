# Reproduce the submitted models and fit explicitly labelled sensitivity models.
.libPaths(c('revision/.R-library', .libPaths()))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(emmeans))
root <- 'revision'
out <- file.path(root, 'outputs', 'analysis')
dir.create(out, recursive=TRUE, showWarnings=FALSE)
metadata <- read.csv(file.path(root, 'data/site_metadata.csv'))

fit_one <- function(d, metric, scenario, fixed='treatment', date=TRUE,
                    country_fixed=FALSE) {
  label <- paste(metric, scenario, sep='__')
  cat(format(Sys.time()), label, 'n=', nrow(d), '\n')
  warnings <- character()
  random <- if (country_fixed) '(1|country:site)' else '(1|country)+(1|country:site)'
  if (date) random <- paste(random, '+(1|country:date)')
  if (country_fixed) fixed <- paste(fixed, '+country')
  offset <- if (metric=='settlement_cuescape') '+offset(log(recorded_nighttime_hours))' else if ('recorded_hours' %in% names(d)) '+offset(log(recorded_hours))' else ''
  formula <- as.formula(paste('count ~', fixed, '+', random, offset))
  result <- tryCatch(withCallingHandlers({
    model <- glmer.nb(formula, data=d, control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
    coefficients <- as.data.frame(coef(summary(model)))
    coefficients$term <- rownames(coefficients)
    names(coefficients)[1:4] <- c('estimate','se','z','p')
    coefficients$lower95 <- coefficients$estimate - qnorm(.975)*coefficients$se
    coefficients$upper95 <- coefficients$estimate + qnorm(.975)*coefficients$se
    coefficients$ratio <- exp(coefficients$estimate)
    coefficients$metric <- metric
    coefficients$scenario <- scenario
    write.csv(coefficients, file.path(out,paste0(label,'_coefficients.csv')),row.names=FALSE)
    conv <- model@optinfo$conv$lme4$messages
    diag <- data.frame(metric,scenario,n=nrow(d),sites=length(unique(paste(d$country,d$site))),
      formula=paste(deparse(formula),collapse=' '),singular=isSingular(model),
      theta=getME(model,'glmer.nb.theta'),AIC=AIC(model),
      pearson_ratio=sum(residuals(model,type='pearson')^2)/df.residual(model),
      convergence=paste(conv,collapse='; '))
    write.csv(diag,file.path(out,paste0(label,'_diagnostics.csv')),row.names=FALSE)
    capture.output(summary(model),file=file.path(out,paste0(label,'_summary.txt')))
    if (fixed=='treatment') {
      contrasts <- as.data.frame(summary(pairs(emmeans(model,~treatment)),infer=TRUE,adjust='tukey'))
      write.csv(contrasts,file.path(out,paste0(label,'_contrasts.csv')),row.names=FALSE)
    }
    saveRDS(model,file.path(root,'scratch',paste0(label,'.rds')))
    'ok'
  },warning=function(w){warnings <<- c(warnings,conditionMessage(w)); invokeRestart('muffleWarning')}),
  error=function(e) paste('ERROR:',conditionMessage(e)))
  writeLines(c(result,unique(warnings)),file.path(out,paste0(label,'_warnings.txt')))
  cat(label, result, '\n')
}

run_metric <- function(metric) {
  d <- read.csv(file.path(root,'data/results/functions',paste0(metric,'.csv')))
  if (metric=='graze_count') d <- subset(d,country!='kenya')
  if (metric=='settlement_cuescape') d$recorded_nighttime_hours <- d$max_poss_count / 720
  d$treatment <- relevel(factor(d$treatment),'degraded')
  d <- merge(d,metadata,by=c('country','site'),all.x=TRUE)
  d$age_months <- NULL
  d <- merge(d, read.csv(file.path(root,'data/results/functions/sites_age.csv')),
             by=c('country','site'),all.x=TRUE)
  use_date <- metric!='settlement_cuescape'
  fit_one(d,metric,'baseline',date=use_date)
  if (metric %in% c('graze_count','snaps_count')) {
    effort <- read.csv(file.path(root,'data',paste0(metric,'_effort.csv')))
    match_rows <- match(paste(d$country,d$site,d$date),paste(effort$country,effort$site,effort$date))
    d$count <- effort$count[match_rows]
    d$recorded_hours <- effort$recorded_hours[match_rows]
    stopifnot(!anyNA(d$count),!anyNA(d$recorded_hours))
  }
  if (metric=='phonic_richness') {
    corrected <- read.csv(file.path(root,'data/richness_reconstructed.csv'))
    d$count <- corrected$count[match(paste(d$country,d$site,d$date),paste(corrected$country,corrected$site,corrected$date))]
    stopifnot(!anyNA(d$count))
    fit_one(d,metric,'corrected_baseline',date=use_date)
  }
  fit_one(subset(d,country!='indonesia'),metric,'exclude_indonesia',date=use_date)
  fit_one(d,metric,'country_fixed',date=use_date,country_fixed=TRUE)
  if (!use_date) fit_one(d,metric,'include_date',date=TRUE)
  # Same-row baseline is essential: missing cover removes the Maldives entirely.
  cover <- subset(d,!is.na(coral_cover))
  for (bound in c(0,2.5,5)) {
    cover$cover10 <- ifelse(cover$cover_censored,bound,cover$coral_cover)/10
    if (bound==2.5) fit_one(cover,metric,'cover_complete_baseline',date=use_date)
    fit_one(cover,metric,paste0('cover_bound_',bound),fixed='treatment+cover10',date=use_date)
  }
  # Age slopes are exploratory between-site comparisons, not recovery rates.
  mid <- subset(d,treatment=='restored' & !is.na(age))
  mid$age10 <- (mid$age-40)/10
  fit_one(mid,metric,'mid_age',fixed='age10',date=use_date,country_fixed=TRUE)
  restored <- subset(d,!is.na(reef_stars))
  restored$treatment <- droplevels(restored$treatment)
  restored$stars100 <- restored$reef_stars/100
  fit_one(restored,metric,'restored_baseline',date=use_date)
  fit_one(restored,metric,'restored_stars',fixed='treatment+stars100',date=use_date)
}
metrics <- commandArgs(trailingOnly=TRUE)
if (!length(metrics)) metrics <- c('phonic_richness','settlement_cuescape','graze_count','snaps_count')
stopifnot(all(metrics %in% c('phonic_richness','settlement_cuescape','graze_count','snaps_count')))
parallel::mclapply(metrics,run_metric,mc.cores=min(4,length(metrics)))
if ('phonic_richness' %in% metrics) for (phase in c('phase0','phase2')) {
  d <- read.csv(file.path(root,'data',paste0('richness_',phase,'.csv')))
  d$treatment <- relevel(factor(d$treatment),'degraded')
  fit_one(d,'phonic_richness',paste0('duty_',phase))
}
capture.output(sessionInfo(),file=file.path(out,'R_session.txt'))
