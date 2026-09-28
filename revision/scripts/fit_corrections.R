.libPaths(c('revision/.R-library',.libPaths()))
library(lme4)
root <- 'revision'; out <- file.path(root,'outputs/analysis')
fit <- function(d,metric,scenario,formula,family='nb') {
  cat(metric,scenario,'\n')
  warns <- character()
  m <- withCallingHandlers(if(family=='poisson') glmer(formula,data=d,family=poisson,
      control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5))) else
      glmer.nb(formula,data=d,control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5))), warning=function(w) {warns <<- c(warns,conditionMessage(w));invokeRestart('muffleWarning')})
  c <- as.data.frame(coef(summary(m)));c$term<-rownames(c);names(c)[1:4]<-c('estimate','se','z','p')
  c$lower95<-c$estimate-qnorm(.975)*c$se;c$upper95<-c$estimate+qnorm(.975)*c$se
  c$metric<-metric;c$scenario<-scenario
  write.csv(c,file.path(out,paste0(metric,'__',scenario,'_coefficients.csv')),row.names=FALSE)
  writeLines(c(unique(warns),model=paste(deparse(formula),collapse=' '),singular=isSingular(m),
    convergence=paste(m@optinfo$conv$lme4$messages,collapse='; ')),file.path(out,paste0(metric,'__',scenario,'_warnings.txt')))
  capture.output(summary(m),file=file.path(out,paste0(metric,'__',scenario,'_summary.txt')))
  saveRDS(m,file.path(root,'scratch',paste0(metric,'__',scenario,'.rds')))
}
for (metric in c('graze_count','snaps_count')) {
 d<-read.csv(file.path(root,'data',paste0(metric,'_effort.csv')))
 if(metric=='graze_count') d<-subset(d,country!='kenya')
 d$treatment<-relevel(factor(d$treatment),'degraded')
 f<-count~treatment+offset(log(recorded_hours))+(1|country)+(1|country:site)+(1|country:date)
 fit(d,metric,'actual_effort',f)
 fit(subset(d,country!='indonesia'),metric,'actual_effort_exclude_indonesia',f)
}
# Match project-specific figures to the effort-standardised global analyses.
for (metric in c('graze_count','snaps_count')) {
 d<-read.csv(file.path(root,'data',paste0(metric,'_effort.csv')))
 if(metric=='graze_count') d<-subset(d,country!='kenya')
 d$treatment<-relevel(factor(d$treatment),'degraded')
 for (project in unique(d$country)) {
  local<-droplevels(d[d$country==project,])
  fit(local,metric,paste0('actual_effort_',project),count~treatment+offset(log(recorded_hours))+(1|site)+(1|date))
 }
}

source('revision/scripts/fit_richness_countries.R')
