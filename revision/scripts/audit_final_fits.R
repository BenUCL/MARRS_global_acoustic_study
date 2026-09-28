# Distinguish intermediate fitting warnings from the final model's convergence state.
.libPaths(c('revision/.R-library',.libPaths()))
suppressPackageStartupMessages(library(lme4))
paths<-list.files('revision/scratch',pattern='\\.rds$',full.names=TRUE)
rows<-lapply(paths,function(path){
 m<-readRDS(path)
 if(!inherits(m,'merMod'))return(NULL)
 final<-m@optinfo$conv$lme4$messages
 g<-m@optinfo$derivs$gradient
 data.frame(model=sub('\\.rds$','',basename(path)),optimizer_code=paste(m@optinfo$conv$opt,collapse=';'),
  final_messages=paste(final,collapse='; '),singular=isSingular(m),
  max_gradient=if(is.numeric(g))max(abs(g)) else NA_real_,
  pearson_ratio=sum(residuals(m,type='pearson')^2)/df.residual(m),
  n=nobs(m))
})
result<-do.call(rbind,rows)
write.csv(result,'revision/outputs/final_fit_diagnostics.csv',row.names=FALSE)
print(result[grepl('actual_effort|poisson_',result$model),c('model','optimizer_code','final_messages','singular')],row.names=FALSE)
