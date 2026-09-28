# Install only into the isolated revision library; do not modify global R packages.
dir.create('revision/.R-library',showWarnings=FALSE,recursive=TRUE)
install.packages(c('lme4','emmeans'),lib='revision/.R-library',
                 repos='https://cloud.r-project.org',Ncpus=4)
