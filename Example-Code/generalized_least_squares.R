library(nlme)
# load data & delete missing observations
data(globwarm, package = "faraway")
globwarm <- na.omit(globwarm)
# fit OLS model 
lmod <- gls(nhtemp ~ . - year, data = globwarm)
# fit GLS model with AR1 correlation structure
gmod <- gls(nhtemp ~ . - year, 
            data = globwarm, 
            correlation = corAR1(form = ~ year))
# estimate CI for correlation parameter 
intervals(gmod, which = "var-cov")
# compare models
anova(gmod, lmod)
