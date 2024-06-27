library(lme4)
library(RLRsim)
data("sleepstudy")
# fit random intercept model
mix_mod <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy) 
# estimate restricted likelihood ratio test statistic 
exactRLRT(mix_mod)
