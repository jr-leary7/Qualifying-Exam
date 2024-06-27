library(lme4)
library(pbkrtest)
data("sleepstudy")
# fit random intercept only model
null_mod <- lmer(Reaction ~ (1 | Subject), data = sleepstudy)
# fit model with random intercepts and fixed effect for time
full_mod <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
# test significance of fixed effect for time 
KRmodcomp(full_mod, null_mod)
