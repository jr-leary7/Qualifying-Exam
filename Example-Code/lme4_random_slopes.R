library(lme4)
data("sleepstudy")
# fit model with time as a fixed effect
mix_mod <- lmer(Reaction ~ Days + (1 | Subject),
                data = sleepstudy, 
                REML = FALSE)
# fit model with a random slope on time
mix_mod_2 <- lmer(Reaction ~ (1 + Days | Subject), 
                  data = sleepstudy, 
                  REML = FALSE) 
# compare models with AIC 
AIC(mix_mod, mix_mod_2)
