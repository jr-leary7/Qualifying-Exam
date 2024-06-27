library(nlme)
data(psid, package = "faraway")
# fit full model including age
mix_mod <- lme(log(income) ~ age + educ + sex,
               random = ~ 1 + year | person, 
               data = psid, 
               correlation = corAR1(), 
               method = "ML")
# fit model without age
null_mod <- lme(log(income) ~ educ + sex,
                random = ~ 1 + year | person, 
                data = psid, 
                correlation = corAR1(), 
                method = "ML")
# compare models via F test
anova(mix_mod, null_mod)
# compare models via AIC 
AIC(mix_mod, null_mod)
