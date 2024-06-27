library(dplyr)
data(orings, package = "faraway")
# clean data
orings <- mutate(orings, nodamage = 6 - damage)
# fit logistic regression model
logit_mod <- glm(cbind(damage, nodamage) ~ temp, 
                 family = binomial(link = "logit"), 
                 orings)
# test goodness-of-fit
p_val <- 1 - pchisq(sum(residuals(logit_mod, type = "pearson")^2), df.residual(logit_mod))
# estimate dispersion
disp_est <- sum(residuals(logit_mod, type = "pearson")^2) / df.residual(logit_mod)
# fit logistic regression model with variable dispersion
logit_mod_2 <- glm(cbind(damage, nodamage) ~ temp, 
                   family = quasibinomial(link = "logit"), 
                   orings)
