library(INLA)
library(dplyr)
data("Surg")
# clean up data
Surg <- rename(Surg, 
               Cases = n, 
               Deaths = r, 
               Hospital = hospital)
# fit Bayesian logistic GLMM with random intercept for hospital
mix_mod_bayes <- inla(Deaths ~  f(Hospital, model = "iid"), 
                      family = "binomial",
                      Ntrials = Cases,  
                      data = Surg, 
                      control.predictor = list(compute = TRUE),
                      control.compute = list(dic = TRUE, return.marginals.predictor = TRUE))
# extract random effects per each hospital
mix_mod_bayes$summary.random$Hospital
# extract summary of fixed effects (just the intercept)
mix_mod_bayes$summary.fixed 
# extract model DIC
mix_mod_bayes$dic 
