library(brms)
library(dplyr)
set.seed(312)
# simulate data with an interaction effect
model_data <- data.frame(X1 = rnorm(100, mean = 3, sd = 3), 
                         X2 = as.factor(sample(c("A", "B"), size = 100, replace = TRUE)), 
                         X3 = rpois(100, lambda = 4)) %>% 
              rowwise() %>% 
              mutate(mu = if_else(X2 == "A", 3 * X1 - 2.5 * X3, 2.25 * X1 + 0.5 * X3^2), 
                     y = rnorm(1, mean = mu)) %M% 
              ungroup()
# fit model without interaction
lmod <- brm(y ~ X1 + X2 + X3, 
            data = model_data, 
            silent = 2, 
            seed = 312, 
            cores = 4)
# fit model with interaction
lmod_int <- brm(y ~ X1 + X2*X3, 
                data = model_data, 
                silent = 2, 
                seed = 312, 
                cores = 4)
# estimate Bayes factor for interaction model 
2 * log10(bayes_factor(lmod_int, lmod)$bf)
