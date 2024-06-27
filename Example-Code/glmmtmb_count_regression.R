library(glmmTMB)
data("epil2")
# fit poisson model
gmix_mod <- glmmTMB(y ~ trt*Base + Age + (1 + Visit | subject), 
                    data = epil2, 
                    family = poisson(link = "log"))
# fit negative-binomial model 
gmix_mod_nb <- glmmTMB(y ~ trt*Base + Age + (1 + Visit | subject), 
                       data = epil2, 
                       family = nbinom2(link = "log"))
# extract estimated dispersion parameter from negative-binomial model
summary(gmix_mod_nb)$sigma
