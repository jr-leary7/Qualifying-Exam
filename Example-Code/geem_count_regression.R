library(geeM)
library(dplyr)
data("seizure", package = "geepack")
# add subject ID & exposure variables & pivot data to long format from wide 
seizure <- mutate(seizure, id = paste0("S", row_number())) %>% 
           tidyr::pivot_longer(cols = c(base, starts_with("y")), 
                               names_to = "period", 
                               values_to = "seizures") %>%
           mutate(exposure = if_else(period == "base", 8, 2))
# fit Poisson model with exchangeable correlation structure
mod_gee <- geem(seizures ~ offset(log(exposure)) + age*trt, 
                id = seizure$id, 
                data = seizure, 
                family = poisson(link = "log"), 
                corstr = "exchangeable")
# fit Negative-binomial model with same correlation structure
mod_gee_nb <- geem(seizures ~ offset(log(exposure)) + age*trt, 
                   id = seizure$id, 
                   data = seizure, 
                   family = MASS::negative.binomial(theta = 1, link = "log"), 
                   corstr = "exchangeable")
