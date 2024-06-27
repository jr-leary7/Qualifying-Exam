library(VGAM)
library(dplyr)
# read in & clean data 
alligators <- read.delim("https://users.stat.ufl.edu/~aa/glm/data/Alligators.dat",
                         header = TRUE, 
                         sep = "") %>% 
              mutate(size = factor(size), lake = factor(lake))
# fit nominal logistic regression model
nom_mod <- vglm(cbind(y2, y3, y4, y5, y1) ~ size + lake, 
                family = multinomial(), 
                data = alligators)
# test goodness-of-fit using deviance 
p_val <- 1 - pchisq(deviance(nom_mod), df = df.residual(nom_mod))
