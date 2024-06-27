library(MASS)
data("gala", package = "faraway")
# fit Poisson model
mod_p <- glm(Species ~ ., 
             data = gala, 
             family = poisson(link = "log"))
# test fit of Poisson model
p_val_p <- 1 - pchisq(sum(residuals(mod_p , type = "pearson")^2), df = df.residual(mod_p))
# estimate dispersion parameter
disp_est <- sum(residuals(mod_p , type = "pearson")^2) / mod_p$df.df.residual
# fit NB model
mod_nb <- glm.nb(Species ~ ., 
                 data = gala, 
                 link = "log")
# test fit of NB model
p_val_nb <- 1 - pchisq(deviance(mod_nb), df = df.residual(mod_nb))
# test overdispersion
pscl::odTest(mod_nb)
