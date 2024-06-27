library(mgcv)
data("ozone", package = "faraway")
# fit poisson model with 3 smoothers
gam_mod <- gam(O3 ~ s(temp) + s(ibh) + s(ibt),
               family = poisson(link = "log"), 
               data = ozone)
# check diagnostics
gam.check(gam_mod)
# fit negative-binomial model to account for overdispersion
gam_mod_nb <- gam(O3 ~ s(temp) + s(ibh) + s(ibt),
                  family = nb(link = "log"), 
                  data = ozone)
# get final theta estimate
gam_mod_nb$family$getTheta(TRUE)
# plot first covariate versus its smoother
plot(gam_mod_nb, select = 1)
