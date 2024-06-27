library(geepack)
data("ohio")
# fit model with exchangeable correlation structure 
gee_exch <- geeglm(resp ~ age*smoke, 
                   id = id, 
                   data = ohio, 
                   family = binomial(link = "logit"),
                   corstr = "exchangeable", 
                   scale.fix = FALSE)
# fit model with ar-1 correlation structure 
gee_ar1 <- geeglm(resp ~ age*smoke, 
                  id = id, 
                  data = ohio, 
                  family = binomial(link = "logit"),
                  corstr = "ar1", 
                  scale.fix = FALSE)
# compare correlation structures with qic 
QIC(gee_exch, gee_ar1)
