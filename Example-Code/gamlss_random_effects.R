library(gamlss)
data("Leukemia")
# fit Gamma GAM with cubic spline on age
gam_mod_re <- gamlss(height ~ cs(age) + treatment + random(case), 
                     data = Leukemia, 
                     family = GAF(), 
                     trace = FALSE)
# fit Gamma GAM with no smoother on age
gam_mod_re_nosmooth <- gamlss(height ~ age + treatment + random(case), 
                              data = Leukemia, 
                              family = GAF(), 
                              trace = FALSE)
# compare models via generalized AIC
GAIC(gam_mod_re, gam_mod_re_nosmooth)
# compare models via pseudo-R-squared 
Rsq(gam_mod_re); Rsq(gam_mod_re_nosmooth)
