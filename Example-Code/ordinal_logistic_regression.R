library(MASS)
library(VGAM)
# read in data
mental <- read.delim("https://users.stat.ufl.edu/~aa/glm/data/Mental.dat", 
                     header = TRUE, 
                     sep = "")
# fit proportional odds model
po_mod <- polr(factor(impair) ~ life + ses, 
               data = mental, 
               Hess = TRUE)
# test goodness-of-fit
p_val <- 1 - pchisq(deviance(po_mod), df = df.residual(po_mod))
# fit model without proportional odds assumption
ord_mod <- vglm(impair ~ life + ses, 
                family = cumulative(),
                data = mental)
# test goodness-of-fit
p_val_ord <- 1 - pchisq(deviance(ord_mod), df = df.residual(ord_mod))
