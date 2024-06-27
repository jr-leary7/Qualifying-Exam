library(lmtest)
library(sandwich)
set.seed(312)
# generate covariates
X1 <- rnorm(50, sd = 2)
X2 <- rpois(50, lambda = 5)
X3 <- rgamma(50, shape = 0.5)
X <- cbind(X1, X2, X3)
# set true coefficients
betas <- c(1.75, 4.2, -3.1)
# simulate response
y <- X %*% betas + rnorm(50)
# fit linear model
lmod <- lm(y ~ X)
# test significance of coefficients
coeftest(lmod, vcov. = sandwich(lmod))
# test significance of entire model
waldtest(lmod, vcov = sandwich(lmod))
# generate confidence intervals for coefficients 
coefci(lmod, vcov. = sandwich(lmod))
