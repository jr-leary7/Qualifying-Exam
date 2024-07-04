set.seed(312)
# generate heteroskedastic data 
X <- seq(0, 10, length.out = 50)
beta <- 3.45
y <- beta * X + rnorm(50, 0, 1 + 0.5 * X)
# fit OLS model 
lmod <- lm(y ~ X)
# model variance 
var_mod <- lm(abs(residuals(lmod)) ~ fitted.values(lmod))
sigma2_est <- predict(var_mod)^2
# compute weights 
weights <- 1 / sigma2_est
# fit WLS model
wls_mod <- lm(y ~ X, weights = weights)
# compare models via adjusted R-squared
summary(lmod)$adj.r.squared; summary(wls_mod)$adj.r.squared
# compare models via AIC 
AIC(lmod); AIC(wls_mod)
