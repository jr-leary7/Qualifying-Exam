set.seed(312)
# simulate covariates and Gaussian response
X <- matrix(rnorm(500), nrow = 100)
beta <- matrix(c(-3, 2, 0.5, 6, -1), nrow = 5)
y <- X %*% beta + rnorm(100)
# fit linear model
lmod <- lm(y ~ X)
# extract residuals & measures of influence 
resid_pearson <- residuals(lmod)
resid_standardized <- rstandard(lmod)
resid_jackknife <- rstudent(lmod)
leverages <- influence(lmod)$hat
cooks_distances <- cooks.distance(lmod)
