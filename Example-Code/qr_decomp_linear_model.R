set.seed(312)
# simulate covariates and Gaussian response
X <- matrix(rnorm(500), nrow = 100)
beta <- matrix(c(-3, 2, 0.5, 6, -1), nrow = 5)
y <- X %*% beta + rnorm(100)
# fit linear model
lmod <- lm(y ~ X)
# add intercept to design matrix - this is required 
X_design <- cbind(1, X)
# use QR decomposition to estimate coefficients
qr_decomp <- qr(X_design)
qr_coef <- as.numeric(solve(qr.R(qr_decomp), t(qr.Q(qr_decomp)) %*% y))
# check equality with results from lm()
all.equal(as.numeric(coef(lmod)), qr_coef)
