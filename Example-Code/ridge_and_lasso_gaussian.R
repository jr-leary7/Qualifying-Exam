library(glmnet)
set.seed(312)
# simulate X_j having true relationships with y 
X1 <- rnorm(100)
X2 <- rpois(100, lambda = 3)
X3 <- runif(100)
y <- 3 * X1 - 2.2 * X2 + 4 * X3 + rnorm(100, sd = 0.5)
X_true <- cbind(X1, X2, X2)
# simulate other covariates having no relationship to y
X_fake <- matrix(rnorm(100 * 97), nrow = 100, ncol = 97)
X_all <- cbind(X_true, X_fake)
# fit LASSO model
lasso_mod <- glmnet(X_all, 
                    y, 
                    family = gaussian(), 
                    alpha = 1, 
                    nlambda = 50)
# fit ridge regression model
ridge_mod <- glmnet(X_all, 
                    y, 
                    family = gaussian(), 
                    alpha = 0, 
                    nlambda = 50)
