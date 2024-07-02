set.seed(312)
# simulate X and y
n <- 100
X <- rnorm(100)
beta_true <- 3.5
sigma_true <- 1
y <- beta_true * X + rnorm(100, mean = 0, sd = sigma_true)
# simulate missing values
y_missing <- y
y_missing[sample(1:100, size = 10)] <- NA
# provide initial estimates 
beta_est <- 0
sigma_est <- 0
max_iter <- 100
tolerance <- 1e-6
# EM algorithm
for (i in seq(max_iter)) {
  # E-step -- estimate missing values
  y_est <- y_missing
  y_est[is.na(y_est)] <- beta_est * X[is.na(y_est)]
  # M-step -- update estimates for beta and sigma
  beta_new <- sum((y_est - mean(y_est)) * (X - mean(X))) / sum((X - mean(X))^2)
  sigma_new <- sqrt(sum((y_est - beta_new * X)^2) / n)
  # check convergence
  if (abs(beta_new - beta_est) < tolerance && abs(sigma_new - sigma_est) < tolerance) {
    break
  }
  beta_est <- beta_new
  sigma_est <- sigma_new
}
