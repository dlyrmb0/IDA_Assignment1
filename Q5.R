# Question 4
setwd("C:/Users/lenovo/Desktop/IDA_Assignment1")

load("dataex5.Rdata")

# install.packages("maxLik")
library(maxLik)  # Assuming maxLik for optimization

logitLikelihood <- function(beta, x, y) {
  p <- exp(beta[1] + beta[2] * x) / (1 + exp(beta[1] + beta[2] * x))
  return(-sum(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE))  # Negative for minimization
}

# EM algorithm skeleton
beta_init <- c(0, 0)  # Initial guesses for beta0 and beta1
convergence_threshold <- 1e-6
max_iter <- 1000
iter <- 0
converged <- FALSE

while(!converged && iter < max_iter) {
  iter <- iter + 1
  
  # E-step: Estimate missing Y based on current beta estimates
  # For logistic regression, this is just calculating p_i(beta)
  expected_y <- exp(beta_init[1] + beta_init[2] * dataex5$X) / (1 + exp(beta_init[1] + beta_init[2] * dataex5$X))
  dataex5$Y[is.na(dataex5$Y)] <- expected_y[is.na(dataex5$Y)]
  
  # M-step: Maximize likelihood with current Y estimates (observed + expected)
  ml_result <- maxLik(logitLikelihood, start = beta_init, x = dataex5$X, y = dataex5$Y)
  beta_update <- coef(ml_result)
  
  # Check for convergence
  if (max(abs(beta_update - beta_init)) < convergence_threshold) {
    converged <- TRUE
  } else {
    beta_init <- beta_update
  }
}

if (converged) {
  cat("Converged after", iter, "iterations.\n")
  print(beta_update)
} else {
  cat("Did not converge within the maximum number of iterations.\n")
}
