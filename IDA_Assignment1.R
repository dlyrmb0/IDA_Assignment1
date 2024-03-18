setwd("C:/Users/Sharon/Desktop/IDA")

load("dataex2.Rdata")

# install.packages("mice")
library(mice)

true_beta1 <- 3  # The true value of β1
n_datasets <- 100  # Total number of datasets
m <- 20  # Number of imputations or bootstrap samples per dataset

# For Stochastic Regression Imputation
coverage_count_imp <- 0
for (i in 1:n_datasets) {
  dataset <- as.data.frame(dataex2[, , i])
  names(dataset) <- c("X", "Y")
  
  imp <- mice(dataset, m = m, method = 'norm.predict', seed = 1, print = FALSE)
  
  for (j in 1:m) {
    completed_data <- complete(imp, j)
    fit <- lm(Y ~ X, data = completed_data)
    ci <- confint(fit)[2, ]  # Assuming beta_1 is the second coefficient
    
    if (ci[1] <= true_beta1 && ci[2] >= true_beta1) {
      coverage_count_imp <- coverage_count_imp + 1
    }
  }
}
empirical_coverage_prob_imp <- coverage_count_imp / (n_datasets * m)
print(paste("Empirical coverage probability (Imputation):", empirical_coverage_prob_imp))

# For Bootstrap-Based Version (Adjusted for one dataset example)
coverage_count_boot <- 0
n_bootstraps <- m  # Reusing 'm' for the number of bootstrap samples

dataset <- as.data.frame(dataex2[, , 1])  # Example: Using the first dataset
names(dataset) <- c("X", "Y")

for (i in 1:n_bootstraps) {
  sample_indices <- sample(nrow(dataset), replace = TRUE)
  bootstrap_sample <- dataset[sample_indices, ]
  
  fit <- lm(Y ~ X, data = bootstrap_sample)
  ci <- confint(fit)["X", ]
  
  if(ci[1] <= true_beta1 && ci[2] >= true_beta1) {
    coverage_count_boot <- coverage_count_boot + 1
  }
}
empirical_coverage_prob_boot <- coverage_count_boot / n_bootstraps
print(paste("Empirical coverage probability (Bootstrap):", empirical_coverage_prob_boot))

load("dataex3.Rdata")

# Assume data is loaded and sigma squared is known
sigma <- 1.5  # Known standard deviation
D <-  # You need to specify the censoring limit D based on your dataset or context
  
logLikelihood <- function(mu, data, D, sigma) {
    uncensored <- data$R == 1
    censored <- data$R == 0
    
    ll_uncensored <- sum(dnorm(data$X[uncensored], mean = mu, sd = sigma, log = TRUE))
    ll_censored <- sum(pnorm(D, mean = mu, sd = sigma, log.p = TRUE, lower.tail = TRUE))
    
    return(-(ll_uncensored + ll_censored))  # Return negative log-likelihood for minimization
  }

# Optimizing
result <- optim(par = 0, fn = log_likelihood, x = dataex3$X, r = dataex3$R, sigma = sigma)

# MLE of mu
mu_mle <- result$par
print(mu_mle)
 
load("dataex5.Rdata")


# install.packages("maxLik")
library(maxLik)  # Assuming maxLik for optimization

# Placeholder for your data loading step
# Assume `data` is loaded from "dataex5.Rdata" with variables x (covariate) and y (response, with NAs)

logitLikelihood <- function(beta, x, y) {
  p <- exp(beta[1] + beta[2] * x) / (1 + exp(beta[1] + beta[2] * x))
  ll <- sum(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
  return(-ll)  # Negative for minimization
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

