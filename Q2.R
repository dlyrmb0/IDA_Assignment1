load("dataex2.Rdata") # Load the dataset 

# install.packages("mice")
library(mice)

# Initialize parameters and counters
beta1 <- 3  
n <- 100 
m <- 20 

# Initialize counters for tracking coverage
coverage_count_stochastic <- 0
coverage_count_bootstrap <- 0

for (i in 1:n) {
  # Convert the current dataset to a dataframe and name its columns
  data2 <- as.data.frame(dataex2[, , i])
  names(data2) <- c("x", "y")
  
  # Perform stochastic imputation
  imp_stochastic <- mice(data2, m = m, method = 'norm.nob', seed = 1, print = FALSE)
  model_stochatsic <- with(imp_stochastic, lm(y ~ x))
  pool_stochastic <- pool(model_stochatsic)
  summary_stochastic <- summary(pool_stochastic, conf.int = TRUE)
    
  # Check if the true beta1 is within the 95% CI of the stochastic model
  if (summary_stochastic[2,7] <= beta1 && summary_stochastic[2,8] >= beta1) {
    coverage_count_stochastic <- coverage_count_stochastic + 1
  }
  
  # Repeat the process for bootstrap imputation
  imp_bootstrap <- mice(data2, m = m, method = 'norm.boot', seed = 1, print = FALSE)
  model_bootstrap <- with(imp_bootstrap, lm(y ~ x))
  pool_bootstrap <- pool(model_bootstrap)
  summary_bootstrap <- summary(pool_bootstrap, conf.int = TRUE)
  
  # Check if the true beta1 is within the 95% CI of the bootstrap model
  if (summary_bootstrap[2,7] <= beta1 && summary_bootstrap[2,8] >= beta1) {
    coverage_count_bootstrap <- coverage_count_bootstrap + 1
  }
}

# Calculate and print the empirical coverage probabilities
empirical_coverage_prob_stochastic <- coverage_count_stochastic / n
print(paste("Empirical coverage probability (Stochastic):", empirical_coverage_prob_stochastic))
empirical_coverage_prob_bootstrap <- coverage_count_bootstrap / n
print(paste("Empirical coverage probability (Bootstrap):", empirical_coverage_prob_bootstrap))










