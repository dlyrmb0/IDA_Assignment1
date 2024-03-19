# Question 2
setwd("C:/Users/lenovo/Desktop/IDA_Assignment1")

load("dataex2.Rdata")

# install.packages("mice")
library(mice)

beta1 <- 3  
n <- 100 
m <- 20 

coverage_count_stochastic <- 0
coverage_count_bootstrap <- 0

for (i in 1:n) {
  data2 <- as.data.frame(dataex2[, , i])
  names(data2) <- c("x", "y")
  
  imp_stochastic <- mice(data2, m = m, method = 'norm.nob', seed = 1, print = FALSE)
  model_stochatsic <- with(imp_stochastic, lm(y ~ x))
  pool_stochastic <- pool(model_stochatsic)
  summary_stochastic <- summary(pool_stochastic, conf.int = TRUE)
    
  if (summary_stochastic[2,7] <= beta1 && summary_stochastic[2,8] >= beta1) {
    coverage_count_stochastic <- coverage_count_stochastic + 1
  }
  
  imp_bootstrap <- mice(data2, m = m, method = 'norm.boot', seed = 1, print = FALSE)
  model_bootstrap <- with(imp_bootstrap, lm(y ~ x))
  pool_bootstrap <- pool(model_bootstrap)
  summary_bootstrap <- summary(pool_bootstrap, conf.int = TRUE)
  
  if (summary_bootstrap[2,7] <= beta1 && summary_bootstrap[2,8] >= beta1) {
    coverage_count_bootstrap <- coverage_count_bootstrap + 1
  }
}

empirical_coverage_prob_stochastic <- coverage_count_stochastic / n
print(paste("Empirical coverage probability (Imputation):", empirical_coverage_prob_stochastic))
empirical_coverage_prob_bootstrap <- coverage_count_bootstrap / n
print(paste("Empirical coverage probability (Bootstrap):", empirical_coverage_prob_bootstrap))










