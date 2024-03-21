load("dataex5.Rdata") # Load the dataset
#require(maxLik)

data1 <- na.omit(dataex5) # Cases with complete data
x1 <- data1[,1]
y1 <- data1[,2]

data2 <- dataex5[is.na(dataex5[,2]), ] # Cases with missing Y values
x2 <- data2[,1]
y2 <- data2[,2]

# Initialize parameters
beta0 <- c(0, 0)
beta <- beta0
eps <- 1e-8
diff <- 1

# Loop for the EM algorithm until convergence
while (diff > eps) {
  beta_old <- beta
  
  # E-step: Calculate the conditional expectation of missing data
  logLikelihood <- function(beta, beta_old) {
    pi1 <- exp(beta[1] + x1 * beta[2])/(1+exp(beta[1] + x1 * beta[2]))
    pi2 <- exp(beta[1] + x2 * beta[2])/(1+exp(beta[1] + x2 * beta[2]))
    pi2_old <- exp(beta_old[1] + x2 * beta_old[2])/(1+exp(beta_old[1] + x2 * beta_old[2]))
    return(-(sum(y1 * log(pi1) + (1 - y1) * log(1 -  pi1))+sum(pi2_old * log(pi2) + (1 - pi2_old) * log(1 - pi2))))
  }
  
  # M-step: Update parameter estimates
  MLE <- optim(par = beta, fn = logLikelihood, beta_old=beta_old)
  beta <- MLE$par
  
  diff <- sum(abs(beta - beta_old)) # Check convergence condition
}

print(paste("The maximum likelihood estimate of beta_0 is", beta[1]))
print(paste("The maximum likelihood estimate of beta_1 is", beta[2]))
