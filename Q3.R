load("dataex3.Rdata") # Load the dataset 

sigma <- 1.5 # Set the known standard deviation sigma to 1.5 

# Define the log-likelihood function for the given model
logLikelihood <- function(mu, x, r, sigma) {
    lower_phi <- dnorm(x, mean = mu, sd = sigma)
    upper_phi <- pnorm(x, mean = mu, sd = sigma)
    return(-(sum(r*log(lower_phi) + (1 - r)*log(upper_phi)))) 
}

# Use the 'optim' function to find the MLE of mu by minimizing the negative log-likelihood
result <- optim(par = 0, fn = logLikelihood, x=dataex3$X, r=dataex3$R, sigma = sigma)

# Extract the MLE of mu from the optimization result
mu_mle <- result$par
print(paste("The maximum likelihood estimate of mu is", mu_mle))