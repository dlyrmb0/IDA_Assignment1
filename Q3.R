# Question 3
setwd("C:/Users/lenovo/Desktop/IDA_Assignment1")

load("dataex3.Rdata")

sigma <- 1.5  

logLikelihood <- function(mu, x, r, sigma) {
    lower_phi <- dnorm(x, mean = mu, sd = sigma)
    upper_phi <- pnorm(x, mean = mu, sd = sigma)
    
    return(-(sum(r*log(lower_phi) + (1 - r)*log(upper_phi)))) 
}

result <- optim(par = 0, fn = logLikelihood, x=dataex3$X, r=dataex3$R, sigma = sigma)

mu_mle <- result$par
print(mu_mle)
