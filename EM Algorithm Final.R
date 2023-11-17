library(hawkes)

# Simulate data from a Hawkes process with exponential decay
set.seed(123)
lambda0 <- 0.1 # base intensity
alpha <- 0.5 # excitation parameter
beta <- 1 # decay parameter
T <- 100 # end time
data <- simulateHawkes(lambda0, alpha, beta, T)

# Define log-likelihood function for Hawkes process with exponential decay
loglik_hawkes_exp <- function(theta, data) {
  lambda0 <- theta[1]
  alpha <- theta[2]
  beta <- theta[3]
  n <- length(data)
  ll <- -lambda0*T # log-likelihood of base intensity
  mu <- rep(lambda0, n) # initialize intensity function
  for (i in 2:n) {
    mu[i] <- lambda0 + alpha*sum(exp(-beta*(data[i]-data[1:(i-1)])))
    ll <- ll + log(mu[i]) - mu[i]*(data[i]-data[i-1]) # log-likelihood of each event time
  }
  return(ll)
}

# Run EM algorithm to estimate parameters
theta_init <- c(0.2, 0.3, 0.8) # initial parameter estimates
theta_est <- em(loglik_hawkes_exp, theta_init, data, control=list(trace=TRUE))$estimate # run EM algorithm
theta_est # print estimated parameters
