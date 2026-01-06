####################################################################################################
###
### File:    02_bayesian_multinomial_longitudinal_regression.r
### Purpose: Prepare the dataset for being processed using Rjags functions.
### Authors: Gabriel Rodrigues Palma
### Date:    12/01/24
###
####################################################################################################
# Load packages -----
source('00_source.r')

# Load 
# Install and load necessary packages

data1 <- c(1,2,3,4,5,6)
data2 <- c(18, 12, 13, 14, 19)

# Create a 3D array
result <- array(c(data1, data2), dim = c(4,3,2))
result[1, ,1]
result[, ,1]

# Creating a bayesian multinomial longitudinal regression model -----
N <- 10 # Number of observations
J <- 3 # Number of categories
S <- cbind(rpois(N, 20) + 1, 
           rpois(N, 20) + 1) # Sum of values (different for each observation)
C <- 2 # Number of covariates
K <- 2 # Time

softmax <- function(x) exp(x) / sum(exp(x))

beta <- array(rnorm(J * C * K), dim = c(J, C, K))
x <- array(rnorm(N * C * K), dim = c(N, C, K))
xc <- rep(c('sleep', 'awake'), 5)
p <- y <- array(NA, dim = c(N, J, K))
for (i in 1:N) {
  for (k in 1:K){
    p[i, ,k] <- softmax(beta[, , k] %*% x[i, , k])
    y[i, ,k] <- rmultinom(1, size = S[i, k], prob = p[i, ,k]) 
  }
}

model_code <- "
model
{
  # Likelihood
  for (i in 1:N) {# Observaton loop
    for (k in 1:K){ # Longitudinal loop
      
      y[i, , k] ~ dmulti(p[i, , k], S[i, k])
      for(j in 1:J) { # Category loop
        
        exp_z[i, j, k] <- exp(z[i, j, k])
        p[i, j, k] <- exp_z[i, j, k]/sum(exp_z[i, , k])
        z[i, j, k] <- beta[j, , k] %*% x[i, , k] + u[i]
      
      }
    }
    
  }
  # Prior
  for (i in 1:N){
    u[i] ~ dnorm(0, 0.1^-2)
  }
  
  
  for (c in 1:C){
    for(k in 1:K) {
      for(j in 1:J) {
        beta[j, c, k] ~ dnorm(0, 0.1^-2)
      }}}
  
}
"

model_code <- "
model
{
  # Likelihood
  for (i in 1:N) {# Observaton loop
    for (k in 1:K){ # Longitudinal loop
      
      y[i, , k] ~ dmulti(p[i, , k], S[i, k])
      for(j in 1:J) { # Category loop
        
        exp_z[i, j, k] <- exp(z[i, j, k])
        p[i, j, k] <- exp_z[i, j, k]/sum(exp_z[i, , k])
        z[i, j, k] <- beta[j, , k] %*% x[i, , k] + u[i]
      
      }
    }
    
  }
  # Prior
  for (i in 1:N){
    u[i] ~ dnorm(0, 0.1^-2)
  }
  
  
  for (c in 1:C){
    for(k in 1:K) {
        beta[1:J, c, k] ~ ddirch(rep(1 , J))}}
  
}
"

model_data <- list(N = N, y = y, x = x, S = S, K = K, J = J, C = C)

# Choose the parameters to watch
model_parameters <- c("beta", "p")

# Run the model
model_run <- jags(
  data = model_data,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code)
)

# Results and output of the simulated example, to include convergence checking, output plots, interpretation etc
plot(model_run)
print(model_run)

# Compare the predicted vs true values of beta
model_run$BUGSoutput$mean$beta
beta

# However you're better off lookin at the predicted probabilities as these
# will be more directly comparable
p_pred <- model_run$BUGSoutput$mean$p
head(cbind(p[, 1, 1], p_pred[, 1]), 20)


