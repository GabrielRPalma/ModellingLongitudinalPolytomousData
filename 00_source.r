####################################################################################################
###
### File:    00_source.R
### Purpose: Load required packages and functions used to 
###          create interactive maps.
### Authors: Gabriel Rodrigues Palma
### Date:    12/01/24
###
####################################################################################################

# packages required ---------------------------
packages <- c('tidyverse', # data-wrangling + plotting
              'here', # efficient file structures
              'msm', # Transition models
              'gganimate', 
              'reshape2', 
              'gifski',
              'heatmaply', 
              'moments', 
              'mclust', 
              'glue', 
              'rjags',
              'psych', 
              'GPArotation', 
              'ggfortify', 
              'lavaan', 
              'ggridges', 
              'xtable', 
              'ggExtra',
              'R2jags'
              
              
)

install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Main functions ---------------------------
construct_dataset <- function(covariates, response, 
                              processed_data, N, K, C, J){
  # This function produces the a dataset ready to be used for the
  #bayesian model. 
  #Input:
  #      covariates: The selected covariates to be used in the model
  #      response: The response variables used in the model
  #      processed_data: The dataset containing all variables
  #      N: The number of observations available for the dataset
  #      K: The number of times that the experiment was replicated
  #Output:
  #      X: An array with the covariates
  #      Y: An array with the response variables
  #      C: The number of covariates used
  #      J: The number of categories used for the response variable
  #      S: The total frequency of all categories J
  C <- ncol(covariates)
  J <- ncol(response)
  
  Y <- split(response, list(processed_data$Data))
  Y_matrices <- lapply(Y, as.matrix)
  Y <- array(unlist(Y), dim = c(N, J, K))
  
  X <- split(covariates, list(processed_data$Data))
  X_matrices <- lapply(X, as.matrix)
  X <- array(unlist(X), dim = c(N, C, K))
  
  S <- apply(Y, MARGIN = 3, FUN = function(x) apply(x, MARGIN = 1, sum))
  
  result <- list()
  result$X <- X
  result$Y <- Y
  result$C<- C
  result$J <- J
  result$S <- S
  
  return(result)
}

fit_bayesian_multinomial_longitudinal_model <- function(N, Y, X,
                                                        S, K, J,
                                                        C){
  # This function fits the bayesian multinomial longitudinal model.
  #Input:
  #      N: The number of observations available for the dataset
  #      K: The number of times that the experiment was replicated
  #      X: An array with the covariates
  #      Y: An array with the response variables
  #      C: The number of covariates used
  #      J: The number of categories used for the response variable
  #      S: The total frequency of all categories J
  #Output:
  #      model_run: The output of the JAGS function with the model results
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
          for (j in 1:J){
            beta[j, c, k] ~ dnorm(0, 0.1^-2)
          }}}
      
    }
    "
  
  model_data <- list(N = N, y = Y, x = X, S = S, K = K, J = J, C = C)
  
  # Choose the parameters to watch
  model_parameters <- c("beta", "p", "u")
  # Run the model
  model_run <- jags(
    data = model_data,
    parameters.to.save = model_parameters,
    model.file = textConnection(model_code),
    n.chains=4, # Number of different starting positions
    n.iter=5000, # Number of iterations
    n.burnin=1000, # Number of iterations to remove at start
    n.thin=15
  )
  DIC <- model_run$BUGSoutput$DIC
  model_summary <- as.data.frame(model_run$BUGSoutput$summary)
  
  names <- rownames(model_summary)
  model_summary$parameters <- names
  model_summary <- model_summary %>% 
    mutate(Parm = str_split(parameters, "\\[|,|\\]", simplify = TRUE)[,1], 
           Individual = str_split(parameters, "\\[|,|\\]", simplify = TRUE)[,2], 
           Covariate = str_split(parameters, "\\[|,|\\]", simplify = TRUE)[,3], 
           Time = str_split(parameters, "\\[|,|\\]", simplify = TRUE)[,4]) %>%
    dplyr::select(-parameters)
  model_summary <- model_summary %>%
    dplyr::select(Parm, mean, `2.5%`, `50%`, `97.5%`, Individual, Covariate, Time)
  colnames(model_summary) <- c('Parameter', 'average', 'lower', 'mode', 'higher', 'Category', 'Covariate', 'Time')
  
  result <- list()
  result$model_summary <- model_summary
  result$DIC <- DIC
  result$model_run <- model_run
  
  return(result)
}

# plot settings ---------------------------
pallete = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]

theme_new <- function(base_size = 20, base_family = "Arial"){
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(size = 20, colour = "grey30"),
      legend.key=element_rect(colour=NA, fill =NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks =         element_line(colour = "grey20"),
      plot.title.position = 'plot',
      legend.position = "bottom"
    )
}
