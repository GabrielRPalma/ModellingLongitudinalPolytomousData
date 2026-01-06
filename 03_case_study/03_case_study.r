####################################################################################################
###
### File:    03_case_study.r
### Purpose: Analyse of the dataset 
### Authors: Gabriel Rodrigues Palma
### Date:    12/01/24
###
####################################################################################################
# Load packages -----
source('00_source.r')

# Pre processed the data -----
source('01_data_preprocessing/01_data_preprocessing.r')
processed_data %>%
  group_by(Data) %>%
  count()

####################################################################################################
#################### Model with interaction ########################################################
####################################################################################################
processed_data <- processed_data %>%
  filter(!(Data %in% c('06/05/2019', '13/05/2019', '29/04/2019')))
processed_data$Intercept <- rep(1, nrow(processed_data))
processed_data <- 
  processed_data %>%
  dplyr::select(-c(Sexo_code_1, Bloco_code_1, Enriquecimento_code_1))
# Add interaction in the model
processed_data <- processed_data %>%
  mutate(interaction_sexo2_enriquecimento_2 = Sexo_code_2 * Enriquecimento_code_2,
         interaction_sexo2_enriquecimento_3 = Sexo_code_2 * Enriquecimento_code_3)

Y <- split(processed_data[, 8:14], list(processed_data$Data))
Y_matrices <- lapply(Y, as.matrix)
Y <- array(unlist(Y), dim = c(72, 7, 12))

X <- split(processed_data[, c(3:7, 15, 16, 17)], list(processed_data$Data))
X_matrices <- lapply(X, as.matrix)
X <- array(unlist(X), dim = c(72, 8, 12))

# Implement the bayesian multinomial longitudinal regression -----

N <- 72 # Number of observations
J <- 7 # Number of categories
S <- apply(Y, MARGIN = 3, FUN = function(x) apply(x, MARGIN = 1, sum))
C <- 8 # Number of covariates
K <- 12 # Time

# Applying the multinomial longitudinal regression -----
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

# DIC = 10154.5

# Results and output of the simulated example, to include convergence checking, output plots, interpretation etc
# Encontrar as cadeias -> Cadeia de cada fator e ache a cadeia da diferenca a partir dos quantins
model_run$BUGSoutput$DIC
model_summary <- as.data.frame(model_run$BUGSoutput$summary)
hist(model_summary$Rhat)
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

sink('output_data/case_study_bayesian_multinomial_longitudinal_regression_parameter.csv')
write.csv(model_summary)
sink()

####################################################################################################
#################### Model without interaction ########################################################
####################################################################################################
source('01_data_preprocessing/01_data_preprocessing.r')
processed_data %>%
  group_by(Data) %>%
  count()

processed_data <- processed_data %>%
  filter(!(Data %in% c('06/05/2019', '13/05/2019', '29/04/2019')))
processed_data$Intercept <- rep(1, nrow(processed_data))
processed_data <- 
  processed_data %>%
  dplyr::select(-c(Sexo_code_1, Bloco_code_1, Enriquecimento_code_1))

Y <- split(processed_data[, 8:14], list(processed_data$Data))
Y_matrices <- lapply(Y, as.matrix)
Y <- array(unlist(Y), dim = c(72, 7, 12))

X <- split(processed_data[, c(3:7, 15)], list(processed_data$Data))
X_matrices <- lapply(X, as.matrix)
X <- array(unlist(X), dim = c(72, 6, 12))

# Implement the bayesian multinomial longitudinal regression -----

N <- 72 # Number of observations
J <- 7 # Number of categories
S <- apply(Y, MARGIN = 3, FUN = function(x) apply(x, MARGIN = 1, sum))
C <- 6 # Number of covariates
K <- 12 # Time

# Applying the multinomial longitudinal regression -----
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
model_run$BUGSoutput$DIC
# DIC = 10158.1 (No interaction)
# DIC = 10154.5 (With interaction)