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
processed_data$Intercept <- rep(1, nrow(processed_data))

########################FEMEA################################
#################################################################################################### 
##################### Comparing sex for Enriquecimento_code_1 ######################################
####################################################################################################
filtered_data <- processed_data %>%
  filter(Enriquecimento_code_1 == 1)

model_data <- construct_dataset(covariates = filtered_data[,c(
                           "Bloco_code_2", "Bloco_code_3", "Sexo_code_2", "Intercept")],
                              response = filtered_data[, 11:17], 
                              processed_data = filtered_data, 
                              N = 72, K = 12, C = 4, J = 7)

bayesian_model <- fit_bayesian_multinomial_longitudinal_model(N = 72, 
                                                              Y = model_data$Y,
                                                              X = model_data$X,
                                                              S = model_data$S, 
                                                              K = 12, 
                                                              J = model_data$J,
                                                              C = model_data$C)


# Checking for differences on sex
bayesian_model_parameters <- bayesian_model$model_summary
bayesian_model_parameters$Parameter <- factor(bayesian_model_parameters$Parameter)
beta_parameters <- bayesian_model_parameters %>%
  filter(Parameter == 'beta')
beta_parameters$Covariate <- factor(beta_parameters$Covariate)
levels(beta_parameters$Covariate) <- c("Bloco_code_2", "Bloco_code_3",
                                       "Femea", "Intercept")
beta_parameters$Factors <- beta_parameters$Covariate
levels(beta_parameters$Factors) <-  c("Bloco", "Bloco",         
                                      "Sexo", "Intercept")
beta_parameters$Category <- factor(beta_parameters$Category)

sex_data <- beta_parameters %>%
  filter(Factors == 'Sexo')
sex_data <- sex_data %>%
  mutate(Difference= ifelse(lower > 0, 'Yes', ifelse(higher < 0, 'Yes', 'No')))

sex_data$Time <- factor(sex_data$Time, levels = unique(sex_data$Time))

g1 <- sex_data %>%
  filter(Factors == 'Sexo') %>%
  ggplot(mapping = aes(x = average, y = Category,
                       colour = Difference)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, 
                    xmax = higher), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~factor(Time)) +
  theme_new() +
  theme(text = element_text(size = 16), # Definindo o tamanho da fonte global
                    axis.text = element_text(size = 12), # Tamanho da fonte nos textos dos eixos
                    axis.title = element_text(size = 16) # Tamanho da fonte nos títulos dos eixos
  ) +
  labs(y = "Type of behaviour", 
       x = expression(beta~posterior~distribution))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#232323","#C95910"))

ggsave("g1_femea.jpeg", plot = g1,
       width = 12, height = 8, dpi = 300)

#################################################################################################### 
##################### Comparing sex for Enriquecimento_code_2 ######################################
####################################################################################################
filtered_data <- processed_data %>%
  filter(Enriquecimento_code_2 == 1)

model_data <- construct_dataset(covariates = filtered_data[,c(
  "Bloco_code_2", "Bloco_code_3", "Sexo_code_2", "Intercept")],
  response = filtered_data[, 11:17], 
  processed_data = filtered_data, 
  N = 72, K = 12, C = 4, J = 7)

bayesian_model <- fit_bayesian_multinomial_longitudinal_model(N = 72, 
                                                             Y = model_data$Y,
                                                              X = model_data$X,
                                                              S = model_data$S, 
                                                              K = 12, 
                                                              J = model_data$J,
                                                              C = model_data$C)
# Checking for differences on sex
bayesian_model_parameters <- bayesian_model$model_summary
bayesian_model_parameters$Parameter <- factor(bayesian_model_parameters$Parameter)
beta_parameters <- bayesian_model_parameters %>%
  filter(Parameter == 'beta')
beta_parameters$Covariate <- factor(beta_parameters$Covariate)
levels(beta_parameters$Covariate) <- c("Bloco_code_2", "Bloco_code_3",
                                       "Femea", "Intercept")
beta_parameters$Factors <- beta_parameters$Covariate
levels(beta_parameters$Factors) <-  c("Bloco", "Bloco",         
                                      "Sexo", "Intercept")
beta_parameters$Category <- factor(beta_parameters$Category)

sex_data <- beta_parameters %>%
  mutate(Difference= ifelse(lower > 0, 'Yes', ifelse(higher < 0, 'Yes', 'No')))


sex_data$Time <- factor(sex_data$Time, levels = unique(sex_data$Time))

g2 <- sex_data %>%
  filter(Factors == 'Sexo') %>%
  ggplot(mapping = aes(x = average, y = Category,
                       colour = Difference)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, 
                    xmax = higher), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~factor(Time)) +
  theme_new() +
  theme(text = element_text(size = 16), # Definindo o tamanho da fonte global
        axis.text = element_text(size = 12), # Tamanho da fonte nos textos dos eixos
        axis.title = element_text(size = 16) # Tamanho da fonte nos títulos dos eixos
  ) +
  labs(y = "Type of behaviour", 
       x = expression(beta~posterior~distribution))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#232323","#C95910"))

ggsave("g2_femea.jpeg", plot = g2,
       width = 12, height = 8, dpi = 300)

#################################################################################################### 
##################### Comparing sex for Enriquecimento_code_3 ######################################
####################################################################################################
filtered_data <- processed_data %>%
  filter(Enriquecimento_code_3 == 1)

model_data <- construct_dataset(covariates = filtered_data[,c(
  "Bloco_code_2", "Bloco_code_3", "Sexo_code_2", "Intercept")],
  response = filtered_data[, 11:17], 
  processed_data = filtered_data, 
  N = 72, K = 12, C = 4, J = 7)

bayesian_model <- fit_bayesian_multinomial_longitudinal_model(N = 72, 
                                                              Y = model_data$Y,
                                                              X = model_data$X,
                                                              S = model_data$S, 
                                                              K = 12, 
                                                              J = model_data$J,
                                                              C = model_data$C)
# Checking for differences on sex
bayesian_model_parameters <- bayesian_model$model_summary
bayesian_model_parameters$Parameter <- factor(bayesian_model_parameters$Parameter)
beta_parameters <- bayesian_model_parameters %>%
  filter(Parameter == 'beta')
beta_parameters$Covariate <- factor(beta_parameters$Covariate)
levels(beta_parameters$Covariate) <- c("Bloco_code_2", "Bloco_code_3",
                                       "Femea", "Intercept")
beta_parameters$Factors <- beta_parameters$Covariate
levels(beta_parameters$Factors) <-  c("Bloco", "Bloco",         
                                      "Sexo", "Intercept")
beta_parameters$Category <- factor(beta_parameters$Category)

sex_data <- beta_parameters %>%
  mutate(Difference = ifelse(lower > 0, 'Yes', ifelse(higher < 0, 'Yes', 'No')))

sex_data$Time <- factor(sex_data$Time, levels = unique(sex_data$Time))

g3 <- sex_data %>%
  filter(Factors == 'Sexo') %>%
  ggplot(mapping = aes(x = average, y = Category,
                       colour = Difference)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, 
                    xmax = higher), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~factor(Time)) +
  theme_new() +
  theme(text = element_text(size = 16), # Definindo o tamanho da fonte global
        axis.text = element_text(size = 10), # Tamanho da fonte nos textos dos eixos
        axis.title = element_text(size = 16) # Tamanho da fonte nos títulos dos eixos
  ) +
  labs(y = "Type of behaviour", 
       x = expression(beta~posterior~distribution))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#232323","#C95910"))

ggsave("g3_femea.jpeg", plot = g3,
       width = 12, height = 8, dpi = 300)

########################MACHO################################
#################################################################################################### 
##################### Comparing sex for Enriquecimento_code_1 ######################################
####################################################################################################
filtered_data <- processed_data %>%
  filter(Enriquecimento_code_1 == 1)

model_data <- construct_dataset(covariates = filtered_data[,c(
  "Bloco_code_2", "Bloco_code_3", "Sexo_code_1", "Intercept")],
  response = filtered_data[, 11:17], 
  processed_data = filtered_data, 
  N = 72, K = 12, C = 4, J = 7)

bayesian_model <- fit_bayesian_multinomial_longitudinal_model(N = 72, 
                                                              Y = model_data$Y,
                                                              X = model_data$X,
                                                              S = model_data$S, 
                                                              K = 12, 
                                                              J = model_data$J,
                                                              C = model_data$C)


# Checking for differences on sex
bayesian_model_parameters <- bayesian_model$model_summary
bayesian_model_parameters$Parameter <- factor(bayesian_model_parameters$Parameter)
beta_parameters <- bayesian_model_parameters %>%
  filter(Parameter == 'beta')
beta_parameters$Covariate <- factor(beta_parameters$Covariate)
levels(beta_parameters$Covariate) <- c("Bloco_code_2", "Bloco_code_3",
                                       "Macho", "Intercept")
beta_parameters$Factors <- beta_parameters$Covariate
levels(beta_parameters$Factors) <-  c("Bloco", "Bloco",         
                                      "Sexo", "Intercept")
beta_parameters$Category <- factor(beta_parameters$Category)

sex_data <- beta_parameters %>%
  filter(Factors == 'Sexo')
sex_data <- sex_data %>%
  mutate(Difference = ifelse(lower > 0, 'Yes', ifelse(higher < 0, 'Yes', 'No')))


sex_data$Time <- factor(sex_data$Time, levels = unique(sex_data$Time))

g4 <- sex_data %>%
  filter(Factors == 'Sexo') %>%
  ggplot(mapping = aes(x = average, y = Category,
                       colour = Difference)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, 
                    xmax = higher), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~factor(Time)) +
  theme_new() +
  theme(text = element_text(size = 16), # Definindo o tamanho da fonte global
        axis.text = element_text(size = 16), # Tamanho da fonte nos textos dos eixos
        axis.title = element_text(size = 16) # Tamanho da fonte nos títulos dos eixos
  ) +
  labs(y = "Type of behaviour", 
       x = expression(beta~posterior~distribution))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#232323","#C95910"))

ggsave("g1_macho.jpeg", plot = g4,
       width = 12, height = 8, dpi = 300)

#################################################################################################### 
##################### Comparing sex for Enriquecimento_code_2 ######################################
####################################################################################################
filtered_data <- processed_data %>%
  filter(Enriquecimento_code_2 == 1)

model_data <- construct_dataset(covariates = filtered_data[,c(
  "Bloco_code_2", "Bloco_code_3", "Sexo_code_1", "Intercept")],
  response = filtered_data[, 11:17], 
  processed_data = filtered_data, 
  N = 72, K = 12, C = 4, J = 7)

bayesian_model <- fit_bayesian_multinomial_longitudinal_model(N = 72, 
                                                              Y = model_data$Y,
                                                              X = model_data$X,
                                                              S = model_data$S, 
                                                              K = 12, 
                                                              J = model_data$J,
                                                              C = model_data$C)
# Checking for differences on sex
bayesian_model_parameters <- bayesian_model$model_summary
bayesian_model_parameters$Parameter <- factor(bayesian_model_parameters$Parameter)
beta_parameters <- bayesian_model_parameters %>%
  filter(Parameter == 'beta')
beta_parameters$Covariate <- factor(beta_parameters$Covariate)
levels(beta_parameters$Covariate) <- c("Bloco_code_2", "Bloco_code_3",
                                       "Macho", "Intercept")
beta_parameters$Factors <- beta_parameters$Covariate
levels(beta_parameters$Factors) <-  c("Bloco", "Bloco",         
                                      "Sexo", "Intercept")
beta_parameters$Category <- factor(beta_parameters$Category)

sex_data <- beta_parameters %>%
  mutate(Difference = ifelse(lower > 0, 'Yes', ifelse(higher < 0, 'Yes', 'No')))


sex_data$Time <- factor(sex_data$Time, levels = unique(sex_data$Time))

g5 <- sex_data %>%
  filter(Factors == 'Sexo') %>%
  ggplot(mapping = aes(x = average, y = Category,
                       colour = Difference)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, 
                    xmax = higher), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~factor(Time)) +
  theme_new() +
  theme(text = element_text(size = 16), # Definindo o tamanho da fonte global
        axis.text = element_text(size = 16), # Tamanho da fonte nos textos dos eixos
        axis.title = element_text(size = 16) # Tamanho da fonte nos títulos dos eixos
  ) +
  labs(y = "Type of behaviour", 
       x = expression(beta~posterior~distribution))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#232323","#C95910"))

ggsave("g2_macho.jpeg", plot = g5,
       width = 12, height = 8, dpi = 300)
#################################################################################################### 
##################### Comparing sex for Enriquecimento_code_3 ######################################
####################################################################################################
filtered_data <- processed_data %>%
  filter(Enriquecimento_code_3 == 1)

model_data <- construct_dataset(covariates = filtered_data[,c(
  "Bloco_code_2", "Bloco_code_3", "Sexo_code_1", "Intercept")],
  response = filtered_data[, 11:17], 
  processed_data = filtered_data, 
  N = 72, K = 12, C = 4, J = 7)

bayesian_model <- fit_bayesian_multinomial_longitudinal_model(N = 72, 
                                                              Y = model_data$Y,
                                                              X = model_data$X,
                                                              S = model_data$S, 
                                                              K = 12, 
                                                              J = model_data$J,
                                                              C = model_data$C)
# Checking for differences on sex
bayesian_model_parameters <- bayesian_model$model_summary
bayesian_model_parameters$Parameter <- factor(bayesian_model_parameters$Parameter)
beta_parameters <- bayesian_model_parameters %>%
  filter(Parameter == 'beta')
beta_parameters$Covariate <- factor(beta_parameters$Covariate)
levels(beta_parameters$Covariate) <- c("Bloco_code_2", "Bloco_code_3",
                                       "Macho", "Intercept")
beta_parameters$Factors <- beta_parameters$Covariate
levels(beta_parameters$Factors) <-  c("Bloco", "Bloco",         
                                      "Sexo", "Intercept")
beta_parameters$Category <- factor(beta_parameters$Category)

sex_data <- beta_parameters %>%
  mutate(Difference = ifelse(lower > 0, 'Yes', ifelse(higher < 0, 'Yes', 'No')))

sex_data$Time <- factor(sex_data$Time, levels = unique(sex_data$Time))

g6  <- sex_data %>%
  filter(Factors == 'Sexo') %>%
  ggplot(mapping = aes(x = average, y = Category,
                       colour = Difference))+
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, 
                    xmax = higher), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~factor(Time)) +
  theme_new() +
  theme(text = element_text(size = 14), # Definindo o tamanho da fonte global
        axis.text = element_text(size = 14), # Tamanho da fonte nos textos dos eixos
        axis.title = element_text(size = 14) # Tamanho da fonte nos títulos dos eixos
  ) +
  labs(y = "Type of behaviour", 
       x = expression(beta~posterior~distribution))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c( "#232323", "#C95910"))

ggsave("g3_macho.jpeg", plot = g6,
       width = 12, height = 8, dpi = 300)
