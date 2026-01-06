####################################################################################################
###
### File:    03_data_visualisation.r
### Purpose: Analyse of the dataset 
### Authors: Gabriel Rodrigues Palma
### Date:    01/03/24
###
####################################################################################################
# # Load packages -----
source('00_source.r')

# Pre processed the data -----
bayesian_model_parameters <- read.csv('output_data/case_study_bayesian_multinomial_longitudinal_regression_parameter.csv')[,-1]
bayesian_model_parameters$Parameter <- factor(bayesian_model_parameters$Parameter)
levels(bayesian_model_parameters$Parameter)

beta_parameters <- bayesian_model_parameters %>%
            filter(Parameter == 'beta')
beta_parameters$Covariate <- factor(beta_parameters$Covariate)
levels(beta_parameters$Covariate) <-  c("Bloco_code_2", "Bloco_code_3",         
                                        "Corrente",
                                        "Nenhum", "Femea", 'Intercept', 
                                        'Interaction_sex2_enriquecimento2', 
                                        'Interaction_sexo2_enriquecimento3')
beta_parameters$Factors <- beta_parameters$Covariate
levels(beta_parameters$Factors) <-  c("Bloco", "Bloco",         
                                      "Enriquecimento",
                                      "Enriquecimento", "Sexo", 
                                      "Intercept", "Interaction", 
                                      "Interaction")

beta_parameters$Category <- factor(beta_parameters$Category)
#levels(beta_parameters$Category) <- c("Agressivo" , "Alimentando", "Calmo",
#                                      "Inter_animal", "Inter_meio_amb",
#                                      "Inter_object",   "Locomovendo")
# Creating visualisations for beta -----
# Checking for interaction
interaction_data <- beta_parameters %>%
  filter(Factors == 'Interaction')
interaction_data <- interaction_data %>%
  mutate(Difference = ifelse(lower > 0, 'Yes', ifelse(higher < 0, 'Yes', 'No')))


interaction_data %>%
  ggplot(mapping = aes(x = average, y = Category, 
                       group = Covariate)) +
  geom_point(position = position_dodge(width = 0.5), aes(colour = Difference, shape = Covariate)) +
  geom_errorbar(aes(xmin = lower, 
                    xmax = higher, 
                    colour = Difference), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~factor(Time)) +
  theme_new() +
  theme(text = element_text(size = 12), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  labs(y = "Types of behaviours", x = expression(beta~distribuição~posteriori)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c( "#232323","#C95910"))+
  scale_shape_manual(values = c(1, 2), labels = c("Enrichment chain for female pigs ", "Environment without enrichment for female pigs"))

interaction_data %>%
  ggplot(mapping = aes(x = average, y = Category, 
                       group = Covariate)) +
  geom_point(position = position_dodge(width = 0.5), aes(colour = Difference, shape = Covariate)) +
  geom_errorbar(aes(xmin = lower, 
                    xmax = higher, 
                    colour = Difference), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~factor(Time)) +
  theme_new() +
  theme(text = element_text(size = 12), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  labs(y = "Tipos de comportamentos", x = expression(beta~distribuição~posteriori),
       colour = "Diferença", shape = "Covariável") +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#232323","#C95910")) +
  scale_shape_manual(values = c(1, 2), labels = c("Corrente para suínos fêmeas", "Ambiente SE para suínos fêmeas"))


# Checking for differences on sex
sex_data <- beta_parameters %>%
  filter(Factors == 'Sexo')
sex_data <- sex_data %>%
  mutate(Diferença = ifelse(lower > 0, 'Sim', ifelse(higher < 0, 'Sim', 'Não')))

sex_data %>%
  filter(Factors == 'Sexo') %>%
  ggplot(mapping = aes(x = average, y = Category,
                       colour = Diferença)) +
      geom_point(position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(xmin = lower, 
                        xmax = higher), 
                    width = 0.2, position = position_dodge(width = 0.5)) +
      facet_wrap(~factor(Time)) +
      theme_new() +
  theme(text = element_text(size = 12), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
      labs(y = "Tipos de Comportamentos", x = expression(beta~distribuição~posteriori)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#232323","#C95910"))

## Checking for differences on enrichment 
Enriquecimento_data <- beta_parameters %>%
                       filter(Factors == 'Enriquecimento') %>%
  mutate(Diferença = ifelse(lower > 0, 'Sim', ifelse(higher < 0, 'Sim', 'Não')))

Enriquecimento_data %>%
  ggplot(mapping = aes(x = average, y = Category, 
                       group = Covariate)) +
  geom_point(position = position_dodge(width = 0.5), aes(colour = Diferença, shape = Covariate)) +
  geom_errorbar(aes(xmin = lower, 
                    xmax = higher, 
                    colour = Diferença), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~factor(Time)) +
  theme_new() +
  theme(text = element_text(size = 12), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))+
  labs(y = "Tipos de Comportamentos", 
       x = expression(beta~distribuição~posteriori),
       group = "Covariável")  +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c( "#232323","#C95910"))

Enriquecimento_data %>%
  ggplot(mapping = aes(x = average, y = Category, 
                       group = Covariate)) +
  geom_point(position = position_dodge(width = 0.5), aes(colour = Diferença, shape = Covariate)) +
  geom_errorbar(aes(xmin = lower, 
                    xmax = higher, 
                    colour = Diferença), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~factor(Time)) +
  theme_new() +
  theme(text = element_text(size = 12), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  labs(y = "Tipos de Comportamentos", x = expression(beta~distribuição~posteriori),
       colour = "Diferença", shape = "Covariável") +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c( "#232323","#C95910"))    
