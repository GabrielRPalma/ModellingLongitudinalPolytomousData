####################################################################################################
###
### File:    01_data_preprocessing.r
### Purpose: Prepare the dataset for being processed using Rjags functions
### Authors: Gabriel Rodrigues Palma
### Date:    12/01/24
###
####################################################################################################
# Load functions -----
source('00_source.r')

# Load dataset -----
dataset <- read.csv('input_data/Dados_Etograma.csv', sep =';')
dataset <- dataset %>%
  filter(Etograma != "")
dataset$Sexo_code <- as.integer(dataset$Sexo == "Femea")

dataset$Horario_code <- factor(dataset$Horario)
levels(dataset$Horario_code) <- 0:7
dataset$Horario_code <- as.integer(dataset$Horario_code)

dataset$Enriquecimento_code <- factor(dataset$Enriquecimento)
levels(dataset$Enriquecimento_code) <- 0:2
dataset$Enriquecimento_code <- as.integer(dataset$Enriquecimento_code)

dataset$Bloco_code <- factor(dataset$Bloco)
levels(dataset$Bloco_code) <- 0:2
dataset$Bloco_code <- as.integer(dataset$Bloco_code)


# Data preprocessing -----
processed_data <- dataset %>%
                  group_by(Data, Animal, Bloco_code, 
                           Enriquecimento_code, Sexo_code) %>%
                  summarise(Agressivo = ifelse(is.na(table(Etograma)[1]), 0, table(Etograma)[1]), 
                            Alimentando = ifelse(is.na(table(Etograma)[2]), 0, table(Etograma)[2]), 
                            Calmo = ifelse(is.na(table(Etograma)[3]), 0, table(Etograma)[3]), 
                            Inter_animal = ifelse(is.na(table(Etograma)[4]), 0, table(Etograma)[4]), 
                            Inter_meio_amb = ifelse(is.na(table(Etograma)[5]), 0, table(Etograma)[5]), 
                            Inter_object = ifelse(is.na(table(Etograma)[6]), 0, table(Etograma)[6]), 
                            Locomovendo = ifelse(is.na(table(Etograma)[7]), 0, table(Etograma)[7]))

processed_data$Sexo_code_1 <- as.integer(processed_data$Sexo_code == "Femea")
processed_data$Sexo_code_2 <- as.integer(processed_data$Sexo_code != "Femea")


processed_data$Enriquecimento_code_1 <- as.integer(processed_data$Enriquecimento_code == 1)
processed_data$Enriquecimento_code_2 <- as.integer(processed_data$Enriquecimento_code == 2)
processed_data$Enriquecimento_code_3 <- as.integer(processed_data$Enriquecimento_code == 3)


processed_data$Bloco_code_1 <- as.integer(processed_data$Bloco_code == 1)
processed_data$Bloco_code_2 <- as.integer(processed_data$Bloco_code == 2)
processed_data$Bloco_code_3 <- as.integer(processed_data$Bloco_code == 3)

processed_data <- processed_data %>%
          dplyr::select(c(Data, Animal, Bloco_code_1, Bloco_code_2, Bloco_code_3, 
                          Enriquecimento_code_1, Enriquecimento_code_2, Enriquecimento_code_3, 
                          Sexo_code_1, Sexo_code_2, Agressivo, Alimentando, Calmo, Inter_animal, 
                          Inter_meio_amb, Inter_object, Locomovendo))
processed_data <- processed_data[, -c(1, 2)]
