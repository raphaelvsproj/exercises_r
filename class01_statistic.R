if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
require(dplyr)
library(car)

######## Carregando o banco de dados ########
# Session > Set Working Directory > Choose Directory
setwd("C:\Users\raphael.vieira\OneDrive - Valec\Área de Trabalho\UNB\00 - CURSOS LIVRES\12 - RStudio\02 - R para Estatística\Statistic_applying_to_R")


## Codificando variáveis no R
dados_c <- read.csv('Banco de Dados 2 Codificado.csv', sep = ';', dec = ',', stringsAsFactors = T)

## Ajustando as variáveis

dados_c$Genero <- factor(dados_c$Genero, label = c("M","F"), levels = c(0,1))
dados_c$Grau_de_Instruçao <- factor(dados_c$Grau_de_Instruçao,
                                    label = c("Fundamental", "Medio", "Superior"),
                                    levels = 0:2,
                                    order = T)

# Codificando valores ausentes (missing values):

dados_c[dados_c==-999] <- NA
View(dados_c)
glimpse(dados_c)