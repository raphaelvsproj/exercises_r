########## ESTATÍSTICAS DESCRITIVAS ##########
# Tentando criar tabelas de frequências (absolutas e relativas) para variáveis
# categóricas e numéricas
# E obtendo medidas de tendência central e medidas de dispersão para variáveis
# numéricas.

library(dplyr)
if(!require(psych))
  install.packages("psych")
library(psych)

dados <- read.csv2('Banco de Dados 2.csv')
dados[dados==-999] <- NA
glimpse(dados)

#### Tabela de frequências de variáveis categóricas ####

# Frequências Absolutas
table(dados$Genero)
table(dados$Grau_de_Instruçao)

# Tabela cruzada com frequências absolutas
table(dados$Genero, dados$Grau_de_Instruçao)

# Frequências Relativas (em percentual)
prop.table(table(dados$Genero))
prop.table(table(dados$Grau_de_Instruçao))

# Tabela cruzada com frequências relativas
prop.table(table(dados$Genero, dados$Grau_de_Instruçao))


#### Tabela de frequências de variáveis Quantitativas ####

### Tabela de Frequências ###

# Variáveis Discretas:
table(dados$N_Filhos)
prop.table(table(dados$N_Filhos))

# Variáveis Contínuas ------ Necessário criar categorias que correspondam a faixa de valores:

# PASSO 1: Analisando Amplitude:
range(dados$Salario) # OBS: range = amplitude

# PASSO 2 (opcional): avaliar a quantidade de categorias adequada (Método Sturges):
nclass.Sturges(dados$Salario)

# PASSO 3: Criação da tabela com faixas:
table(cut(dados$Salario, seq(0,6, l = 7)))

## Função Summary - fornece média, mediana, quartis e valores mín e max.
summary(dados$Salario)
summary(dados$N_Filhos)

## Funções describe e describe.by (pacote 'psych') - média, desvio, erro, mediana
describe(dados$Salario)
describeBy(dados$Salario, group = dados$Genero)
describeBy(dados$Salario, group = dados$Grau_de_Instruçao)
describeBy(dados$Salario, group = dados$Genero:dados$Grau_de_Instruçao)

## Usando o group_by do pacote dplyr:
tabela <- dados %>% group_by(Genero, Grau_de_Instruçao) %>% 
  summarise(
    média = mean(Salario),
    DP = sd(Salario),
    mediana = median(Salario)
  )
tabela


