######## TESTE DE MANN-WHITNEY NO R ########
# Fazer o teste não paramétrico de Mann-Whitney para duas amostras independentes: uni e bicaudal.
# Pedir a estatística descritiva dos dados

# Objetivo: comparar dois grupos independentes, utilizado quando a amostra não é normal.
# em amostras "não normais", a média não é uma boa representação do conjunto de dados,
# portanto não faz sentido rodar o teste mann-whitney e reportar a média dos grupos,
# faz muito mais sentido reportar a mediana e o intervalo, amplitude interquartil.

# também chamado de: Wilcoxon rank-sum test (teste para soma dos postos de Wilcoxon)

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(rstatix)) install.packages("rstatix")
library(rstatix)

###### PRESSUPOSTOS ######
## Variável dependente numérica ou categórica ordinal.
## Variável independente composta por dois grupos independentes.

###### EXERCÍCIO ######
# O banco de dados contém informações de 32 alunos de uma rede pública e privada.
# Verificar se há efeito da posição que o aluno ocupa na sala (se "Frente" ou "Fundo")
# sobre as suas notas de Biologia, Física e História.
# Descrever os resultados de forma apropriada.

dados <- read.csv2('Banco de Dados 3.csv')
View(dados)
glimpse(dados)

# Realização do teste de Mann-Whitney
wilcox.test(Nota_Biol ~ Posicao_Sala, data = dados) # rejeita H0
wilcox.test(Nota_Fis ~ Posicao_Sala, data = dados) # rejeita H0 
wilcox.test(Nota_Hist ~ Posicao_Sala, data = dados) # aceita H0

# As hipóteses para o teste de Mann-Whitney são:
# H0: mediana do grupo A = mediana do grupo B → p > 0,05
# H1: mediana do grupo A ≠ mediana do grupo B → p ≤ 0,05

# OBS: o teste bicaudal é default, caso deseje unicaudal, necessário incluir:
  # alternative = "greater" ou alternative = "less"
wilcox.test(Nota_Biol ~ Posicao_Sala, data = dados, alternative = "greater") # rejeita H0
wilcox.test(Nota_Fis ~ Posicao_Sala, data = dados, alternative = "greater") # rejeita H0 
wilcox.test(Nota_Hist ~ Posicao_Sala, data = dados, alternative = "greater") # aceita H0
# Nesse caso, o teste verificará se é a mediana do primeiro grupo é maior que a mediana do segundo
# O R está considerando "Frente" como primeiro grupo


# Análise descritiva dos dados
dados %>% group_by(Posicao_Sala) %>% 
  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "median_iqr") # mediana ou amplitude interquartil

# Utilizando estatística não paramétrica
dados %>% group_by(Posicao_Sala) %>% 
  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "mean_sd") # média
?get_summary_stats

# Visualização da distribuição
par(mfrow = c(1,2))
hist(dados$Nota_Biol[dados$Posicao_Sala == "Frente"],
     ylab="Frequência", xlab = "Notas de Biologia", main = "Grupo Frente")
hist(dados$Nota_Biol[dados$Posicao_Sala == "Fundos"],
     ylab="Frequência", xlab = "Notas de Biologia", main = "Grupo Fundos")

hist(dados$Nota_Fis[dados$Posicao_Sala == "Frente"],
     ylab="Frequência", xlab = "Notas de Física", main = "Grupo Frente")
hist(dados$Nota_Fis[dados$Posicao_Sala == "Fundos"],
     ylab="Frequência", xlab = "Notas de Física", main = "Grupo Fundos")

hist(dados$Nota_Hist[dados$Posicao_Sala == "Frente"],
     ylab="Frequência", xlab = "Notas de História", main = "Grupo Frente")
hist(dados$Nota_Hist[dados$Posicao_Sala == "Fundos"],
     ylab="Frequência", xlab = "Notas de História", main = "Grupo Fundos")


## RESULTADO
# O teste de Mann-Whitney mostrou que a mediana das notas de biologia do grupo "Frente" é
# diferente da mediana das notas do grupo "Fundos" (W = 227; p < 0,05). A mediana do grupo
# "Frente" (6,4 e 2,25, mediana e IQR) foi superior à do grupo "Fundos" (3,8 e 1,5).

# Para a disciplina de Física, a mediana das notas do grupo "Frente" também é diferente da
# mediana das notas do grupo "Fundos" (W = 219; p < 0,05). A mediana do grupo "Frente"
# (6,6 e 3,65, mediana e IQR) foi superior à do grupo "Fundos" (3,8 e 1).

# E para a disciplina de História, a mediana das notas do grupo "Frente" também é diferente da
# mediana das notas do grupo "Fundos" (W = 154; p > 0,05). A mediana do grupo "Frente"
# (5,4 e 5, mediana e IQR) foi superior à do grupo "Fundos" (4,9 e 2,2), mesmo o teste
# tendo aceitado H0.