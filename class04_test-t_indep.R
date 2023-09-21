######## TESTE-T INDEPENDENTE ########
# Fazer o teste de Shapiro-Wilk, separando por grupos;
# Fazer o teste Levene - que verifica a homogeneidade das variâncias;
# Fazer o teste-t para duas amostras independentes: Uni e bicaudal;
# Fazer um boxplot para os dados, separados por grupo.

library(dplyr)
if(!require(RVAideMemoire))install.packages("RVAideMemoire") # pacote que permite
# que façamos o teste shapiro-wilk dividido por grupos.
library(RVAideMemoire)
if(!require(car))install.packages("car") # pacote que traz o teste de Levene,
# a fim de verificar a homogeneidade das variâncias.
library(car)

# Verificar se há efeito da posição que o aluno ocupa na sala (onde ele senta,
# se na "Frente" ou no "Fundo") sobre as notas de Biologia, Física e História.
# Descrever os resultados de forma apropriada.

dados <- read.csv2('Banco de Dados 3.csv')
glimpse(dados)


#### Teste Shapiro-Wilk por grupos pelo pacote "RVAideMoire":
byf.shapiro(Nota_Biol ~ Posicao_Sala, dados) # H0 aceito
byf.shapiro(Nota_Fis ~ Posicao_Sala, dados) # H0 aceito
byf.shapiro(Nota_Hist ~ Posicao_Sala, dados) # H0 aceito

# O teste revelou que para ambas as hipóteses apresentam distribuição normal.
# H0: Distribuição dos dados = normal → p > 0,05
# H1: Distribuição dos dados ≠ normal → p ≤ 0,05


#### Teste Levene pelo pacote "car", teste baseado na média, o default realiza teste baseado na mediana:
leveneTest(Nota_Biol ~ Posicao_Sala, dados, center=mean) # H0 aceito
leveneTest(Nota_Fis ~ Posicao_Sala, dados, center=mean) # H0 rejeitado
leveneTest(Nota_Hist ~ Posicao_Sala, dados, center=mean) # H0 rejeitado

# O teste de levene tem pressuposto de que as variâncias dos grupos são ou não homogêneas
# H0: as variâncias dos grupos são homogêneas → p > 0,05
# H1: as variâncias dos grupos não são homogêneas → p ≤ 0,05

# Para as notas de Física e História, as variâncias dos grupos não são homogêneas.


#### Teste-T para amostras independentes:
t.test(Nota_Biol ~ Posicao_Sala, dados, var.equal=TRUE) # Rejeita H0
t.test(Nota_Fis ~ Posicao_Sala, dados, var.equal=FALSE) # Rejeita H0
t.test(Nota_Hist ~ Posicao_Sala, dados, var.equal=FALSE) # Aceita H0

# O teste-T tem como hipótese nula (argumento para rejeitar H0) o seguinte:
# H0: Média do grupo A = Média do Grupo B → p > 0,05
# H1: Média do grupo A ≠ Média do Grupo B → p ≤ 0,05

# OBSERVAÇÃO:
# o teste-t tem como default o bicaudal, e caso desejamos aplicar análise unicaudal,
# é necessário incluir:
# alternative = "greater" ou alternative = "less"
t.test(Nota_Biol ~ Posicao_Sala, dados, var.equal=TRUE, alternative = "greater") # Rejeita H0
t.test(Nota_Fis ~ Posicao_Sala, dados, var.equal=FALSE, alternative = "greater") # Rejeita H0
t.test(Nota_Hist ~ Posicao_Sala, dados, var.equal=FALSE, alternative = "greater") # Aceita H0


# Verificou-se assim que as notas de Biologia e Fìsica sofrem alteração com a posição em que
# o aluno senta na sala de aula, mas para com a matéria de História não ocorre o mesmo.

par(mfrow=c(1,3)) # Estabeleci que quero que os gráficos saiam na mesma linha
boxplot(Nota_Biol ~ Posicao_Sala, data = dados, ylab="Notas de Biologia", xlab="Posição na Sala")
boxplot(Nota_Fis ~ Posicao_Sala, data = dados, ylab="Notas de Física", xlab="Posição na Sala")
boxplot(Nota_Hist ~ Posicao_Sala, data = dados, ylab="Notas de História", xlab="Posição na Sala")

# O teste-T para duas amostras independentes mostrou que há efeito da posição na sala sobre a nota
# de física (t(17,68) = 4,44; p < 0,001). O grupo que senta na frente apresentou, em média, notas
# de física superiores às do grupo que senta nos fundos da sala.







