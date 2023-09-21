######## Teste-T para uma amostra #########

#### Verificar a altura dos empregados comparando com a média nacional: 1,67m.

library(dplyr)
dados <- read.csv('Banco de Dados 2 - Teste T.csv', sep = ';', dec = ',')
View(dados)
glimpse(dados)

# Verificação da normalidade (Shapiro-Wilk Test)
shapiro.test(dados$Altura)

# Uma vez a amostra tendo distribuição normal, realizamos o teste-t.
t.test(dados$Altura, mu = 167)

# o teste-t para uma amostra revelou que a média de altura verificada (168,43 cm)
# não é diferente da média de altura nacional (167 cm)
# (t(29) = 0,702; p = 0,488).
# H0: Distribuição dos dados = normal → p > 0,05
# H1: Distribuição dos dados ≠ normal → p ≤ 0,05

# OBS:
# O teste bicaudal é o default: caso deseje unicaudal, necessário incluir:
# alternative = "greater" ou alternative = "less".
t.test(dados$Altura, mu = 167, alternative = "greater")
# Nesse caso, o teste verificará se a média amostral é maior que a média testada.

boxplot(dados$Altura, ylab = "Altura(cm)")
