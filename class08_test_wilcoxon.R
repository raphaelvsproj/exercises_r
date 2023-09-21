######## TESTE NÃO-PARAMÉTRICO DE WILCOXON NO R ########
# Fazer o teste de Wilcoxon para duas amostras pareadas: uni e bicaudal.
# Pedir a estatística descritiva dos dados

# Objetivo: comparar dois grupos independentes, utilizado quando a amostra não é normal.
# Geralmente, esse tipo de dado é bem representado pela "média", no entanto a mediana é uma medida
# de tendência central que representa muito melhor, consequentemente o desvio padrão não tende
# a representar de forma adequada esses dados, dando assim preferência à amplitude interquartil.

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(rstatix)) install.packages("rstatix")
library(rstatix)

# O teste de Wilcoxon também é conhecido como: Wilcoxon signed-rank test
# (teste dos pontos sinalizados de Wilcoxon)

# Pressupostos:
## Variável dependente numérica ou categórica ordinal.
## Variável independente composta por dois grupos dependentes (pareados).

dados <- read.csv2('Banco de Dados 4.csv') %>% 
  rename(Convulsoes_PT = Convulsões_PT, Convulsoes_S1 = Convulsões_S1,
         Convulsoes_S6 = Convulsões_S6, Genero = Gênero)
glimpse(dados)

### EXERCÍCIO:
# O banco de dados contém informações de 275 indivíduos tratados com anticonvulsivante.
# A variável "Convulsões" indica o número de convulsões por dia de cada paciente em três
# condições:
# Convulcoes_PT - antes do início do tratamento;
# Convulcoes_S1 - na primeira semana de tratamento;
# Convulcoes_S6 - na sexta semana de tratamento;

# Verificar se há diferença no número de convulsões por dia de cada indivíduo antes do
# tratamento e na primeira semana do tratamento.
# Descreva os resultados de forma apropriada.

wilcox.test(dados$Convulsoes_PT, dados$Convulsoes_S1, paired = T) # teste bicaudal e Rejeita H0
wilcox.test(dados$Convulsoes_PT, dados$Convulsoes_S1, paired = T, alternative = "greater") # teste unicaudal e Rejeita H0

# O teste wilcoxon tem como hipótese nula (h0) o seguinte:.
# H0: Mediana das diferenças = 0 → p > 0,05
# H1: Mediana das diferenças ≠ 0 → p ≤ 0,05

dados$DiferencaPTS1 <- dados$Convulsoes_PT - dados$Convulsoes_S1
View(dados)

dados %>% get_summary_stats(Convulsoes_PT, Convulsoes_S1, DiferencaPTS1, type = "median_iqr")

## RESULTADO
# A quantidade de convulsões na primeira semana foi inferior à quantidade de convulsões pré-tratamento.
# O teste de sinais de Wilcoxon mostrou que essa diferença é estatisticamente significativa (V = 14626, p < 0,05).

