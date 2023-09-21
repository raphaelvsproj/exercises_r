######## TESTE-T PAREADO NO R ########
# Fazer o teste de Shapiro-Wilk;
# Fazer o teste-t para duas amostras dependentes: Uni e bicaudal;
# Fazer um boxplot para os dados;
# Pedir a Estatística descritiva dos dados.

if(!require(dplyr))install.packages("dplyr")
library(dplyr)
if(!require(psych))install.packages("psych")
library(psych)

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

dados <- read.csv2('Banco de Dados 4.csv') %>% 
  rename(Convulsoes_PT = Convulsões_PT, Convulsoes_S1 = Convulsões_S1,
         Convulsoes_S6 = Convulsões_S6, Genero = Gênero)
glimpse(dados)

# Em se tratando de Teste-T com dados pareados (dependentes), o pressuposto não é que
# as duas variáveis tenham distribuição normal, mas que a diferença entre elas é que
# tenha distribuição normal.

dados$DiferencaPTS1 <- dados$Convulsoes_PT - dados$Convulsoes_S1

# Shapiro-Wilk para verificação de normalidade da amostra.
# H0: Distribuição dos dados = normal → p > 0,05
# H1: Distribuição dos dados ≠ normal → p ≤ 0,05
shapiro.test(dados$DiferencaPTS1) # Verificou-se que a diferença não tem distribuição normal (Rejeita H0).

t.test(dados$Convulsoes_PT, dados$Convulsoes_S1, paired = TRUE) # Rejeita H0

# O teste-T tem como hipótese nula (argumento para rejeitar H0) o seguinte:
# H0: Média das Diferenças = 0 → p > 0,05
# H1: Média das Diferenças ≠ 0 → p ≤ 0,05

# Verificou-se então que existe diferença verificada no número de convulsões entre a primeira semana
# e a semana pré-tratamento. O teste-t pareado mostrou que a média de convulsões na primeira semana
# é diferente da média de convulsões pré-tratamento (t(274) = 3,68; p < 0,001). A média de convulsões
# na primeira semana foi inferior à média da semana pré-tratamento.

par(mfrow=c(1,2))
boxplot(dados$Convulsoes_PT, ylab = "Quantidade de Convulsões", xlab = "Semana Pré-Tratamento")
boxplot(dados$Convulsoes_S1, ylab = "Quantidade de Convulsões", xlab = "1ª Semana de Tratamento")
# Verificou-se muitos outliers em cada um dos gráficos, o que significa que o Teste-T pode não
# ser uma boa opção para este tipo de análise por se basear na média.

summary(dados$Convulsoes_PT)
summary(dados$Convulsoes_S1)
describe(dados$Convulsoes_PT)
describe(dados$Convulsoes_S1)

# Necessário utilizar um teste que se baseia na mediana.






