######## TESTE ANOVA DE UMA VIA ########
# Verificar a normalidade por grupo (teste de Sapiro-Wilk)
# Verificar a homogeneidade das variâncias (teste de Levene)
# Verificar a presença de outliers (boxplot e função)
# Fazer a ANOVA de uma via
# Fazer diferentes tipos de post-hoc (Duncan, Bonferroni, Tukey HSD)
# Pedir a estatística descritiva dos dados

## DEFINIÇÃO:
# A Análise de Variância (ANOVA) é uma fórmula estatística usada para comparar 
# as variâncias entre as medianas (ou médias) de grupos diferentes. 
# Diversos cenários usam ANOVA para determinar se há alguma diferença entre as medianas 
# dos diferentes grupos.

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(RVAideMemoire)) install.packages("RVAideMemoire")
library(RVAideMemoire)
if(!require(car)) install.packages("car")
library(car)
if(!require(psych)) install.packages("psych")
library(psych)
if(!require(rstatix)) install.packages("rstatix")
library(rstatix)
if(!require(DescTools)) install.packages("DescTools")
library(DescTools)


###### EXERCÍCIO ######
# O banco de dados contém informações de 31 indivíduos tratados com placebo
# um anti-hipertensivo já em uso no mercado ou um anti-hipertensivo novo.
# Verificar se há efeito do tratamento sobre a pressão sistólica e os batimentos
# cardíacos desses indivíduos. Descrever os resultados de forma apropriada.

dados <- read.csv2('Banco de Dados 5.csv', stringsAsFactors = T)
View(dados)
glimpse(dados)

# Analisar a normalidade dos dados
## Shapiro por grupo (pacote RVAideMemoire) - devemos analisar a normalidade da amostra como um todo, por grupo, ou a normalidade dos resíduos?!
# Nesse caso, optamos por analisar em grupo (Shapiro-Wilk test)

# Shapiro-Wilk para verificação de normalidade da amostra.
# H0: Distribuição dos dados = normal → p > 0,05
# H1: Distribuição dos dados ≠ normal → p ≤ 0,05

byf.shapiro(BC ~ Grupo, dados)
# A distribuição "Placebo" não apresentou distribuição normal, logo H0 rejeitado para "Placebo".
byf.shapiro(Pressao ~Grupo, dados)
# Todas as distribuições apresentaram padrão normal, aceitando H0.


# Verificação de homogeneidade das variâncias (homocedasticidade)
## Teste Levene (pacote car)

# O teste de levene tem pressuposto de que as variâncias dos grupos são ou não homogêneas
# H0: as variâncias dos grupos são homogêneas → p > 0,05
# H1: as variâncias dos grupos não são homogêneas → p ≤ 0,05

leveneTest(BC ~ Grupo, dados, center=mean)
# a homogeneidade das variâncias apresentou valor de p(28)=0.09856 > 0.05, logo H0 aceita.
leveneTest(Pressao ~ Grupo, dados, center=mean)
# a homogeneidade das variâncias apresentou valor de p(28)=0.6323 > 0.05, logo H0 aceita.

# Por default, o teste realizado pelo pacote car tem como base a mediana (median)
# O teste baseado na mediana é mais robusto, mas nesse caso mudamos o default para
# que o teste seja baseado na média (comparável ao SPSS)

# Verificar se existem outliers por grupo - pacote dplyr e rstatix
dados %>% 
  group_by(Grupo) %>% 
  identify_outliers(BC) # Para batimentos cardíacos
boxplot(BC ~ Grupo, data = dados, ylab="Frequência Cardíaca (bps)", xlab="Tratamento")

# A função de identificar os outliers considera 1,50 vezes a amplitude interquartil,
# a função do boxplot considera 1,58 vezes a amplitude interquartil

dados %>% 
  group_by(Grupo) %>% 
  identify_outliers(Pressao) # Para pressão arterial
boxplot(Pressao ~ Grupo, data = dados, ylab="Pressão Arterial (mmHg)", xlab="Tratamento")

# Até o momento temos homogeneidade de variância tanto pra BC quanto pra Pressão Arterial,
# Temos normalidade em todos os grupos para pressão, mas não temos em todos os grupos para BC,
# E verificou-se outliers tanto para BC quanto para pressão.

# Logo, não temos uma condição ideal para realizar um teste ANOVA de uma via.

anova_BC <- aov(BC ~ Grupo, dados)
anova_Pressao <- aov(Pressao ~ Grupo, dados)
# sabemos que existe diferença em ambos os casos, mas não 
# sabemos quais são essas diferenças e em quais grupos.
summary(anova_BC)
summary(anova_Pressao)

# O teste ANOVA considera:
# H0: média do grupo A = média do grupo B → p > 0,05
# H1: há pelo menos uma diferença entre as médias dos grupos → p ≤ 0,05

# ANÁLISE DE POST-HOC
# Post-hocs permitidos: "hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan".

# Uso do Duncan - mais "moderado"
PostHocTest(anova_BC, method = "duncan", conf.level = 0.95)
PostHocTest(anova_Pressao, method = "duncan", conf.level = 0.95)

# Uso do TukeyHSD - "moderado"
PostHocTest(anova_BC, method = "hsd", conf.level = 0.95)
PostHocTest(anova_Pressao, method = "hsd", conf.level = 0.95)

# Uso do Bonferroni - teste mais "conservador" de todos
PostHocTest(anova_BC, method = "bonf", conf.level = 0.95)
PostHocTest(anova_Pressao, method = "bonf", conf.level = 0.95)

# Exemplo de como resumir em uma tabela mais de um post-hoc
round(
  cbind(duncan = PostHocTest(anova_BC, method = "duncan")$Grupo[,"pval"],
        bonf = PostHocTest(anova_BC, method = "bonf")$Grupo[,"pval"],
        hsd = PostHocTest(anova_BC, method = "hsd")$Grupo[,"pval"]),
6)

round(
  cbind(duncan = PostHocTest(anova_Pressao, method = "duncan")$Grupo[,"pval"],
        bonf = PostHocTest(anova_Pressao, method = "bonf")$Grupo[,"pval"],
        hsd = PostHocTest(anova_Pressao, method = "hsd")$Grupo[,"pval"]),
  6)
# round: função "arredondar para x casas decimais".
# cbind: função para criar tabela com resumo de dados.

## Análise descritiva dos dados
describeBy(dados$BC, group = dados$Grupo)
describeBy(dados$Pressao, group = dados$Pressao)

##### RESPOSTA #####
# A ANOVA de uma via mostrou que há efeito do tratamento sobre a média de pressão arterial
# [F(2, 28) = 5,521; p = 0,0095]. O post-hoc Turkey HSD mostrou que há diferenças entre
# o grupo placebo e o grupo anti-hipertensivo padrão, mas não entre os demais grupos.