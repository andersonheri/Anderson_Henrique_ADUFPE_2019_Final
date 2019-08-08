# UNIVERSIDADE FEDERAL DE PERNAMBUCO
# PROGRAMA DE POS-GRADUACAO EM CIENCIA POLITICA
# Professor: David Moreira
# Aluno: Anderson Henrique da Silva

#==============================================
#      ANALISE DE DADOS - UFPE 2019
#==============================================

#===========================================================
# Script da análise, gráficos e testes do artigo - 
# "PERCEPÇÃO MACROECONÔMICA E FELICIDADE PARTIDARIA: 
#   UM ESTUDO DE CASO DO BRASIL (2010-2016)"
#===========================================================



# Definir diretorio de busca do R
setwd("C:/Users/Anderson/Desktop/Artigo - MA e DA")

# Abrir banco de dados 
load("data_final.RData")

#==================== Estatistica Descritiva ===================

#========= Produção das análises descritivas =======

#Pacotes

if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(GGally) == F) install.packages('GGally'); require(GGally)


# Apendix - Grafico 1

# Porcentagem de homens e Mulheres
#Tranformar a informacao em porcentagem

a <- as.data.frame(ftable(data_final$homem))
a <- transform(a, percentage = ave(Freq, FUN = function(x) as.numeric(paste0(round(x/sum(x), 3)*100))))
a$Var1 <- ifelse(a$Var1 == 1, "Homem",
                 ifelse(a$Var1 == 0, "Mulher", 0))

#Grafico
ggplot(a, aes(x=Var1, y=percentage), xlab = "") + 
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  geom_bar(position=position_dodge(), stat="identity", width=.5) +theme_minimal() +
  geom_col() +
  geom_text(aes(label=percentage), vjust=-0.1)

table(data_final$homem)
# Apendix - Grafico 2
# Percentagem Ideologia
#Porcentagem de ideologia

b <- as.data.frame(ftable(data_final$direita))
b <- transform(b, percentage = ave(Freq, FUN = function(x) as.numeric(paste0(round(x/sum(x), 3)*100))))
b$Var1 <- ifelse(b$Var1 == 1, "Direita",
                 ifelse(b$Var1 == 0, "Esquerda", 0))



#Gráfico

ggplot(b, aes(x=Var1, y=percentage), xlab = "Sexo") + 
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  geom_bar(position=position_dodge(), stat="identity", width=.5) +theme_minimal() +
  geom_col() +
  geom_text(aes(label=percentage), vjust=-0.1)



# Apendix - Grafico 3
#Percentagem de Felicidade
#Percentagem de pessoas que se consideram felizes

c <- as.data.frame(ftable(data_final$felicidade))
c <- transform(c, percentage = ave(Freq, FUN = function(x) as.numeric(paste0(round(x/sum(x), 3)*100))))
c$Var1 <- ifelse(c$Var1 == 1, "Felizes",
                 ifelse(c$Var1 == 0, "Infelizes", 0))



ggplot(c, aes(x=Var1, y=percentage), xlab = "") + 
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  geom_bar(position=position_dodge(), stat="identity", width=.5) +theme_minimal() +
  geom_col() +
  geom_text(aes(label=percentage), vjust=-0.1)



#Descritiva da quantidade de pessoas que se preocupam com Inflação e Desemprego

#Quantidade de pessoas que se preocupam com a inflacao 
table(data_final$inflacao)

#Quantidade de pessoas que se preocupam com a desemprego 
table(data_final$desemprego)


#============== Elaboracao do modelo multivariado de Regressao Logistica ==============


#Modelo de pessoas que se identifica com "Esquerda"
mylogit <- glm(felicidade ~ esquerda + inflacao + desemprego + homem +
                 econ_geral_fac + econ_ind_fac,
               family = "binomial", data = data_final)


summary(mylogit)

#Modelo interativo de pessoas que se identifica com "Esquerda"

mylogit1 <- glm(felicidade ~ esquerda + inflacao + desemprego + homem +
                  econ_geral_fac + econ_ind_fac + esquerda*desemprego,
                family = "binomial", data = data_final)


summary(mylogit1)


#Modelo de pessoas que se identifica com "Direita"

mylogit2 <- glm(felicidade ~ direita + inflacao + desemprego + homem +
                  econ_geral_fac + econ_ind_fac,
                family = "binomial", data = data_final)

summary(mylogit2)

#Modelo interativo de pessoas que se identifica com "Direita"


mylogit3 <- glm(felicidade ~ direita + inflacao + desemprego + homem +
                  econ_geral_fac + econ_ind_fac + direita*inflacao,
                family = "binomial", data = data_final)

summary(mylogit3)


# Modelo com todas as variaveis

mylogit4 <- glm(felicidade ~ direita + esquerda + inflacao + desemprego + homem +
                  econ_geral_fac + econ_ind_fac + direita*inflacao + 
                  esquerda*desemprego,
                family = "binomial", data = data_final)

summary(mylogit4)

#Comparar ambos os modelos  

#Abrir pacote de comparacao e visualizacao dos modelos
if(require(stargazer) == F) install.packages('stargazer'); require(stargazer)

#Execução da funcao
stargazer(mylogit,mylogit1, mylogit2, mylogit3, mylogit4, type="text", 
          column.labels = c("Esquerda", " Esquerda*Desemprego",
                            "Direita", "Direita*Inflação", "Todos"),
          intercept.bottom = FALSE,
          single.row=FALSE,
          notes.append = FALSE,
          header=FALSE)


#============= Testes de Robustes dos modelos de Regressao Logistica ============


#Testes de predição do modelo - Escore otimo, Teste de concordancia,
#Erro de classificação incorreta e Roc Curve.

#Carregar pacotes necessarios
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(caret) == F) install.packages('caret'); require(caret)
if(require(smbinning) == F) install.packages('smbinning'); require(smbinning)
if(require(InformationValue) == F) install.packages('InformationValue'); require(InformationValue)



## Teste de Robustes - Modelo 1

#Estimar os valores preditos do modelo de Regressao Logistica
predicted <- predict(mylogit, data_final, type="response") 

#Optimal Score
optCutOff <- optimalCutoff(data_final$felicidade, predicted)[1] 
optCutOff

#teste de Concordancia do banco de dados
Concordance(data_final$felicidade, predicted)


#Teste de Sensitividade e Especificidade do banco de dados
#Sensitividade
sensitivity(data_final$felicidade, predicted, threshold = optCutOff)

#Especificidade
specificity(data_final$felicidade, predicted, threshold = optCutOff)


# Erro de classificacao incorreta (porcentagem dos valores previstos x valores reais)
# do modelo
misClassError(data_final$felicidade, predicted, threshold = optCutOff)

#ROC Curve do modelo de RL
plotROC(data_final$felicidade, predicted) #Gráfico



## Teste de Robustes - Modelo 2

#Estimar os valores preditos do modelo de Regressao Logistica
predicted1 <- predict(mylogit1, data_final, type="response") 

#Optimal Score
optCutOff <- optimalCutoff(data_final$felicidade, predicted1)[1] 
optCutOff

#teste de Concordancia do banco de dados
Concordance(data_final$felicidade, predicted1)


#Teste de Sensitividade e Especificidade do banco de dados
#Sensitividade
sensitivity(data_final$felicidade, predicted1, threshold = optCutOff)

#Especificidade
specificity(data_final$felicidade, predicted1, threshold = optCutOff)


# Erro de classificacao incorreta (porcentagem dos valores previstos x valores reais)
# do modelo
misClassError(data_final$felicidade, predicted1, threshold = optCutOff)

#ROC Curve do modelo de RL
plotROC(data_final$felicidade, predicted1) #Gráfico

## Modelo 3

## Teste de Robustes - Modelo 3

#Estimar os valores preditos do modelo de Regressao Logistica
predicted2 <- predict(mylogit2, data_final, type="response") 

#Optimal Score
optCutOff <- optimalCutoff(data_final$felicidade, predicted2)[1] 
optCutOff

#teste de Concordancia do banco de dados
Concordance(data_final$felicidade, predicted2)


#Teste de Sensitividade e Especificidade do banco de dados
#Sensitividade
sensitivity(data_final$felicidade, predicted2, threshold = optCutOff)

#Especificidade
specificity(data_final$felicidade, predicted2, threshold = optCutOff)


# Erro de classificacao incorreta (porcentagem dos valores previstos x valores reais)
# do modelo
misClassError(data_final$felicidade, predicted2, threshold = optCutOff)

#ROC Curve do modelo de RL
plotROC(data_final$felicidade, predicted2) #Gráfico


## Modelo 4

## Teste de Robustes - Modelo 4

#Estimar os valores preditos do modelo de Regressao Logistica
predicted3 <- predict(mylogit3, data_final, type="response") 

#Optimal Score
optCutOff <- optimalCutoff(data_final$felicidade, predicted3)[1] 
optCutOff

#teste de Concordancia do banco de dados
Concordance(data_final$felicidade, predicted3)


#Teste de Sensitividade e Especificidade do banco de dados
#Sensitividade
sensitivity(data_final$felicidade, predicted3, threshold = optCutOff)

#Especificidade
specificity(data_final$felicidade, predicted3, threshold = optCutOff)


# Erro de classificacao incorreta (porcentagem dos valores previstos x valores reais)
# do modelo
misClassError(data_final$felicidade, predicted3, threshold = optCutOff)

#ROC Curve do modelo de RL
plotROC(data_final$felicidade, predicted3) #Gráfico

## Modelo 5

## Teste de Robustes - Modelo 5

#Estimar os valores preditos do modelo de Regressao Logistica
predicted4 <- predict(mylogit4, data_final, type="response") 

#Optimal Score
optCutOff <- optimalCutoff(data_final$felicidade, predicted4)[1] 
optCutOff

#teste de Concordancia do banco de dados
Concordance(data_final$felicidade, predicted4)


#Teste de Sensitividade e Especificidade do banco de dados
#Sensitividade
sensitivity(data_final$felicidade, predicted4, threshold = optCutOff)

#Especificidade
specificity(data_final$felicidade, predicted4, threshold = optCutOff)


# Erro de classificacao incorreta (porcentagem dos valores previstos x valores reais)
# do modelo
misClassError(data_final$felicidade, predicted4, threshold = optCutOff)

#ROC Curve do modelo de RL
plotROC(data_final$felicidade, predicted4) #Gráfico

