# UNIVERSIDADE FEDERAL DE PERNAMBUCO
# PROGRAMA DE POS-GRADUACAO EM CIENCIA POLITICA
# Professor: David Moreira
# Aluno: Anderson Henrique da Silva

#==============================================
#      ANALISE DE DADOS - UFPE 2019
#==============================================

#===========================================================
# Script da construção do banco de dados do artigo -
# "PERCEPÇÃO MACROECONÔMICA E FELICIDADE:  UM ESTUDO DE CASO DO BRASIL (2010-2016)"
#===========================================================



# Definir diretorio de busca do R
setwd("C:/Users/Anderson/Desktop/Artigo - MA e DA")



##Pacote para abrir banco de dados do LAPOP 
if(require(foreign) == F) install.packages('foreign'); require(foreign)

##Abrir banco de dados original do LAPOP
lapop_data2010 <- read.dta("lapop_brazil2010.dta")
lapop_data2012 <- read.dta("lapop_brazil2012.dta")
lapop_data2014 <- read.spss("lapop_brazil2014.sav", to.data.frame = T)
lapop_data2016 <- read.dta("lapop_brazil2016.dta")


##Selecionar variaveis e criar um novos bancos de dados com as variaveis de interesses

data2010 <- subset.data.frame(lapop_data2010, select = c ("pais", "q1",
                                                  "prov", "municipio",
                                                  "ls3","a4", "soct2", "idio2", "l1"))


data2012 <- subset.data.frame(lapop_data2012, select = c ("pais", "q1",
                                                          "prov", "municipio",
                                                          "ls3","a4", "soct2", "idio2", "l1"))


data2014 <- subset.data.frame(lapop_data2012, select = c ("pais", "q1",
                                                          "prov", "municipio",
                                                          "ls3","a4", "soct2", "idio2", "l1"))


data2016 <- subset.data.frame(lapop_data2016, select = c ("pais", "q1",
                                                  "prov", "municipio",
                                                  "ls3","a4", "soct2", "idio2", "l1"))







#============== Recodificação de variáveis dos bancos ==========

#================= 2010 ====================== 

# Ver as categorias presente na variável "felicidade"

table(data2010$ls3)



#Recodificar a variável 'ls3' para 'Felicidade' 

data2010$felicidade <- ifelse(data2010$ls3 == "Muito satisfeito(a)", 1,
                          ifelse(data2010$ls3 == "Pouco satisfeito(a)", 1, 0))


#Definir variável como numérico
data2010$felicidade <- as.numeric(data2010$felicidade)


##Ideologia

# Ver as categorias presente na variável "felicidade"

table(data2010$l1)

#Tranformar a variável de ideologia em "numérico"
data2010$l1_id <- as.numeric(data2010$l1)

# Criar a variável "Esquerda"
data2010$esquerda <- ifelse(data2010$l1_id <= 4, 1,0)

                            
# Direita
data2010$direita <- ifelse(data2010$l1_id >=7, 1,0)

#Transformação das variáveis criadas em numéricas em numéricas
data2010$esquerda <- as.numeric(data2010$esquerda)
data2010$direita <- as.numeric(data2010$direita)

#Remover a variável ls1 "como fator"
data2010$l1<- NULL



##Inflação

#Ver as categorias das variáveis
table(data2010$a4)



#Inflação - junção dos temas  "problemas com a economia" e "altos preços"
data2010$inflacao <- ifelse(data2010$a4 == "Economia, problemas com, crise de", 1,
                        ifelse(data2010$a4 == "Inflação, altos preços", 1, 0))





#Transformação em numérico
data2010$inflacao <- as.numeric(data2010$inflacao)



# Desemprego 
data2010$desemprego <- ifelse(data2010$a4 == "Desemprego/falta de emprego", 1, 0)


#Tranformaçao numerico
data2010$desemprego <- as.numeric(data2010$desemprego)



## Economia geral

#Ver as categorias da variável 
table(data2010$soct2)

# Percepção da Economia Geral
data2010$econ_geral <- ifelse(data2010$soct2 == "Melhor", 1,
                          ifelse(data2010$soct2 == "Igual", 2,
                                 ifelse(data2010$soct2 == "Pior", 3, 0)))

#Transformar em numerico
data2010$econ_geral <- as.numeric(data2010$econ_geral)


#Economia individual

#Ver as categorias da variável 
table(data2010$idio2)


#Percepção da Economia Individual
data2010$econ_ind <- ifelse(data2010$idio2 == "Melhor", 1,
                        ifelse(data2010$idio2 == "Igual", 2,
                               ifelse(data2010$idio2 == "Pior", 3, 0)))

#Tranformar em numerico
data2010$econ_ind <- as.numeric(data2010$econ_ind)

#Tranformar as variáveis de percepção economica geral e indivudual suas 
# suas categorias em fatores para a análise de regressão

data2010$econ_geral_fac <- factor(data2010$econ_geral)
data2010$econ_ind_fac <- factor(data2010$econ_ind)



## Criação da categoria de gênero tendo "homem" como referência

# Ver as categorias da variável
table(data2010$q1)

#Transformar em variável "homem"
data2010$homem <- ifelse(data2010$q1 == "Homem", 1, 0)

#tranformaçãoe numérico
data2010$homem <- as.numeric(data2010$homem)


#Omitir os NA do banco de dados
data2010 <- na.omit(data2010)

#Criar uma variável "Year" para mergir

data2010$year <- 2010

#================= 2012 ======================

# Ver as categorias presente na variável "felicidade"

table(data2012$ls3)



#Recodificar a variável 'ls3' para 'Felicidade' 

data2012$felicidade <- ifelse(data2012$ls3 == "Muito satisfeito(a)", 1,
                              ifelse(data2012$ls3 == "Pouco satisfeito(a)", 1, 0))


#Definir variável como numérico
data2012$felicidade <- as.numeric(data2012$felicidade)


##Ideologia

# Ver as categorias presente na variável "ideologia"

table(data2012$l1)

#Tranformar a variável de ideologia em "numérico"
data2012$l1_id <- as.numeric(data2012$l1)

# Criar a variável "Esquerda"
data2012$esquerda <- ifelse(data2012$l1_id <= 4, 1,0)


# Direita
data2012$direita <- ifelse(data2012$l1_id >=7, 1,0)

#Transformação das variáveis criadas em numéricas em numéricas
data2012$esquerda <- as.numeric(data2012$esquerda)
data2012$direita <- as.numeric(data2012$direita)

#Remover a variável ls1 "como fator"
data2012$l1<- NULL



##Inflação

#Ver as categorias das variáveis
table(data2012$a4)



#Inflação - junção dos temas  "problemas com a economia" e "altos preços"
data2012$inflacao <- ifelse(data2012$a4 == "Economia, problemas com, crise de", 1,
                            ifelse(data2012$a4 == "Inflação, altos preços", 1, 0))





#Transformação em numérico
data2012$inflacao <- as.numeric(data2012$inflacao)



# Desemprego 
data2012$desemprego <- ifelse(data2012$a4 == "Desemprego/falta de emprego", 1, 0)


#Tranformaçao numerico
data2012$desemprego <- as.numeric(data2012$desemprego)



## Economia geral

#Ver as categorias da variável 
table(data2012$soct2)

# Percepção da Economia Geral
data2012$econ_geral <- ifelse(data2012$soct2 == "Melhor", 1,
                              ifelse(data2012$soct2 == "Igual", 2,
                                     ifelse(data2012$soct2 == "Pior", 3, 0)))

#Transformar em numerico
data2012$econ_geral <- as.numeric(data2012$econ_geral)


#Economia individual

#Ver as categorias da variável 
table(data2012$idio2)


#Percepção da Economia Individual
data2012$econ_ind <- ifelse(data2012$idio2 == "Melhor", 1,
                            ifelse(data2012$idio2 == "Igual", 2,
                                   ifelse(data2012$idio2 == "Pior", 3, 0)))

#Tranformar em numerico
data2012$econ_ind <- as.numeric(data2012$econ_ind)

#Tranformar as variáveis de percepção economica geral e indivudual suas 
# suas categorias em fatores para a análise de regressão logistica

data2012$econ_geral_fac <- factor(data2012$econ_geral)
data2012$econ_ind_fac <- factor(data2012$econ_ind)



## Criação da categoria de gênero tendo "homem" como referência

# Ver as categorias da variável
table(data2012$q1)

#Transformar em variável "homem"
data2012$homem <- ifelse(data2012$q1 == "Homem", 1, 0)

#tranformaçãoe numérico
data2012$homem <- as.numeric(data2012$homem)


#Omitir os NA do banco de dados
data2012 <- na.omit(data2012)

#Criar uma variável "ano" para mergir

data2012$year <- 2012


#================= 2014 ============

# Ver as categorias presente na variável "felicidade"

table(data2014$ls3)



#Recodificar a variável 'ls3' para 'Felicidade' 

data2014$felicidade <- ifelse(data2014$ls3 == "Muito satisfeito(a)", 1,
                              ifelse(data2014$ls3 == "Pouco satisfeito(a)", 1, 0))


#Definir variável como numérico
data2014$felicidade <- as.numeric(data2014$felicidade)


##Ideologia

# Ver as categorias presente na variável "ideologia"

table(data2014$l1)

#Tranformar a variável de ideologia em "numérico"
data2014$l1_id <- as.numeric(data2014$l1)

# Criar a variável "Esquerda"
data2014$esquerda <- ifelse(data2014$l1_id <= 4, 1,0)


# Direita
data2014$direita <- ifelse(data2014$l1_id >=7, 1,0)

#Transformação das variáveis criadas em numéricas em numéricas
data2014$esquerda <- as.numeric(data2014$esquerda)
data2014$direita <- as.numeric(data2014$direita)

#Remover a variável ls1 "como fator"
data2014$l1<- NULL



##Inflação

#Ver as categorias das variáveis
table(data2014$a4)



#Inflação - junção dos temas  "problemas com a economia" e "altos preços"
data2014$inflacao <- ifelse(data2014$a4 == "Economia, problemas com, crise de", 1,
                            ifelse(data2014$a4 == "Inflação, altos preços", 1, 0))





#Transformação em numérico
data2014$inflacao <- as.numeric(data2014$inflacao)



# Desemprego 
data2014$desemprego <- ifelse(data2014$a4 == "Desemprego/falta de emprego", 1, 0)


#Tranformaçao numerico
data2014$desemprego <- as.numeric(data2014$desemprego)



## Economia geral

#Ver as categorias da variável 
table(data2014$soct2)

# Percepção da Economia Geral
data2014$econ_geral <- ifelse(data2014$soct2 == "Melhor", 1,
                              ifelse(data2014$soct2 == "Igual", 2,
                                     ifelse(data2014$soct2 == "Pior", 3, 0)))

#Transformar em numerico
data2014$econ_geral <- as.numeric(data2014$econ_geral)


#Economia individual

#Ver as categorias da variável 
table(data2014$idio2)


#Percepção da Economia Individual
data2014$econ_ind <- ifelse(data2014$idio2 == "Melhor", 1,
                            ifelse(data2014$idio2 == "Igual", 2,
                                   ifelse(data2014$idio2 == "Pior", 3, 0)))

#Tranformar em numerico
data2014$econ_ind <- as.numeric(data2014$econ_ind)

#Tranformar as variáveis de percepção economica geral e indivudual suas 
# suas categorias em fatores para a análise de regressão logistica

data2014$econ_geral_fac <- factor(data2014$econ_geral)
data2014$econ_ind_fac <- factor(data2014$econ_ind)



## Criação da categoria de gênero tendo "homem" como referência

# Ver as categorias da variável
table(data2014$q1)

#Transformar em variável "homem"
data2014$homem <- ifelse(data2014$q1 == "Homem", 1, 0)

#tranformaçãoe numérico
data2014$homem <- as.numeric(data2014$homem)


#Omitir os NA do banco de dados
data2014 <- na.omit(data2014)

#Criar uma variável "ano" para mergir

data2014$year <- 2014

#================= 2016 ============

# Ver as categorias presente na variável "felicidade"

table(data2016$ls3)



#Recodificar a variável 'ls3' para 'Felicidade' 

data2016$felicidade <- ifelse(data2016$ls3 == "Very Satisfied", 1,
                              ifelse(data2016$ls3 == "Somewhat Satisfied", 1, 0))


#Definir variável como numérico
data2016$felicidade <- as.numeric(data2016$felicidade)


##Ideologia

# Ver as categorias presente na variável "ideologia"

table(data2016$l1)

#Tranformar a variável de ideologia em "numérico"
data2016$l1_id <- as.numeric(data2016$l1)

# Criar a variável "Esquerda"
data2016$esquerda <- ifelse(data2016$l1_id <= 4, 1,0)


# Direita
data2016$direita <- ifelse(data2016$l1_id >=7, 1,0)

#Transformação das variáveis criadas em numéricas em numéricas
data2016$esquerda <- as.numeric(data2016$esquerda)
data2016$direita <- as.numeric(data2016$direita)

#Remover a variável ls1 "como fator"
data2016$l1<- NULL



##Inflação

#Ver as categorias das variáveis
table(data2016$a4)



#Inflação - junção dos temas  "problemas com a economia" e "altos preços"
data2016$inflacao <- ifelse(data2016$a4 == "Economy, problems with, crisis of", 1,
                            ifelse(data2016$a4 == "Inflation, high prices", 1, 0))





#Transformação em numérico
data2016$inflacao <- as.numeric(data2016$inflacao)



# Desemprego 
data2016$desemprego <- ifelse(data2016$a4 == "Unemployment", 1, 0)


#Tranformaçao numerico
data2016$desemprego <- as.numeric(data2016$desemprego)



## Economia geral

#Ver as categorias da variável 
table(data2016$soct2)

# Percepção da Economia Geral
data2016$econ_geral <- ifelse(data2016$soct2 == "Better", 1,
                              ifelse(data2016$soct2 == "Same", 2,
                                     ifelse(data2016$soct2 == "Worse", 3, 0)))

#Transformar em numerico
data2016$econ_geral <- as.numeric(data2016$econ_geral)


#Economia individual

#Ver as categorias da variável 
table(data2016$idio2)


#Percepção da Economia Individual
data2016$econ_ind <- ifelse(data2016$idio2 == "Better", 1,
                            ifelse(data2016$idio2 == "Same", 2,
                                   ifelse(data2016$idio2 == "Worse", 3, 0)))

#Tranformar em numerico
data2016$econ_ind <- as.numeric(data2016$econ_ind)

#Tranformar as variáveis de percepção economica geral e indivudual suas 
# suas categorias em fatores para a análise de regressão logistica

data2016$econ_geral_fac <- factor(data2016$econ_geral)
data2016$econ_ind_fac <- factor(data2016$econ_ind)



## Criação da categoria de gênero tendo "homem" como referência

# Ver as categorias da variável
table(data2016$q1)

#Transformar em variável "homem"
data2016$homem <- ifelse(data2016$q1 == "Male", 1, 0)

#tranformaçãoe numérico
data2016$homem <- as.numeric(data2016$homem)


#Omitir os NA do banco de dados
data2016 <- na.omit(data2016)


#Criar uma variável "ano" para mergir

data2016$year <- 2016

#============= MERGIR BANCO DE DADOS, CRIAR BANCO FINAL E SALVAR ===============

# Unir os bancos criados
data <- rbind(data2010, data2012, data2014, data2016)


#Selecionar apenas as variáveis de interesses para a análise

data_final <- subset.data.frame(data, select = c ("pais","year","municipio",
                                                  "felicidade", "homem", 
                                                          "prov",
                                                          "direita","esquerda",
                                                          "inflacao", "desemprego",
                                                          "econ_geral_fac", "econ_ind_fac"))

#Salvar banco de dados final

save(data_final,file="data_final.RData")
