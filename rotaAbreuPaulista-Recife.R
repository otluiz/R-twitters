#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

#rm(df) ## remove antigos objetos

## Carrega a matriz com a rota
#library(xlsx)
#dfRota <- read.xlsx("./data/Rotas/MatrizGravidadeRotaPaulistaQuipapa.xlsx")
dfRota <- read.csv2("./data/Rotas/RotaPaulistaQuipapa.csv", header = TRUE)

## renomeando as colunas para 0,1,... 175
headers <- names(dfRota)          ## cria um vetor com os nomes atuais das colunas
headers <- gsub("X", "", headers) ## retira o "X" dos nomes atuais das colunas
names(dfRota) <- headers          ## renomeia as colunas do dataframe somente com números
## quantas colunas há?
length(dfRota) ## 177 colunas
str(dfRota)

## soma as colunas da 1a parte da roda e guarda em soma1
dfRota$soma1 = rowSums(dfRota[,0:13])

#vecsoma1 contém a soma das colunas do primeiro treco da rota: colunas 0:13
vecSoma1 <- rowSums(dfRota[,0:13])             ### primeiro trecho, Br 101, início Paulista
vecSoma2 <- rowSums(dfRota[,14:147])           ### segundo trecho, Br 232 , trevo de Caruaru
vecSoma3 <- rowSums(dfRota[,147:ncol(dfRota)]) ### terceiro trecho, Br 104, término Quipapá

