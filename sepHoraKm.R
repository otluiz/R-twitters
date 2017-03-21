#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

################################################################################
############################             #######################################
###########################     BR 101      ####################################
############################             #######################################
################################################################################


rm(df) ## remove antigos objetos
df <- read.csv("./data/BR101_RNN.csv") ## carrega o data frame

### Separar o data frame por hora
vetorHora <- split(df, df$Hour)
vetorKm <- split(df, df$KMArredondado)


## calcula quantidade de mortos o atributo Gravidade------------------------------------------------
df.Br101 <- read.csv("./data/LimpoCheiroso.csv") ## carrega o data frame

#### limpando dados
df.Br101$Binario <- NULL
df.Br101$Gravidade <- NULL

###-----------------------------------------------------------------------------------------------------
df.Br101 <- subset(df.Br101,BR=='101')
df.Br232 <- subset(df.Br101,BR=='232')

# Procura em algum Km em determinada hora um acidente e conta-o
myCont <- function (km,hr)  {
  y = 0
  for(i in 1:nrow(df.Br101)){
    if (df.Br101[i,13] == km && df.Br101[i,12] == hr) {
      y = y + df.Br101[i,10]
    }
  }
  return(y)
}

### função para crirar uma matriz de n rows e m columns
criarMatriz <- function(lin,col) {
  my.matrix <- c()
  for(i in 1:lin){
    my.matrix <- rbind(my.matrix, rep(0,col))
  }  
  return(my.matrix)
}


criarMatrizFinal <- function(lin,col){
  r <- criarMatriz(lin,col)
  for(i in 0:(lin - 1)){
    for(j in 0:(col - 1)){
      r[i,j] <- myCont(i,j)
    }
  }
  return(r)
}


l = 24
c = max(df.Br101$KMArredondado)

criarMatrizFinal(l,c)



