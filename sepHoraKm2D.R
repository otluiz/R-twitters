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

## calcula quantidade de mortos o atributo Gravidade------------------------------------------------
df <- read.csv("./data/LimpoCheiroso.csv") ## carrega o data frame

#### limpando dados
df$Binario <- NULL
df$Gravidade <- NULL
df$DiaDaSemana <- NULL
df$Municipio <- NULL
df$TotFerVivos <- NULL
df$TipoAuto <- NULL


###-----------------------------------------------------------------------------------------------------
df.Br101 <- subset(df,BR=='101')
df.Br232 <- subset(df,BR=='232')
df.Br104 <- subset(df,BR=='104')

write.csv(df.Br101,"./data/BR101/RNNLiteral.csv", row.names = FALSE)
write.csv(df.Br232,"./data/BR232/RNNLiteral.csv", row.names = FALSE)
write.csv(df.Br104,"./data/BR104/RNNLiteral.csv", row.names = FALSE)

# Procura em algum Km em determinada hora um acidente e conta-o
#myCont <- function (km,hr)  {
#  y = 0
#  for(i in 1:nrow(df.Br101)){
#    if (df.Br101[i,13] == km && df.Br101[i,12] == hr) {
#      y = y + df.Br101[i,10]
#    }
#  }
#  return(y)
#}

### função para crirar uma matriz de n rows e m columns
criarMatriz <- function(lin,col) {
  my.matrix <- c()
  for(i in 1:lin){
    my.matrix <- rbind(my.matrix, rep(0,col))
  }  
  return(my.matrix)
}

#nrow(df.Br101)

criarMatrizFinal <- function(lin,col){
  r <- criarMatriz(lin,col)
  for(i in 1:nrow(df.Br101)){
    r[df.Br101[i,8]+1,df.Br101[i,9]+1] <- r[df.Br101[i,8]+1,df.Br101[i,9]+1] + df.Br101[i,7]
  }
  return(r)
}
### Definir número de linhas e colunas para a matriz de mortos
l = 24
c = max(df.Br101$KMArredondado)+1
### Criar a Matriz de Mortos que conterá o número de mortos em cada KM / Hora
s = criarMatrizFinal(l,c)

### Converte para data frame & Alterar o no me da linha e coluna
dfs <- as.data.frame(s)
colnames(dfs) = c(0:(c -1))
row.names(dfs) = c(0:(l -1))

write.csv(dfs,"./data/BR101/MatrizMortos2D.csv", row.names = FALSE)
write.csv2(dfs,"./data/BR101/MatrizMortos2D2.csv", row.names = FALSE, sep = ";") ## separador para planilhas

################################################################################
############################             #######################################
###########################     BR 232      ####################################
############################             #######################################
################################################################################
### função para crirar uma matriz de n rows e m columns
#criarMatriz <- function(lin,col) {
#  my.matrix <- c()
#  for(i in 1:lin){
#    my.matrix <- rbind(my.matrix, rep(0,col))
#  }  
#  return(my.matrix)
#}

### Cria uma matriz genérica 2 X 2
criarMatrizFinal <- function(lin,col){
  r <- criarMatriz(lin,col)
  for(i in 1:nrow(df.Br232)){
    r[df.Br232[i,8]+1,df.Br232[i,9]+1] <- r[df.Br232[i,8]+1,df.Br232[i,9]+1] + df.Br232[i,7]
  }
  return(r)
}

### Define as dimensões da matriz de Mortos e cria
l = 24
c = max(df.Br232$KMArredondado)+1
mm232 = criarMatrizFinal(l,c)

### Converte para data frame & Alterar o no me da linha e coluna
df232 <- as.data.frame(mm232)
colnames(df232) = c(0:(c -1))
row.names(df232) = c(0:(l -1))

write.csv(df232,"./data/BR232/MatrizMortos2D.csv", row.names = FALSE)

################################################################################
############################             #######################################
###########################     BR 104      ####################################
############################             #######################################
################################################################################

###-----------------------------------------------------------------------------
#df.Br101 <- subset(df,BR=='101')
#df.Br232 <- subset(df,BR=='232')
df.Br104 <- subset(df,BR=='104')

#write.csv(df.Br101,"./data/BR101/RNNLiteral.csv", row.names = FALSE)
#write.csv(df.Br232,"./data/BR232/RNNLiteral.csv", row.names = FALSE)

# Procura em algum Km em determinada hora um acidente e conta-o
#myCont <- function (km,hr)  {
#  y = 0
#  for(i in 1:nrow(df.Br232)){
#    if (df.Br232[i,13] == km && df.Br232[i,12] == hr) {
#      y = y + df.Br232[i,10]
#    }
#  }
#  return(y)
#}

### função para crirar uma matriz de n rows e m columns
criarMatriz <- function(lin,col) {
  my.matrix <- c()
  for(i in 1:lin){
    my.matrix <- rbind(my.matrix, rep(0,col))
  }  
  return(my.matrix)
}

### Cria uma matriz genérica 2 X 2
criarMatrizFinal <- function(lin,col){
  r <- criarMatriz(lin,col)
  for(i in 1:nrow(df.Br104)){
    r[df.Br104[i,8]+1,df.Br104[i,9]+1] <- r[df.Br104[i,8]+1,df.Br104[i,9]+1] + df.Br104[i,7]
  }
  return(r)
}

### Define as dimensões da matriz de Mortos e cria
l = 24
c = max(df.Br104$KMArredondado)+1
mm104 = criarMatrizFinal(l,c)

### Converte para data frame & Alterar o no me da linha e coluna
df104 <- as.data.frame(mm104)
colnames(df104) = c(0:(c -1))
row.names(df104) = c(0:(l -1))

write.csv(df104,"./data/BR104/MatrizMortos.csv", row.names = FALSE)

