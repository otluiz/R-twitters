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
#df <- read.csv("./data/BR101_RNN.csv") ## carrega o data frame

### Separar o data frame por hora
vetorHora <- split(df, df$Hour)
vetorKm <- split(df, df$KMArredondado)


## calcula quantidade de mortos o atributo Gravidade------------------------------------------------
df <- read.csv("./data/LimpoCheiroso.csv") ## carrega o data frame

### ajustando a variável Dia da semana, tem que criar uma nova coluna
for(i in 1:nrow(df)){
  if(df[i, "DiaDaSemana"] == "Segunda-feira") { df[i, "DiaSemana"] = 1 }
  if(df[i, "DiaDaSemana"] == "Terça-feira") { df[i, "DiaSemana"] = 2 }
  if(df[i, "DiaDaSemana"] == "Quarta-feira") { df[i, "DiaSemana"] = 3 }
  if(df[i, "DiaDaSemana"] == "Quinta-feira") { df[i, "DiaSemana"] = 4 }
  if(df[i, "DiaDaSemana"] == "Sexta-feira") { df[i, "DiaSemana"] = 5 }
  if(df[i, "DiaDaSemana"] == "Sábado") { df[i, "DiaSemana"] = 6 }
  if(df[i, "DiaDaSemana"] == "Domingo") { df[i, "DiaSemana"] = 7 }
}
#### limpando o dada frame
df$Binario <- NULL
df$Gravidade <- NULL
df$DiaDaSemana <- NULL
df$Municipio <- NULL
df$TotFerVivos <- NULL
df$TipoAuto <- NULL

###-----------------------------------------------------------------------------------------------------
df.Br101 <- subset(df, BR=='101')
df.Br232 <- subset(df, BR=='232')

write.csv(df.Br101,"./data/BR101/RNNLiteral.csv", row.names = FALSE)
write.csv(df.Br232,"./data/BR232/RNNLiteral.csv", row.names = FALSE)
## Procura em algum Km em determinada hora um acidente e conta-o
## Km: quilometro onde há uma ocorrência
## hr: hora em que ocorreu
## ds: dia da semana que ocorreu (segunda-feira, terça-feira...)
myCont <- function (hr, km, ds)  {
  y = 0
  for(i in 1:nrow(df.Br101)){
    if (df.Br101[i,8] == hr && df.Br101[i,9] == km && df.Br101[i,10] == ds) {
      y = y + df.Br101[i,10]
    }
  }
  return(y)
}

### função para crirar uma matriz de n linhas e m col e h alt
criarMatriz <- function(lin, col, alt) {
  arr = array(0, dim=c(lin, col, alt));
  l<-list(length=dim(arr)[3]);
  for (i in 1:dim(arr)[3]){
    l[[i]]<-arr[,,i];
  }
  return(arr)
}


criarMatrizFinal <- function(lin,col, alt){
  r <- criarMatriz(lin,col, alt)
  for(i in 0:(lin - 1)){
    for(j in 0:(col - 1)){
      for (k in 1:(alt)){
        r[i,j,k] <- myCont(i,j,k)
      }
    }
  }
  return(r)
}


hr = 24
km = max(df.Br101$KMArredondado)
ds = 7

matrizMortos <- criarMatrizFinal(hr,km,ds)






criarMatriz <- function(lin,col, alt) {
  my.matrix <- c()
  for(i in 1:lin){
    for(j in 1:col){
      for(k in 1:alt){
        my.matrix <- rbind(my.matrix, rep(0,0, alt))  
      }
    }
  }  
  return(my.matrix)
}

r = array(2, dim=c(8,9,2)); ## cria um array de 3 dimensões
l<-list(length=dim(r)[3]);
for (i in 1:dim(r)[3]){
  l[[i]]<-r[,,i];
}
tabela <- data.frame(r)  ## transforma uma matriz "r" em data frame
write.csv(tabela,"./data/BR101/tabela.csv", row.names = FALSE) ## escreve uma matriz em formato csv

### Matriz em 3 dimensões com nomes nas colunas e linhas e altura
(tres_d_array <- array(1:24, dim = c(4,3,2), dimnames = list(
  c("um", "dois", "três","quatro"),
  c("ein", "zwei", "drei"),
  c("un", "deux"))
))

