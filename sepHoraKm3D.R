#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

#rm(df) ## remove antigos objetos

### Separação dos Data frames. Estes data frames são gerados pelo script
### calculoRNNeLogist.R onde a equação e a variável tx_Gravid232 é gerado
df.Br101 <- read.csv("./data/BR101/RNN.csv")
<<<<<<< HEAD
df.Br232 <- read.csv("./data/BR232/RNN.csv")
=======
df.Br232 <- read.csv("./data/BR232/RNN.csv") ## separador "," (vírgula)
>>>>>>> 992b3051b34ae4ec6cbb2b0b861c2e22c0cb40d7

df.Br116 <- read.csv2("./data/BR116/RNN.csv") ## separador ";" (ponto e vírgula)
#################################################################################
######################### Equação para predição #################################
#################################################################################

#for(i in 1:nrow(df.Br101)){
#  fit <- glm(df.Br101$Gravidade ~ df.Br101$tx_CondPista + df.Br101$tx_TipoAcident + df.Br101$tx_TracadoVia + df.Br101$Hour)
#}

# depois somar os mortos + df.Br101$TotMortos
# tx_Gravid101 = dfrnn.BR101$tx_RestVisibi * dfrnn.BR101$tx_CondPista * dfrnn.BR101$tx_TracadoVia * erroBR101 + dfrnn.BR101$Gravidade
#################################################################################
########################### regressão linear  ###################################
#################################################################################

#fitLin <- lm(df.Br101$KMArredondado ~ df.Br101$CondPista + df.Br101$TipoAcident + df.Br101$TracadoVia)



### função para crirar uma matriz de n rows e m columns
criarMatriz <- function(lin,col,alt) {
  rs = array(0, dim=c(lin, col, alt));
  return(rs)
}


################################################################################
############################             #######################################
###########################     BR 101      ####################################
############################             #######################################
################################################################################
#nrow(df.Br101)
#### Criar uma matriz já inicializada com todas as entradas em zero
criarMatrizFinal <- function(lin,col,alt){
  r <- criarMatriz(lin,col,alt)
  for(i in 1:nrow(df.Br101)){
    r[df.Br101[i,3]+1,df.Br101[i,4]+1,df.Br101[i,10]] <- r[df.Br101[i,3]+1,df.Br101[i,4]+1,df.Br101[i,10]] + df.Br101[i,12]
  }
  return(r)
}

l = 24  ## hora 0 - 23
c = max(df.Br101$KMArredondado)+1 ## quilômetro máximo da rodovia
a = 7    ## correspondente aos dias da semana
rs = criarMatrizFinal(l,c,a)


### Converte para data frame & Alterar o no me da linha e coluna
### estes comando foram covertidos na função salvaMatriz()
#dfs <- as.data.frame(rs[,,1])
#colnames(dfs) = c(0:(c -1))
#row.names(dfs) = c(0:(l -1))
#write.csv(dfs,"./data/BR101/MatrizMortos3D1.csv", row.names = FALSE)

################################################################################
### Salvar cada dia da semana em um arquivo diferente, ex.: MatrizMortos3d1.csv, MatrizMortos3D2.csv...
### Cada linha e coluna estão numeradas a partir do Km(0) e Hora(0)

salvaMatriz <- function(rs) {
  for(i in 1:7){
    dfs <- as.data.frame(rs[,,i])
    colnames(dfs) = c(0:(c -1))
    row.names(dfs) = c(0:(l -1))
    nomePart = paste("./data/BR101/MatrizGravidade3D",i, sep = "", collapse = NULL) 
    tipoExte = ".csv"
    nomeArq = paste(nomePart, tipoExte, sep = "") 
    write.csv(dfs,nomeArq, row.names = TRUE)
  }
}

salvaMatriz(rs)  ## grava no diretŕio ./data/BR101 arquivos do tipo MatrizGravidade3D -> correspondente a matriz de mortos de segunda-feira


################################################################################
############################             #######################################
###########################     BR 232      ####################################
############################             #######################################
################################################################################
#nrow(df.Br101)
#### Criar uma matriz já inicializada com todas as entradas em zero
criarMatrizFinal2 <- function(lin,col,alt){
  r <- criarMatriz(lin,col,alt)
  for(i in 1:nrow(df.Br232)){
    r[df.Br232[i,3]+1,df.Br232[i,4]+1,df.Br232[i,10]] <- r[df.Br232[i,3]+1,df.Br232[i,4]+1,df.Br232[i,10]] + df.Br232[i,12]
  }
  return(r)
}

l = 24  ## hora 0 - 23
c = max(df.Br232$KMArredondado)+1 ## quilômetro máximo da rodovia
a = 7    ## correspondente aos dias da semana
rs = criarMatrizFinal2(l,c,a)

################################################################################
### Salvar cada dia da semana em um arquivo diferente, ex.: MatrizMortos3d1.csv, MatrizMortos3D2.csv...
### Cada linha e coluna estão numeradas a partir do Km(0) e Hora(0)
salvaMatriz <- function(rs) {
  for(i in 1:7){
    dfs <- as.data.frame(rs[,,i])
    colnames(dfs) = c(0:(c -1))
    row.names(dfs) = c(0:(l -1))
    nomePart = paste("./data/BR232/MatrizGravidade3D",i, sep = "", collapse = NULL) 
    tipoExte = ".csv"
    nomeArq = paste(nomePart, tipoExte, sep = "") 
    write.csv(dfs,nomeArq, row.names = TRUE)
  }
}

salvaMatriz(rs)  ## grava no diretŕio ./data/Br232 arquivos do tipo MatrizGravidade3D -> correspondente a matriz de mortos de segunda-feira

################################################################################
############################             #######################################
###########################     BR 116      ####################################
############################             #######################################
################################################################################
#nrow(df.Br101)
#### Criar uma matriz já inicializada com todas as entradas em zero
criarMatrizFinal2 <- function(lin,col,alt){
  r <- criarMatriz(lin,col,alt)
  for(i in 1:nrow(df.Br116)){
    r[df.Br116[i,3]+1,df.Br116[i,4]+1,df.Br116[i,10]] <- r[df.Br116[i,3]+1,df.Br116[i,4]+1,df.Br116[i,10]] + df.Br116[i,12]
  }
  return(r)
}

l = 24  ## hora 0 - 23
c = max(df.Br116$KMArredondado)+1 ## quilômetro máximo da rodovia
a = 7    ## correspondente aos dias da semana
rs = criarMatrizFinal2(l,c,a)

################################################################################
### Salvar cada dia da semana em um arquivo diferente, ex.: MatrizMortos3d1.csv, MatrizMortos3D2.csv...
### Cada linha e coluna estão numeradas a partir do Km(0) e Hora(0)
salvaMatriz <- function(rs) {
  for(i in 1:7){
    dfs <- as.data.frame(rs[,,i])
    colnames(dfs) = c(0:(c -1))
    row.names(dfs) = c(0:(l -1))
    nomePart = paste("./data/BR116/MatrizGravidade3D",i, sep = "", collapse = NULL) 
    tipoExte = ".csv"
    nomeArq = paste(nomePart, tipoExte, sep = "") 
    write.csv2(dfs,nomeArq, row.names = TRUE)
  }
}

salvaMatriz(rs)  ## grava no diretŕio ./data/Br116 arquivos do tipo MatrizGravidade3D -> correspondente a matriz de mortos de segunda-feira
