#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

#rm(df) ## remove antigos objetos

df <- read.csv("./data/RedeNeural/prfParaRNN.csv")
attach(df)

### agrupando hora em períodos
#for (i in 1:nrow(df)){
#  if (df[i,"Hour"] >= 0 & df[i,"Hour"] < 5 ) {df[i, "Periodo"] =  1 }   # Madrugada
#  if (df[i,"Hour"] >= 5 & df[i,"Hour"] < 12 ) {df[i, "Periodo"] =  2 }  # Manhã
#  if (df[i,"Hour"] >= 12 & df[i,"Hour"] < 18 ) {df[i, "Periodo"] =  3 } # Tarde
#  if (df[i,"Hour"] >= 18 & df[i,"Hour"] < 24 ) {df[i, "Periodo"] =  4}  # Noite
#}
### Separar as BRs da rota pretendida (BR 101 X BR 232)
df.BR101 <- subset(df,BR=='101')
df.BR232 <- subset(df,BR=='232')
df.BR104 <- subset(df,BR=='104')

### incluindo o fator de precisão (coluna Precision)
## caminho: /home/otluiz/Área de Trabalho/ADecisao-Weka/ArvoreBR.txt
df.BR101['TxErro'] <- 0.812
df.BR116['TxErro'] <- 0.669
df.BR232['TxErro'] <- 0.787

df.BR104['TxErro'] <- 0.957

df.BR423['TxErro'] <- 0.693
.
.
.
.
### salva os dados gerados até agora
write.csv(df.BR101,"./data/BR101/Bkp-parcial101.csv", row.names = FALSE) # salvando
write.csv(df.BR232,"./data/BR232/Bkp-parcial232.csv", row.names = FALSE) # salvando
################################################################################
############################             #######################################
###########################     BR 101      ####################################
############################             #######################################
################################################################################

df.Br101 <- read.csv("./data/BR101/RNNLiteral.csv")


#################################################################################
######################### Equação para predição ##################################
#################################################################################

#for(i in 1:nrow(df.Br101)){
#  fit <- glm(df.Br101$Gravidade ~ df.Br101$tx_CondPista + df.Br101$tx_TipoAcident + df.Br101$tx_TracadoVia + df.Br101$Hour)
#}

# depois somar os mortos + df.Br101$TotMortos

#################################################################################
########################### regressão linear  ###################################
#################################################################################

#fitLin <- lm(df.Br101$KMArredondado ~ df.Br101$CondPista + df.Br101$TipoAcident + df.Br101$TracadoVia)



### função para crirar uma matriz de n rows e m columns
criarMatriz <- function(lin,col,alt) {
  s = array(0, dim=c(lin, col, alt));
  return(s)
}

#nrow(df.Br101)
#### Criar uma matriz já inicializada com todas as entradas em zero
criarMatrizFinal <- function(lin,col,alt){
  r <- criarMatriz(lin,col,alt)
  for(i in 1:nrow(df.BR104)){
    r[df.BR104[i,8]+1,df.BR104[i,9]+1,df.BR104[i,10]] <- r[df.BR104[i,8]+1,df.BR104[i,9]+1,df.BR104[i,10]] + df.BR104[i,7]
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

salvaMatriz <- function(t) {
  for(i in 1:7){
    dfs <- as.data.frame(t[,,i])
    colnames(dfs) = c(0:(c -1))
    row.names(dfs) = c(0:(l -1))
    nomePart = paste("./data/BR101/MatrizMortos3D",i, sep = "", collapse = NULL) 
    tipoExte = ".csv"
    nomeArq = paste(nomePart, tipoExte, sep = "") 
    write.csv(dfs,nomeArq, row.names = TRUE)
  }
}

salvaMatriz(rs)  ## grava no diretŕio ./data/BR101 arquivos do tipo MatrizMortos3D1 -> correspondente a matriz de mortos de segunda-feira


