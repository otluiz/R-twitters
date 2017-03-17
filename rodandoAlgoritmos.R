setwd("~/workspace/R/src")

# free memory
rm(list = ls())
gc()

# dados$x <- NULL ## remove a coluna x
dfT <- read.csv("../data/prfCalculoTaxas.csv") ## carrega o data frame

attach(dfT)
### estrutura do data frame
qtdLinhas <- nrow(dfT)
qtdColunas <- ncol(dfT)
str(dfT)

## calcula e converte para booleano o atributo Gravidade------------------------------------------------
for(i in 1:nrow(dfT)){
  if(dfT[i,"Gravidade"] == "FALSE") { dfT[i,"Gravidade"] = 0 }
  if(dfT[i,"Gravidade"] == "TRUE" ) { dfT[i,"Gravidade"] = 1 }
}


### sumarizando os dados
summary(dfT)


### frequencia absoluta das variáveis
fa = table(BR)
faK = table(KMArredondado)

### frequencia relativa
fr = fa/sum(fa)
frk = faK/sum(faK)

### cria uma coluna com valores lá dentro
dfT["tx_BR"] <- c(qtdLinhas)
dfT["tx_Km"] <- c(qtdColunas)
y = c(tx_BR)
z = c(faK)

## calcula e adiciona a frequencia relativa da variável BR ------------------------------------------------
for(i in 1:qtdLinhas){
  if(dfT[i,"BR"] == "101") { dfT[i,"tx_BR"] = round(y[1],3) }
  if(dfT[i,"BR"] == "104") { dfT[i,"tx_BR"] = round(y[2],3) }
  if(dfT[i,"BR"] == "110") { dfT[i,"tx_BR"] = round(y[3],3) }
  if(dfT[i,"BR"] == "116") { dfT[i,"tx_BR"] = round(y[4],3) }
  if(dfT[i,"BR"] == "232") { dfT[i,"tx_BR"] = round(y[5],3) }
  if(dfT[i,"BR"] == "316") { dfT[i,"tx_BR"] = round(y[6],3) }
  if(dfT[i,"BR"] == "407") { dfT[i,"tx_BR"] = round(y[7],3) }
  if(dfT[i,"BR"] == "408") { dfT[i,"tx_BR"] = round(y[8],3) }
  if(dfT[i,"BR"] == "423") { dfT[i,"tx_BR"] = round(y[9],3) }
  if(dfT[i,"BR"] == "424") { dfT[i,"tx_BR"] = round(y[10],3) }
  if(dfT[i,"BR"] == "428") { dfT[i,"tx_BR"] = round(y[11],3) }
}


for(i in 1:qtdLinhas){
  #### calcula a taxa (tx_Km) com o valor fornecido da coluna KMArredondado
  #dfT[i,"tx_Km"] = round(frk[dfT[i,"KMArredondado"]+1],3)
  dfT[i,"tx_BR"] = round(dfT[i,"tx_BR"],3) 
}

### arredinda valores das colunas para 3 casas decimais
for(i in 1:qtdLinhas){
  #### calcula a taxa (tx_Km) com o valor fornecido da coluna KMArredondado
  #dfT[i,"tx_RestVisibi"] = round(dfT[i,"tx_RestVisibi"],3)
  #dfT[i,"tx_CondPista"] = round(dfT[i,"tx_CondPista"],3)
  #dfT[i,"tx_DiaSemana"] = round(dfT[i,"tx_DiaSemana"],3)
  #dfT[i,"tx_CausaAcident"] = round(dfT[i,"tx_CausaAcident"],3)
  #dfT[i,"tx_TracadoVia"] = round(dfT[i,"tx_TracadoVia"],3)
  dfT[i,"tx_TipoAcident"] = round(dfT[i,"tx_TipoAcident"],3)
}






### salvando alterações 
write.csv(dfT,"../data/prfCalculoTaxas2.csv", row.names = FALSE)
