#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

rm(dfrnn) ## remove antigos objetos
dfrnn <- read.csv("./data/prfCalculoTaxas.csv") ## carrega o data frame
#str(dfrnn)
#levels(dfrnn$RestrVisibili) 
#table(dfrnn$RestrVisibili)
### Atacchar a variável: dfrnn$TipoAuto => TipoAuto
attach(dfrnn)

### Verificar os dados
dfrnn.novo <- dfrnn[order(KMArredondado, Hour),]

#### remover colunas desnecessárias
#dfrnn$TipoAuto <- NULL
#dfrnn$Delegacia <- NULL
#dfrnn$CondPista <- NULL
#dfrnn$TracadoVia <- NULL
#dfrnn$TipoAcident <- NULL
#dfrnn$CausaAcident <- NULL
#dfrnn$tx_DiaSemana <- NULL
#dfrnn$RestrVisibili <- NULL


### Arredondar variáveis para 3 casas decimais
for(i in 1:nrow(dfrnn)){
  dfrnn$tx_RestVisibi   <- round(dfrnn$tx_RestVisibi,3)
  dfrnn$tx_CondPista    <- round(dfrnn$tx_CondPista,3)
  dfrnn$tx_TipoAcident  <- round(dfrnn$tx_TipoAcident,3)
  dfrnn$tx_CausaAcident <- round(dfrnn$tx_CausaAcident,3)
  dfrnn$tx_TracadoVia   <- round(dfrnn$tx_TracadoVia,3)
}

str(Hour)
### Gravar uma cópia para, os "for" são demorados
#write.csv(dfrnn,"./data/prfParaRNN.csv", row.names = FALSE)
### Ler a cópia gravada e prosseguir
#dfrnn <- read.csv("./data/prfParaRNN.csv")
attach(dfrnn)

### Corverter dados categóricos para numéricos
### ajustando a variável Gravidade
for(i in 1:nrow(dfrnn)){
  if(dfrnn[i,"Gravidade"] == "FALSE") { dfrnn[i,"Gravidade"] = 0 }
  if(dfrnn[i,"Gravidade"] == "TRUE" ) { dfrnn[i,"Gravidade"] = 1 }
}

### ajustando a variável Dia da semana, tem que criar uma nova coluna
for(i in 1:nrow(dfrnn)){
  if(dfrnn[i, "DiaDaSemana"] == "Segunda-feira") { dfrnn[i, "DiaSemana"] = 1 }
  if(dfrnn[i, "DiaDaSemana"] == "Terça-feira") { dfrnn[i, "DiaSemana"] = 2 }
  if(dfrnn[i, "DiaDaSemana"] == "Quarta-feira") { dfrnn[i, "DiaSemana"] = 3 }
  if(dfrnn[i, "DiaDaSemana"] == "Quinta-feira") { dfrnn[i, "DiaSemana"] = 4 }
  if(dfrnn[i, "DiaDaSemana"] == "Sexta-feira") { dfrnn[i, "DiaSemana"] = 5 }
  if(dfrnn[i, "DiaDaSemana"] == "Sábado") { dfrnn[i, "DiaSemana"] = 6 }
  if(dfrnn[i, "DiaDaSemana"] == "Domingo") { dfrnn[i, "DiaSemana"] = 7 }
}

### agrupando hora em períodos
for (i in 1:nrow(dfrnn)){
  if (dfrnn[i,"Hour"] >= 0 & dfrnn[i,"Hour"] < 5 ) {dfrnn[i, "Periodo"] =  1 }   # Madrugada
  if (dfrnn[i,"Hour"] >= 5 & dfrnn[i,"Hour"] < 12 ) {dfrnn[i, "Periodo"] =  2 }  # Manhã
  if (dfrnn[i,"Hour"] >= 12 & dfrnn[i,"Hour"] < 18 ) {dfrnn[i, "Periodo"] =  3 } # Tarde
  if (dfrnn[i,"Hour"] >= 18 & dfrnn[i,"Hour"] < 24 ) {dfrnn[i, "Periodo"] =  4}  # Noite
}


### Gravar uma cópia para, os "for" são demorados
write.csv(dfrnn,"./data/prfParaRNN.csv", row.names = FALSE)

#####################################################################################
### Ler a cópia gravada e prosseguir
#dfrnn <- read.csv("./data/prfParaRNN.csv")
attach(dfrnn)

### Ler a cópia gravada e prosseguir
#dfrnn <- read.csv("./data/RedeNeural/prfParaRNN.csv")
dfrn <- read.csv("./data/prfParaRNN.csv")
attach(dfrn)


### Separar as BRs da rota pretendida (BR 101 X BR 232)
dfrn.BR101 <- subset(dfrn,BR=='101')
dfrn.BR232 <- subset(dfrn,BR=='232')
dfrn.BR104 <- subset(dfrn,BR=='104')
dfrn.BR116 <- subset(dfrn,BR=='116')


### incluindo o fator de precisão em cada data.frame Vindo da Árvore de Decisão
erroBR101 <- 0.812
erroBR232 <- 0.787
erroBR104 <- 0.957
erroBR116 <- 0.669

### calculando Prob. ser  Gravidade
## BR 101
tx_Gravid101 = (dfrnn.BR101$tx_RestVisibi + dfrnn.BR101$tx_CondPista + dfrnn.BR101$tx_TracadoVia) *  erroBR101 + dfrnn.BR101$Gravidade
dfrnn.BR101["tx_Gravidade"] <- round(tx_Gravid101,3)

## BR 232
tx_Gravid232 = (dfrnn.BR232$tx_RestVisibi + dfrnn.BR232$tx_CondPista + dfrnn.BR232$tx_TracadoVia) *  erroBR232 + dfrnn.BR232$Gravidade
dfrnn.BR232["tx_Gravidade"] <- round(tx_Gravid232,3)

## BR 104
tx_Gravid104 = (dfrn.BR104$tx_RestVisibi + dfrn.BR104$tx_CondPista + dfrn.BR104$tx_TracadoVia) *  erroBR104 + dfrn.BR104$Gravidade
dfrn.BR104["tx_Gravidade"] <- round(tx_Gravid104,3)

## BR 116
tx_Gravid116 = (dfrn.BR116$tx_RestVisibi + dfrn.BR116$tx_CondPista + dfrn.BR116$tx_TracadoVia) *  erroBR116 + dfrn.BR116$Gravidade
dfrn.BR116["tx_Gravidade"] <- round(tx_Gravid116,3)



### exportando dados CSV e ARFF (para Weka)
write.csv(dfrnn.BR101,"./data/BR101/RNN.csv", row.names = FALSE)
write.csv(dfrnn.BR232,"./data/BR232/RNN.csv", row.names = FALSE)

## COM ";" SEPARADOR
write.csv2(dfrn.BR104,"./data/BR104/RNN.csv", row.names = FALSE)
write.csv2(dfrn.BR116,"./data/BR116/RNN.csv", row.names = FALSE)

library(RWeka)
write.arff(dfrnn.BR101,"../weka/BR101/RNN.arff", eol = "\n")
write.arff(dfrnn.BR232,"../weka/BR232/RNN.arff", eol = "\n")


######## Outras BRs #########
#BR104 <- subset(dfrnn,BR=='104')
BR110 <- subset(dfrnn,BR=='110')
#BR116 <- subset(dfrnn,BR=='116')
BR316 <- subset(dfrnn,BR=='316')
BR407 <- subset(dfrnn,BR=='407')
BR408 <- subset(dfrnn,BR=='408')
BR423 <- subset(dfrnn,BR=='423')
BR424 <- subset(dfrnn,BR=='424')
BR428 <- subset(dfrnn,BR=='428')
