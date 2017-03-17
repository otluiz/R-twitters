setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

library("entropy")
y=c(4,2,3,0,2,4,0,0,2,1,1)
entropy(y, method = "ML")
entropy(y, method = "MM")
entropy(y, method = "Jeffreys")
entropy(y, method = "Laplace")
entropy(y, method = "minimax")
entropy(y, method = "CS")


#-------[ TODO o dataset para ser separado em conjunto de treinamento e testes  ]------------------------
dfN <- read.csv("../data/prfPeriodoTx.csv") ## carrega o data frame

##  Ajuste para Arvore Decisão    ------------------------------------------------
for(i in 1:nrow(dfN)){
  if(dfN[i,"Gravidade"] == 0) { dfN[i,"Gravidade"] = "N" }
  if(dfN[i,"Gravidade"] == 1) { dfN[i,"Gravidade"] = "S" }
}

##  Adicionar BR na frente do número ex 101 => BR101   --------------------------------
for(i in 1:nrow(dfN)){
  if(dfN[i,"BR"] == 101) { dfN[i,"BR"] = "BR101" }
  if(dfN[i,"BR"] == 104) { dfN[i,"BR"] = "BR104" }
  if(dfN[i,"BR"] == 110) { dfN[i,"BR"] = "BR110" }
  if(dfN[i,"BR"] == 116) { dfN[i,"BR"] = "BR116" }
  if(dfN[i,"BR"] == 232) { dfN[i,"BR"] = "BR232" }
  if(dfN[i,"BR"] == 316) { dfN[i,"BR"] = "BR316" }
  if(dfN[i,"BR"] == 407) { dfN[i,"BR"] = "BR407" }
  if(dfN[i,"BR"] == 408) { dfN[i,"BR"] = "BR408" }
  if(dfN[i,"BR"] == 423) { dfN[i,"BR"] = "BR423" }
  if(dfN[i,"BR"] == 424) { dfN[i,"BR"] = "BR424" }
  if(dfN[i,"BR"] == 428) { dfN[i,"BR"] = "BR428" }
}

write.csv(dfN,"../data/prfBRTratada.csv", row.names = FALSE)

# entropy.ChaoShen(z, unit=c("log", "log2", "log10"))

dfN <- read.csv("./data/prfPeriodoTx.csv") ## carrega o data frame
attach(dfN)
str(dfN)


###### calculo da entropia para variável Gravidade @@@@@@@@@@@@@@@@@@@@@@@@@@@
cont <- c(Gravidade)
freq <- table(cont)/length(cont)
-sum(freq * log2(freq))
## tx = 0.9997114
entropy.ChaoShen(freq, unit=c("log", "log2", "log10"))

###### calculo da entropia para variável RestrVisibili @@@@@@@@@@@@@@@@@@@@@@@
contTxRest <- c(RestrVisibili)
freqTxRest <- table(contTxRest)/length(contTxRest)
-sum(freqTxRest * log2(freqTxRest))
## tx = 0.5537316
entropy.ChaoShen(freqTxRest, unit=c("log", "log2", "log10"))

###### calculo da entropia para variável Tipo de Acidente @@@@@@@@@@@@@@@@@@@@@@@

contTxAcident <- c(TipoAcident)
freqTxAcident <- table(contTxAcident)/length(contTxAcident)
-sum(freqTxAcident * log2(freqTxAcident))
## entropia = 3.068937
entropy.ChaoShen(freqTxAcident, unit=c("log", "log2", "log10"))

###### calculo da entropia para variável Causa do Acidente @@@@@@@@@@@@@@@@@@@@@@@

contCausaAcident <- c(CausaAcident)
freqCausaAcident <- table(contCausaAcident)/length(contCausaAcident)
-sum(freqCausaAcident * log2(freqCausaAcident))
## entropia = 2.692134
entropy.ChaoShen(freqTxAcident, unit=c("log", "log2", "log10"))

###### calculo da entropia para variável BR @@@@@@@@@@@@@@@@@@@@@@@

contBR <- c(BR)
freqBR <- table(contBR)/length(contBR)
-sum(freqBR * log2(freqBR))
## entropia = 2.412813
entropy.ChaoShen(freqBR, unit=c("log", "log2", "log10"))

###### calculo da entropia para variável Tracado da Via @@@@@@@@@@@@@@@@@@@@@@@

contTrçVia <- c(TracadoVia)
freqTrçVia <- table(contTrçVia)/length(contTrçVia)
-sum(freqTrçVia * log2(freqTrçVia))
## entropia = 0.8309614
entropy.ChaoShen(freqTrçVia, unit=c("log", "log2", "log10"))

###### calculo da entropia para variável TipoAuto @@@@@@@@@@@@@@@@@@@@@@@

contTipoAuto <- c(TipoAuto)
freqTipoAuto <- table(contTipoAuto)/length(contTipoAuto)
-sum(freqTipoAuto * log2(freqTipoAuto))
## entropia = 3.171005
entropy.ChaoShen(freqTipoAuto, unit=c("log", "log2", "log10"))

###### calculo da entropia para variável Dia da Semana @@@@@@@@@@@@@@@@@@@@@@@

contSemana <- c(DiaDaSemana)
freqSemana <- table(contSemana)/length(contSemana)
-sum(freqSemana * log2(freqSemana))
## entropia = 2.804424
entropy.ChaoShen(freqSemana, unit=c("log", "log2", "log10"))

###### calculo da entropia para variável Dia da Semana @@@@@@@@@@@@@@@@@@@@@@@

contHour <- c(Hour)
freqHour <- table(contHour)/length(contHour)
-sum(freqHour * log2(freqHour))
## entropia = 4.38932
entropy.ChaoShen(freqHour, unit=c("log", "log2", "log10"))

###### calculo da entropia para variável Dia da Semana @@@@@@@@@@@@@@@@@@@@@@@
contPeriodo <- c(Periodo)
freqPeriodo <- table(contPeriodo)/length(contPeriodo)
-sum(freqPeriodo * log2(freqPeriodo))
## entropia = 1.807386
entropy.ChaoShen(freqPeriodo, unit=c("log", "log2", "log10"))

###### calculo da entropia para variável Dia da Semana @@@@@@@@@@@@@@@@@@@@@@@

contKM <- c(KMArredondado)
freqKM <- table(contKM)/length(contKM)
-sum(freqKM * log2(freqKM))
## entropia = 7.564134
entropy.ChaoShen(freqKM, unit=c("log", "log2", "log10"))

###### calculo da entropia para variável Dia da Semana @@@@@@@@@@@@@@@@@@@@@@@

contDelegacia <- c(Delegacia)
freqDelegacia <- table(contDelegacia)/length(contDelegacia)
-sum(freqDelegacia * log2(freqDelegacia))
## entropia = 1.800944
entropy.ChaoShen(freqDelegacia, unit=c("log", "log2", "log10"))

t=table(TipoAcident, Gravidade)
