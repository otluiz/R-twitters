setwd("~/workspace/R/src")

# free memory
rm(list = ls())
gc()

#-------[ TODO o dataset para ser separado em conjunto de treinamento e testes  ]------------------------
dfT <- read.csv("../data/prfCalculoTaxas2.csv") ## carrega o data frame

attach(dfT)
str(dfT)

### calculando Prob
ProbAcident = tx_RestVisibi * tx_CondPista * tx_BR * tx_TracadoVia
dfT["probAcident"] <- round(ProbAcident,3)


### agrupando hora em períodos
for (i in 1:nrow(dfT)){
  if (dfT[i,"Hour"] >= 0 & dfT[i,"Hour"] < 5 ) {dfT[i, "Periodo"] =  "Madrugada" } 
  if (dfT[i,"Hour"] >= 5 & dfT[i,"Hour"] < 12 ) {dfT[i, "Periodo"] =  "Manhã" } 
  if (dfT[i,"Hour"] >= 12 & dfT[i,"Hour"] < 18 ) {dfT[i, "Periodo"] =  "Tarde" }
  if (dfT[i,"Hour"] >= 18 & dfT[i,"Hour"] < 24 ) {dfT[i, "Periodo"] =  "Noite" }
}
 

## excluindo colunas
dfT$tx_BR <- NULL
dfT$tx_CausaAcident <- NULL
dfT$tx_CondPista <- NULL
dfT$tx_DiaSemana <- NULL
dfT$tx_Km <- NULL
dfT$tx_RestVisibi <- NULL
dfT$tx_TipoAcident <- NULL
dfT$tx_TracadoVia <- NULL
dfT$Hour <- NULL

write.csv(dfT,"../data/prfPeriodoTx.csv", row.names = FALSE)

### conjunto de treino, amostra com 30%
smp_size = floor(0.70 * nrow(dfT))

set.seed(123456)

treino_ind <- sample(seq_len(nrow(dfT)), size = smp_size)

treino <- dfT[treino_ind,]
testes <- dfT[-treino_ind,]

#write.csv(treino,"../data/treinoCalculoTx.csv", row.names = FALSE)
#write.csv(testes,"../data/testesCalculoTx.csv", row.names = FALSE)

library(party)
myFormula <- TipoAcident ~ CondPista + RestrVisibili + CausaAcident + TracadoVia + TipoAuto + Gravidade + DiaDaSemana + Periodo + KMArredondado + probAcident
prf_ctree <- ctree(myFormula, data = treino)
# check the prediction
table(predict(prf_ctree), treino$TipoAcident)


###################################################
### code chunk number 4: ch-decision-trees.rnw:42-43
###################################################
print(prf_ctree)


###################################################
### code chunk number 5: ch-decision-trees.rnw:49-50
###################################################
plot(prf_ctree)


###################################################
### code chunk number 6: ch-decision-trees.rnw:59-60
###################################################
plot(prf_ctree, type="simple")


###################################################
### code chunk number 7: ch-decision-trees.rnw:71-74
###################################################
# predict on test data
testPred <- predict(prf_ctree, newdata = teste)
table(testPred, teste$TipoAcident)





######################################################################################
############################                  ########################################
###########################  Separando por BRs  ######################################
############################                  ########################################
######################################################################################

### --------------[  conjunto de treino e teste para a BR 101 ]-----------------------
tdBr101 <- subset(dfT,BR=='101')
tdBr101$BR <- NULL
tdBr101$Delegacia <- NULL
### conjunto de treino, amostra com 30%
smp_size = floor(0.70 * nrow(tdBr101))

set.seed(123456)

treino_101 <- sample(seq_len(nrow(tdBr101)), size = smp_size) ### sepera um conjunto aleatório

treino <- tdBr101[treino_101,]  ### conjunto de treino 70%
testes <- tdBr101[-treino_101,] ### conjunto de testes o restante

write.csv(treino,"~/workspace/weka/BR101/treino101.csv", row.names = FALSE)
write.csv(testes,"~/workspace/weka/BR101/teste101.csv", row.names = FALSE)

#write.csv(tdBr101,"../data/subConjuntos/BR101.csv", row.names = FALSE)
###---------------[  fim da conjunto treino e testes BR 101  ]------------------------------------

tdBr104 <- subset(dfT,BR=='104')
write.csv(tdBr104,"../data/subConjuntos/BR104.csv", row.names = FALSE)
tdBr110 <- subset(dfT,BR=='110')
write.csv(tdBr110,"../data/subConjuntos/BR110.csv", row.names = FALSE)
tdBr116 <- subset(dfT,BR=='116')
write.csv(tdBr116,"../data/subConjuntos/BR106.csv", row.names = FALSE)
tdBr232 <- subset(dfT,BR=='232')
write.csv(tdBr232,"../data/subConjuntos/BR232.csv", row.names = FALSE)
tdBr316 <- subset(dfT,BR=='316')
write.csv(tdBr316,"../data/subConjuntos/BR316.csv", row.names = FALSE)

### --------------[  conjunto de treino e teste para a BR 407 ]-----------------------------------
tdBr407 <- subset(dfT,BR=='407')
tdBr407$BR <- NULL
tdBr407$Delegacia <- NULL

### conjunto de treino, amostra treino 70% teste 30%
smp_size = floor(0.70 * nrow(tdBr407))

set.seed(123456)

treino_407 <- sample(seq_len(nrow(tdBr407)), size = smp_size) ### sepera um conjunto aleatório

treino <- tdBr407[treino_407,]  ### conjunto de treino 70%
testes <- tdBr407[-treino_407,] ### conjunto de testes o restante

write.csv(treino,"~/workspace/weka/BR407/treino407.csv", row.names = FALSE)
write.csv(testes,"~/workspace/weka/BR407/teste407.csv", row.names = FALSE)

#write.csv(tdBr407,"../data/subConjuntos/BR407.csv", row.names = FALSE)
###---------------[  fim da conjunto treino e testes BR 407  ]------------------------------------

tdBr408 <- subset(dfT,BR=='408')
write.csv(tdBr408,"../data/subConjuntos/BR408.csv", row.names = FALSE)
tdBr423 <- subset(dfT,BR=='423')
write.csv(tdBr423,"../data/subConjuntos/BR423.csv", row.names = FALSE)
tdBr424 <- subset(dfT,BR=='424')
write.csv(tdBr424,"../data/subConjuntos/BR424.csv", row.names = FALSE)
tdBr428 <- subset(dfT,BR=='428')
write.csv(tdBr428,"../data/subConjuntos/BR428.csv", row.names = FALSE)
