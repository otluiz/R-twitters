###################################################
### code chunk number 1: ch-text-mining.rnw:6-9
###################################################
# free memory
rm(list = ls())
gc()


##################################################
### setando diretorio de trabalho
##################################################
setwd("/home/otluiz/workspace/R")
getwd()
source("/src/prfLimpoCheiroso.R")
###########################################
### diretorio com os dados semi-tratados
############################################
### ("/home/otluiz/Mestrado/BancodeDados/DadosPRF/DadosSemiTratados/prfTratoLimpo.csv")

###################################################
### lendo o arquivo de dados csv
###################################################
dfn <- read.csv("./data/prfTratoLimpo2.csv")
dfw <- read.csv("./data/LimpoCheiroso.csv")

###################################################
### criando nomes para as colunas
###################################################
names(df) <- c("BR","KM","CondPista","RestrVisibili",
               "TipoAcident","CausaAcident","TracadoVia",
               "Municipio","TipoAuto","DataAcident",
               "HoraAcident","Binario","TotFerVivos",
               "TotMortos","Gravidade","FonteDados",
               "DiaDaSemana","id")


##################################################
### Eliminando algumas colunas "desnecessárias"
### para alguma abordagens
##################################################
dfn$KM <- NULL ## remove a coluna Km, foi ajustada 
#dfw$X <- NULL  ## remove a coluna X, desnecessária

write.csv(dfn, "./data/LimpoCheiroso.csv", row.names = FALSE)

head(dfn)
levels(dfw$Municipio) ##mostrando os dados da coluna



##################################################
### Correlação Linear de algumas variáveis
##################################################
cor(df2$TotFerVivos,df2$TotMortos)
cor(df2$BR,df2$KM)
cor(dfw)


###################################################
### definindo conjunto de treinamento e de testes
###################################################
str(df)
set.seed(123456)
ind <- sample(2, nrow(df), replace=TRUE, prob=c(0.7, 0.3))
trainData <- dummyData[ind==1,]
testData <- dummyData[ind==2,]


###################################################
### gerando: decision-trees
###################################################
library(party)
minhaFormula <- CausaAcident ~ TracadoVia + TipoAuto

df_ctree <- ctree(minhaFormula, data=trainData)
# check the prediction
table(predict(df_ctree), trainData$CausaAcident)


###################################################
### imprimindo decision-trees para minhaFormula
###################################################
print(df_ctree)


###################################################
### plotano a decision-trees para minhaFormula
###################################################
plot(df_ctree)






#####################################################
### cria e grava uma copia de segurança 
#####################################################
#dummyData <- df
#write.csv(df, "./data/dummmyData.csv", row.names = FALSE)
#write.csv(dfw, "./data/LimpoCheiroso.csv", row.names = FALSE)