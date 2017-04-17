#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

## AUla 11: https://www.youtube.com/watch?v=lTMqXSSjCvk
## Fonte: neurolnet: Treinamento rede neural

install.packages("neuralnet")
require(neuralnet)
?neuralnet ## consultar sobre a rede neural

dfrnn.BR101 <- read.csv("./data/BR101/RNN.csv") ## carrega o data frame
dim(dfrnn.BR101)
hist(dfrnn.BR101$tx_Gravidade)
hist(dfrnn.BR101$Gravidade)
attach(dfrnn.BR101) ## amarra as variáveis do data frame


### incluindo o fator de precisão em cada data.frame Vindo da Árvore de Decisão
#erroBR101 <- 0.812
#erroBR232 <- 0.787
#erroBR104 <- 0.957
#erroBR116 <- 0.669

nn101 = neuralnet(formula = Gravidade ~ tx_RestVisibi + tx_CondPista + tx_TracadoVia + tx_CausaAcident + tx_TipoAcident,
                  data = dfrnn.BR101, hidden = 3,
                  err.fct = "sse", linear.output = FALSE) ## sse porque o erro não é automaticamente quadratico

#nn101 = neuralnet(formula = tx_Gravidade ~ tx_RestVisibi + tx_CondPista + tx_TracadoVia + Gravidade,
##              data = dfrnn.BR101, hidden = 3,
#               err.fct = "sse", linear.output = FALSE) ## sse porque o erro não é automaticamente quadratico

#nn = neuralnet(formula = case ~ age+parity+induced+spontaneous, 
#               data = infert, hidden = 2,
#               err.fct = "ce", linear.output = FALSE)
#
# tx_Gravid101 = (dfrnn.BR101$tx_RestVisibi + dfrnn.BR101$tx_CondPista + dfrnn.BR101$tx_TracadoVia) * erroBR101 + dfrnn.BR101$Gravidade
#
## linear.output should be stated as FALSE to ensure
## that the output of the activation Function is mapped
## to the internal [0,1]
## You shall set  ir to false for categorical outputs

nn101
plot(nn101)
nn101$net.result ## overall result i.e. output for each repication
nn101$weights
nn101$result.matrix
nn101$net.result[[1]]
nn101_1 = ifelse(nn101$net.result[[1]]>0.5,1,0)
nn101_1
misClassificationError = mean(tx_Gravidade != nn101_1)
misClassificationError
OutPutVsPerd = cbind(tx_Gravidade, nn101_1)
OutPutVsPerd










#write.csv(dfrnn.BR232,"./data/BR232/RNN.csv", row.names = FALSE)




