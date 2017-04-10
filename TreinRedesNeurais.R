## AUla 11: https://www.youtube.com/watch?v=lTMqXSSjCvk
## Fonte: neurolnet: Treinamento rede neural

install.packages("neuralnet")
require(neuralnet)
?infert
dim(infert)
?neuralnet
infert

nn = neuralnet(formula = case ~ age+parity+induced+spontaneous, 
               data = infert, hidden = 2,
               err.fct = "ce", linear.output = FALSE)
## linear.output should be stated as FALSE to ensure
## that the output of the activation Function is mapped
## to the internal [0,1]
## You shall set  ir to false for categorical outputs

nn
plot(nn)
nn$net.result ## overall result i.e. output for each repication
nn$weights
nn$net.result[[1]]
nn1 = ifelse(nn$net.result[[1]]>0.5,1,0)
nn1
misClassificationError = mean(infert$case != nn1)
misClassificationError
OutPutVsPerd = cbind(infert$case, nn1)
OutPutVsPerd

### Usando BackPropagation Algorithm an Playing
### With Learningrate and entropy

nn.bp = neuralnet(
  formula = case ~ age+parity+induced+spontaneous,
  data = infert, hidden = 2, learningrate = 0.01,
  algorithm = "backprop", err.fct = "ce",
  linear.output = FALSE)

nn.bp
plot(nn.bp)
nn ## nn did a bettr job

#################################
#### usando nn para predição ####
#################################
new.output = compute(nn,covariate = matrix(c(22,1,0,0,
                                             22,1,1,0,
                                             22,1,0,1,
                                             22,1,1,1),
                                           byrow = TRUE, ncol = 4))

new.output$net.result

#################################
#### Intervalo de Confiança #####
#################################
ci = confidence.interval(nn, alpha = 0.05)
ci

#################################
### Viisualizar os resultados ###
#################################
par(mfrow = c(2,2))
gwplot(nn, selected.covariate = "parity",
       min = 2.5, max = 5)
gwplot(nn, selected.covariate = "induced",
       min = 2.5, max = 5)
gwplot(nn, selected.covariate = "spontaneous",
       min = 2.5, max = 5)