# free memory
rm(list = ls())
gc()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(readr)
dfN <- read.csv("~/workspace/R/data/prfNumerico.csv")

## calcula e converte para booleano o atributo Gravidade------------------------------------------------
for(i in 1:nrow(dfN)){
  if(dfN[i,"Gravidade"] == "FALSE") { dfN[i,"Gravidade"] = 0 }
  if(dfN[i,"Gravidade"] == "TRUE" ) { dfN[i,"Gravidade"] = 1 }
}

hora = rep(00:00,23:59,5)
#plot(cpi, xaxt="n", ylab="CPI", xlab="")
plot(prfCalculoTaxas$Hour, xaxt="n", ylab = "Km", xlab = "")

#axis(1, labels=paste(year,quarter,sep="Q"), at=1:12, las=3)
axis(1, labels = paste(prfCalculoTaxas$KMArredondado[BR=232],sep = "h"),at=1:1, las=3)


###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculo de correlação linear entre as variáveis
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cor(dfN$CondPista,dfN$Hour) ############ -0.003940837
cor(dfN$CondPista,dfN$RestrVisibili) ###  0.07928121 +
cor(dfN$CondPista,dfN$TipoAcident) #####  0.01732757
cor(dfN$CondPista,dfN$CausaAcident) ####  0.02809744
cor(dfN$CondPista,dfN$TracadoVia) ######  0.02850112
cor(dfN$CondPista,dfN$TipoAuto) ######## -0.001269703   CONDIÇÃO DA PISTA PARECE NÃO SER 
cor(dfN$CondPista,dfN$Gravidade) ####### -0.0009133167  UMA BOA VARIÁVEL
cor(dfN$CondPista,dfN$DiaDaSemana) ##### -0.005308149
cor(dfN$CondPista,dfN$KMArredondado) ### -0.01388837
cor(dfN$CondPista,dfN$Delegacia) ####### -0.02834986
cor(dfN$CondPista,dfN$BR) ############## -0.0272271

#cor(dfN$BR,dfN$CondPista)
cor(dfN$BR,dfN$RestrVisibili) ###########  0.05271526 +
cor(dfN$BR,dfN$TipoAcident) #############  0.1269645  ++                 ++++
cor(dfN$BR,dfN$CausaAcident) ############  0.05271526 +                  ++++
cor(dfN$BR,dfN$TracadoVia) ##############  0.0127905                ++++++++++++++
cor(dfN$BR,dfN$TipoAuto) ################ -0.0008496433             ++++++++++++++
cor(dfN$BR,dfN$Gravidade) ############### -0.002588708                   ++++   
cor(dfN$BR,dfN$DiaDaSemana) #############  0.06086991 +                  ++++  
cor(dfN$BR,dfN$Hour) ####################  0.04253913
cor(dfN$BR,dfN$KMArredondado) ###########  0.2225249  ++
cor(dfN$BR,dfN$Delegacia) ###############  0.6538042  +++


#cor(dfN$RestrVisibili,dfN$BR) ##########  0.05271526 +
cor(dfN$RestrVisibili,dfN$Hour) #########  0.04771512 +
cor(dfN$RestrVisibili,dfN$TipoAcident) ##  0.03385247 +
cor(dfN$RestrVisibili,dfN$CausaAcident)## -0.01537365
cor(dfN$RestrVisibili,dfN$TracadoVia)  ##  0.06855744 +
cor(dfN$RestrVisibili,dfN$TipoAuto) ##### -0.00117956
cor(dfN$RestrVisibili,dfN$Gravidade) ####  0.003104227
cor(dfN$RestrVisibili,dfN$DiaDaSemana) ##  0.003679712
cor(dfN$RestrVisibili,dfN$KMArredondado)#  0.037151   +
cor(dfN$RestrVisibili,dfN$Delegacia) ####  0.05001657 +


cor(dfN$TipoAcident,dfN$BR) #############  0.1269645  ++
cor(dfN$TipoAcident,dfN$CausaAcident) ###  0.06737295 +
cor(dfN$TipoAcident,dfN$TracadoVia) #####  0.1438253  ++
cor(dfN$TipoAcident,dfN$TipoAuto) ####### -0.007231386 
cor(dfN$TipoAcident,dfN$Gravidade) ###### -0.003229957
cor(dfN$TipoAcident,dfN$DiaDaSemana) ####  0.08449525 +
cor(dfN$TipoAcident,dfN$Hour) ########### -0.009522101 
cor(dfN$TipoAcident,dfN$KMArredondado) ##  0.09203331 +
cor(dfN$TipoAcident,dfN$Delegacia) ######  0.0889743  +
#cor(dfN$TipoAcident,dfN$tx_CondPista) <<<<<<<<<<<

cor(dfN$CausaAcident,dfN$BR) ############  0.05545753  +
cor(dfN$CausaAcident,dfN$CondPista) #####  0.02809744  
cor(dfN$CausaAcident,dfN$TracadoVia) ####  0.0305455
cor(dfN$CausaAcident,dfN$TipoAuto) ######  0.002757052
cor(dfN$CausaAcident,dfN$Gravidade) #####  0.008282911
cor(dfN$CausaAcident,dfN$DiaDaSemana) ###  0.05629724  +
cor(dfN$CausaAcident,dfN$Hour) ########## -0.01631607
cor(dfN$CausaAcident,dfN$KMArredondado) #  0.02922716
cor(dfN$CausaAcident,dfN$Delegacia) #####  0.06882963  +

#cor(dfN$TracadoVia,dfN$CondPista)
#cor(dfN$TracadoVia,dfN$TipoAcident)
#cor(dfN$TracadoVia,dfN$CausaAcident)
#cor(dfN$TracadoVia,dfN$BR) #############  0.0127905
cor(dfN$TracadoVia,dfN$RestrVisibili) ###  0.06855744  +
cor(dfN$TracadoVia,dfN$TipoAuto) ########  0.003603084
cor(dfN$TracadoVia,dfN$Gravidade) ####### -0.0231587
cor(dfN$TracadoVia,dfN$DiaDaSemana) #####  0.01443326
cor(dfN$TracadoVia,dfN$Hour) ############ -0.02849474
cor(dfN$TracadoVia,dfN$KMArredondado) ### -0.005860565
cor(dfN$TracadoVia,dfN$Delegacia) ####### -0.01192291


#cor(dfN$TipoAuto,dfN$CondPista)
#cor(dfN$TipoAuto,dfN$RestrVisibili)    TIPO DE AUTOMÓVEL NÃO INFLUENCIA NA CAUSA
#cor(dfN$TipoAuto,dfN$TipoAcident)       DOS ACIDENTES E PARALIZAÇÕES
#cor(dfN$TipoAuto,dfN$CausaAcident)
#cor(dfN$TipoAuto,dfN$TracadoVia)
cor(dfN$TipoAuto,dfN$BR) ################ -0.0008496433
cor(dfN$TipoAuto,dfN$Gravidade) #########  0.00127836
cor(dfN$TipoAuto,dfN$DiaDaSemana) ####### -0.0005795248
cor(dfN$TipoAuto,dfN$Hour) ############## -0.003008757
cor(dfN$TipoAuto,dfN$KMArredondado) ##### -0.001663227
cor(dfN$TipoAuto,dfN$Delegacia) #########  0.007862909

cor(dfN$Gravidade,dfN$BR) ############### -0.002588708
#cor(dfN$Gravidade,dfN$CondPista )
#cor(dfN$Gravidade,dfN$RestrVisibili)
#cor(dfN$Gravidade,dfN$TipoAcident)      GRAVIDADE DO ACIDENTE TAMBÉM NÃO É UM BOA VARIÁVEL
#cor(dfN$Gravidade,dfN$CausaAcident)
#cor(dfN$Gravidade,dfN$TracadoVia)
#cor(dfN$Gravidade,dfN$TipoAuto)
cor(dfN$Gravidade,dfN$DiaDaSemana) ###### -0.004455545
cor(dfN$Gravidade,dfN$Hour) #############  0.003777386
cor(dfN$Gravidade,dfN$KMArredondado) ####  0.07143123  +
cor(dfN$Gravidade,dfN$Delegacia) ########  0.07143123  +

cor(dfN$DiaDaSemana,dfN$BR) #############  0.06086991  +
#cor(dfN$DiaDaSemana,dfN$CondPista)
#cor(dfN$DiaDaSemana,dfN$RestrVisibili)
#cor(dfN$DiaDaSemana,dfN$TipoAcident)
#cor(dfN$DiaDaSemana,dfN$CausaAcident)    DIA DA SEMANA PARECE NÃO SER BOA VARIÁVEL
#cor(dfN$DiaDaSemana,dfN$TracadoVia)
#cor(dfN$DiaDaSemana,dfN$TipoAuto)
#cor(dfN$DiaDaSemana,dfN$Gravidade)
cor(dfN$DiaDaSemana,dfN$Hour) ###########  0.06307059  +
cor(dfN$DiaDaSemana,dfN$KMArredondado) ##  0.02721923
cor(dfN$DiaDaSemana,dfN$Delegacia) ######  0.04630643

#cor(dfN$Hour,dfN$BR)
#cor(dfN$Hour,dfN$CondPista)
#cor(dfN$Hour,dfN$RestrVisibili)
#cor(dfN$Hour,dfN$TipoAcident)
#cor(dfN$Hour,dfN$CausaAcident)
#cor(dfN$Hour,dfN$TracadoVia)
#cor(dfN$Hour,dfN$TipoAuto)
#cor(dfN$Hour,dfN$Gravidade)
#cor(dfN$Hour,dfN$DiaDaSemana)
cor(dfN$Hour,dfN$KMArredondado) #########  0.03216273
cor(dfN$Hour,dfN$Delegacia) #############  0.05487038

#cor(dfN$KMArredondado,dfN$BR) ##########  0.2225249
#cor(dfN$KMArredondado,dfN$CondPista) ### -0.01388837
#cor(dfN$KMArredondado,dfN$RestrVisibili)  0.037151
#cor(dfN$KMArredondado,dfN$TipoAcident) #  0.02922716
#cor(dfN$KMArredondado,dfN$CausaAcident)#  0.02922716
#cor(dfN$KMArredondado,dfN$TracadoVia) ## -0.005860565
#cor(dfN$KMArredondado,dfN$TipoAuto) #### -0.001663227
#cor(dfN$KMArredondado,dfN$Gravidade)
#cor(dfN$KMArredondado,dfN$DiaDaSemana)    KMARREDONDADO FOI UMA DAS MELHORES VARIÁVEIS
#cor(dfN$KMArredondado,dfN$Hour)
cor(dfN$KMArredondado,dfN$Delegacia) ####  0.494049   +++

#cor(dfN$Delegacia,dfN$BR)
#cor(dfN$Delegacia,dfN$CondPista)
#cor(dfN$Delegacia,dfN$RestrVisibili)
#cor(dfN$Delegacia,dfN$TipoAcident)
#cor(dfN$Delegacia,dfN$CausaAcident)
#cor(dfN$Delegacia,dfN$TracadoVia)
#cor(dfN$Delegacia,dfN$TipoAuto)
#cor(dfN$Delegacia,dfN$Gravidade)
#cor(dfN$Delegacia,dfN$DiaDaSemana)
#cor(dfN$Delegacia,dfN$Hour)
#cor(dfN$Delegacia,dfN$KMArredondado)

### arredondando para 3 casas decimais
for(i in 1:nrow(dfN)){
  dfN$tx_RestVisibi   <- round(dfN$tx_RestVisibi,3)
  dfN$tx_CondPista    <- round(dfN$tx_CondPista,3)
  dfN$tx_TipoAcident  <- round(dfN$tx_TipoAcident,3)
  dfN$tx_DiaSemana    <- round(dfN$tx_DiaSemana,3)
  dfN$tx_CausaAcident <- round(dfN$tx_CausaAcident,3)
  dfN$tx_TracadoVia   <- round(dfN$tx_TracadoVia,3)
}

### gravando com atributo Gravidade alterado de TRUE -> 1, FALSE -> 0
#write.csv(dfN,"~/workspace/R/data/prfNumerico10.csv", row.names = FALSE)

###################################################
### regressão linear
###################################################
fit <- lm(dfN$KMArredondado ~ dfN$Delegacia + dfN$TipoAcident + dfN$TracadoVia + dfN$BR)
fit

###################################################
### code chunk number 5: ch-regression.rnw:65-67
###################################################
#(cpi2011 <- fit$coefficients[[1]] + fit$coefficients[[2]]*2011 +
#   fit$coefficients[[3]]*(1:4))

reg <- fit$coefficients[[1]] + fit$coefficients[[2]] + fit$coefficients[[3]]

attributes(fit)
fit$coefficients

residuals(fit)
summary(fit)

plot(fit)

