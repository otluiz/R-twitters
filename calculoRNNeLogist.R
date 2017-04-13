#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

rm(dfrnn) ## remove antigos objetos
dfrnn <- read.csv("./data/CalculaGravidade/prfCalculoTaxas.csv") ## carrega o data frame/home/otluix/workspace/R/data/CalculaGravidade
#str(dfrnn)
#levels(dfrnn$RestrVisibili) 
#table(dfrnn$RestrVisibili)
### Atacchar a variável: dfrnn$TipoAuto => TipoAuto
attach(dfrnn)

### Verificar os dados
dfrnn.novo <- dfrnn[order(KMArredondado, Hour),]


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
  if(dfrnn[i, "DiaDaSemana"] == "Terça-feira")   { dfrnn[i, "DiaSemana"] = 2 }
  if(dfrnn[i, "DiaDaSemana"] == "Quarta-feira")  { dfrnn[i, "DiaSemana"] = 3 }
  if(dfrnn[i, "DiaDaSemana"] == "Quinta-feira")  { dfrnn[i, "DiaSemana"] = 4 }
  if(dfrnn[i, "DiaDaSemana"] == "Sexta-feira")   { dfrnn[i, "DiaSemana"] = 5 }
  if(dfrnn[i, "DiaDaSemana"] == "Sábado")        { dfrnn[i, "DiaSemana"] = 6 }
  if(dfrnn[i, "DiaDaSemana"] == "Domingo")       { dfrnn[i, "DiaSemana"] = 7 }
}

### agrupando hora em períodos
for (i in 1:nrow(dfrnn)){
  if (dfrnn[i,"Hour"] >= 0 & dfrnn[i,"Hour"] < 5 ) {dfrnn[i, "Periodo"] =  1 }   # Madrugada
  if (dfrnn[i,"Hour"] >= 5 & dfrnn[i,"Hour"] < 12 ) {dfrnn[i, "Periodo"] =  2 }  # Manhã
  if (dfrnn[i,"Hour"] >= 12 & dfrnn[i,"Hour"] < 18 ) {dfrnn[i, "Periodo"] =  3 } # Tarde
  if (dfrnn[i,"Hour"] >= 18 & dfrnn[i,"Hour"] < 24 ) {dfrnn[i, "Periodo"] =  4}  # Noite
}


### Gravar uma cópia para, os "for" são demorados /home/otluix/workspace/R/data/NãoTemGravidade
dfrnn$tx_DiaSemana <- NULL
dfrnn$DiaDaSemana <- NULL
write.csv(dfrnn,"./data/NãoTemGravidade/prfParaRNN.csv", row.names = FALSE)

#####################################################################################
### Ler a cópia gravada e prosseguir
dfrnn <- read.csv("./data/NãoTemGravidade/prfParaRNN.csv")
attach(dfrnn)

## Cria uma coluna TipoAutoNum vinda da coluna TipoAuto (facto) 
dfrnn <- data.frame(dfrnn, TipoAutoNum = as.numeric(dfrnn$TipoAuto))

## granvando dados atualizados
#dfrnn <- read.csv("./data/NãoTemGravidade/prfParaRNN.csv")

## calcualndo taxas para TipoAutoNum
qtdLinhas <- nrow(dfrnn)

names(dfrnn)
levels(dfrnn$TipoAutoNum)
table(dfrnn$TipoAuto)

for(i in 1:qtdLinhas){
  if (dfrnn[i,7] == "Automóvel") { dfrnn[i,"TipoAutoNum"] = round(12898/qtdLinhas, 3)  }
  if (dfrnn[i,7] == "Bicicleta") { dfrnn[i,"TipoAutoNum"] = round(255/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Caminhão") { dfrnn[i,"TipoAutoNum"] = round(6153/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Caminhão-Tanque") { dfrnn[i,"TipoAutoNum"] = round(22/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Caminhão-Trator") { dfrnn[i,"TipoAutoNum"] = round(5324/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Caminhonete") { dfrnn[i,"TipoAutoNum"] = round(4771/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Camioneta") { dfrnn[i,"TipoAutoNum"] = round(2551/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Carroça") { dfrnn[i,"TipoAutoNum"] = round(50/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Carro-de-mao") { dfrnn[i,"TipoAutoNum"] = round(1/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Charrete") { dfrnn[i,"TipoAutoNum"] = round(5/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Ciclomotor") { dfrnn[i,"TipoAutoNum"] = round(242/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Microônibus") { dfrnn[i,"TipoAutoNum"] = round(891/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Motocicletas") { dfrnn[i,"TipoAutoNum"] = round(9151/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Motoneta") { dfrnn[i,"TipoAutoNum"] = round(684/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Não identificado") { dfrnn[i,"TipoAutoNum"] = round(2113/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Não Informado") { dfrnn[i,"TipoAutoNum"] = round(2/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Não se Aplica") { dfrnn[i,"TipoAutoNum"] = round(1750/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Ônibus") { dfrnn[i,"TipoAutoNum"] = round(2922/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Quadriciclo") { dfrnn[i,"TipoAutoNum"] = round(2/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Reboque") { dfrnn[i,"TipoAutoNum"] = round(21/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Semi-Reboque") { dfrnn[i,"TipoAutoNum"] = round(56/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Trator de esteiras") { dfrnn[i,"TipoAutoNum"] = round(1/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Trator de rodas") { dfrnn[i,"TipoAutoNum"] = round(58/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Trator misto") { dfrnn[i,"TipoAutoNum"] = round(3/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Triciclo") { dfrnn[i,"TipoAutoNum"] = round(11/qtdLinhas, 3) }
  if (dfrnn[i,7] == "Utilitário") { dfrnn[i,"TipoAutoNum"] = round(418/qtdLinhas, 3) }
}

#dfrnn$TipoAuto <- NULL
#dfrnn <- data.frame(dfrnn, TipoAutoNum = as.numeric(dfrnn$TipoAuto)) apenas converte em número

write.csv(dfrnn,"./data/NãoTemGravidade/prfParaRNN.csv", row.names = FALSE)


###########################################################################################
######################### Equação para predição Logística #################################
###########################################################################################

for(i in 1:nrow(dfrnn)){
  fit <- glm(dfrnn$Gravidade ~ dfrnn$CondPista + dfrnn$TipoAcident + dfrnn$TracadoVia + dfrnn$TipoAuto, family = "binomial")
  #dfrnn[i, "fit"] <- fit
}






### Separar as BRs da rota pretendida (BR 101 X BR 232)
dfrn.BR101 <- subset(dfrn,BR=='101')
dfrn.BR232 <- subset(dfrn,BR=='232')
dfrn.BR104 <- subset(dfrn,BR=='104')
dfrn.BR116 <- subset(dfrn,BR=='116')



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
