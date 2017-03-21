#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

rm(df) ## remove antigos objetos
df <- read.csv("./data/prfCalculoTaxas.csv") ## carrega o data frame
#str(df)
#levels(df$RestrVisibili) 
#table(df$RestrVisibili)
### Atacchar a variável: df$TipoAuto => TipoAuto
attach(df)

### Verificar os dados
df.novo <- df[order(KMArredondado, Hour),]

#### remover colunas desnecessárias
df$TipoAuto <- NULL
df$Delegacia <- NULL
df$CondPista <- NULL
df$TracadoVia <- NULL
df$TipoAcident <- NULL
df$CausaAcident <- NULL
df$tx_DiaSemana <- NULL
df$RestrVisibili <- NULL


### Arredondar variáveis para 3 casas decimais
for(i in 1:nrow(df)){
  df$tx_RestVisibi   <- round(df$tx_RestVisibi,3)
  df$tx_CondPista    <- round(df$tx_CondPista,3)
  df$tx_TipoAcident  <- round(df$tx_TipoAcident,3)
  df$tx_CausaAcident <- round(df$tx_CausaAcident,3)
  df$tx_TracadoVia   <- round(df$tx_TracadoVia,3)
}

str(Hour)
### Gravar uma cópia para, os "for" são demorados
write.csv(df,"./data/prfParaRNN.csv", row.names = FALSE)
### Ler a cópia gravada e prosseguir
df <- read.csv("./data/prfParaRNN.csv")
attach(df)

### Corverter dados categóricos para numéricos
### ajustando a variável Gravidade
for(i in 1:nrow(df)){
  if(df[i,"Gravidade"] == "FALSE") { df[i,"Gravidade"] = 0 }
  if(df[i,"Gravidade"] == "TRUE" ) { df[i,"Gravidade"] = 1 }
}

### Gravar uma cópia para, os "for" são demorados
write.csv(df,"./data/prfParaRNN.csv", row.names = FALSE)
### Ler a cópia gravada e prosseguir
df <- read.csv("./data/prfParaRNN.csv")
attach(df)

### ajustando a variável Dia da semana, tem que criar uma nova coluna
for(i in 1:nrow(df)){
  if(df[i, "DiaDaSemana"] == "Segunda-feira") { df[i, "DiaSemana"] = 1 }
  if(df[i, "DiaDaSemana"] == "Terça-feira") { df[i, "DiaSemana"] = 2 }
  if(df[i, "DiaDaSemana"] == "Quarta-feira") { df[i, "DiaSemana"] = 3 }
  if(df[i, "DiaDaSemana"] == "Quinta-feira") { df[i, "DiaSemana"] = 4 }
  if(df[i, "DiaDaSemana"] == "Sexta-feira") { df[i, "DiaSemana"] = 5 }
  if(df[i, "DiaDaSemana"] == "Sábado") { df[i, "DiaSemana"] = 6 }
  if(df[i, "DiaDaSemana"] == "Domingo") { df[i, "DiaSemana"] = 7 }
}

df$DiaDaSemana <- NULL
### Gravar uma cópia para, os "for" são demorados
write.csv(df,"./data/prfParaRNN.csv", row.names = FALSE)
### Ler a cópia gravada e prosseguir
df <- read.csv("./data/prfParaRNN.csv")
attach(df)

### agrupando hora em períodos
for (i in 1:nrow(df)){
  if (df[i,"Hour"] >= 0 & df[i,"Hour"] < 5 ) {df[i, "Periodo"] =  1 }   # Madrugada
  if (df[i,"Hour"] >= 5 & df[i,"Hour"] < 12 ) {df[i, "Periodo"] =  2 }  # Manhã
  if (df[i,"Hour"] >= 12 & df[i,"Hour"] < 18 ) {df[i, "Periodo"] =  3 } # Tarde
  if (df[i,"Hour"] >= 18 & df[i,"Hour"] < 24 ) {df[i, "Periodo"] =  4}  # Noite
}

### Separar as BRs da rota pretendida (BR 101 X BR 232)
df.BR101 <- subset(df,BR=='101')
df.BR232 <- subset(df,BR=='232')


### incluindo o fator de precisão em cada data.frame
df.BR101['Precisão'] <- 0.812
df.BR232['Precisão'] <- 0.767

### Normalizar os dados: Dia da Semana
df.BR101$DiaSemana <- (df.BR101$DiaSemana - min(df.BR101$DiaSemana))/(max(df.BR101$DiaSemana) - min(df.BR101$DiaSemana))
df.BR101["DiaSemana"] <- round(df.BR101$DiaSemana,3)

df.BR232$DiaSemana <- (df.BR232$DiaSemana - min(df.BR232$DiaSemana))/(max(df.BR232$DiaSemana) - min(df.BR232$DiaSemana))
df.BR232["DiaSemana"] <- round(df.BR232$DiaSemana,3)

### calculando Prob. ser  Gravidade
tx_Gravid101 = df.BR101$tx_RestVisibi * df.BR101$tx_CondPista * df.BR101$tx_TracadoVia * df.BR101$DiaSemana * df.BR101$Precisão + df.BR101$Gravidade
df.BR101["tx_Gravidade"] <- round(tx_Gravid101,3)

tx_Gravid232 = df.BR232$tx_RestVisibi * df.BR232$tx_CondPista * df.BR232$tx_TracadoVia * df.BR232$DiaSemana * df.BR232$Precisão + df.BR232$Gravidade
df.BR232["tx_Gravidade"] <- round(tx_Gravid232,3)
### Ordenar as colunas

### exportando dados CSV e ARFF (para Weka)
write.csv(df.BR101,"./data/BR101/RNN.csv", row.names = FALSE)
write.csv(df.BR232,"./data/BR232/RNN.csv", row.names = FALSE)

library(RWeka)
write.arff(df.BR101,"../weka/BR101/RNN.arff", eol = "\n")
write.arff(df.BR232,"../weka/BR232/RNN.arff", eol = "\n")


######## Outras BRs #########
BR104 <- subset(df,BR=='104')
BR110 <- subset(df,BR=='110')
BR116 <- subset(df,BR=='116')
BR316 <- subset(df,BR=='316')
BR407 <- subset(df,BR=='407')
BR408 <- subset(df,BR=='408')
BR423 <- subset(df,BR=='423')
BR424 <- subset(df,BR=='424')
BR428 <- subset(df,BR=='428')
