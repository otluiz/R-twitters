setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

# dados$x <- NULL ## remove a coluna x
dfM <- read.csv("./data/prfAcidentesATratar.csv") ## dados originais, sem tratamento da inconsistência

##  separnado por BR
df.Br104 <- subset(dfM,BR=='104')

## encontra um determinado intervalo baseado em um quilometro 
## para saber a que Municipio pertence
library(plyr)
df.Br104.Mun <- subset(df.Br104, df.Br104$KM >= 85 & df.Br104$KM < 95)
