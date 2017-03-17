setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

dfR <- read.csv(file = "./data/BR101/BR101.csv", header=TRUE, sep=",")
dfS <- read.csv(file = "./data/BR232/BR232.csv", header=TRUE, sep=",")
dfT <- read.csv(file = "./data/BR407/BR407.csv", header=TRUE, sep=",")

attach(dfR)
attach(dfS)
attach(dfT)
str(dfR)
### gerando vários tipos de gráficos
#############################################
png("./graficos/br1011_%d.png")
plot(dfR,dfR$KMArredondado , col = ifelse(dfR$KMArredondado > 100, "blue","red"))
#boxplot(br101.ord1$KM~br101.ord1.cate, col = ifelse(br101.ord1$KM < 60 && br101.ord1$KM > 110,"red", "blue"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(dfR$Hour, xlab = "Hora do acidente", ylab = "Frequência",col = ifelse(Hour > 5 && Hour <20, "red","blue") , main = "Acidentes/hora BR-101")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()

#############################################
png("./graficos/br232_%d.png")
hist(dfS$Hour, xlab = "Hora do acidente", ylab = "Frequência",col = ifelse(Hour > 5 && Hour <20, "red","blue") , main = "Acidentes/hora BR-232 ")
dev.off()

#############################################
png("./graficos/br407_%d.png")
hist(dfT$Hour, xlab = "Hora do acidente", ylab = "Frequência",col = ifelse(Hour > 5 && Hour <20, "red","blue") , main = "Acidentes/hora BR-407")
dev.off()
