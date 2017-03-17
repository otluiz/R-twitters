
#############################################
### arredonda algo como: 04:47 -> 04:45
### ou 04:56 -> 05:00
#############################################
myRound <- function (x, convert = TRUE)  {
  x <- as.POSIXlt(x)
  mins <- x$min
  mult <- mins %/% 15
  remain <- mins %% 15
  if(remain > 7L || (remain == 7L && x$sec > 29))
    mult <- mult + 1
  if(mult > 3) {
    x$min <- 0
    x <- x + 3600
  } else {
    x$min <- 15 * mult
  }
  x <- trunc.POSIXt(x, units = "mins")
  if(convert) {
    x <- format(x, format = "%H:%M")
  }
  x
}

#############################################
### limpa memória e seta diretório
#############################################

setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

#############################################
### carrega os dados
#############################################
dfOrigin <- read.csv("./data/prfTratadData.csv") ## dataframe com dados originais tratados

dfOrigin$QtdVeculos <- NULL ## remove a coluna
dfOrigin$TotFerVivos <- NULL ## remove a coluna
dfOrigin$TotMortos <- NULL ## remove a coluna
dfOrigin$TipoParalisacao <- NULL ## remove a coluna

attach(dfOrigin)
str(dfOrigin)



#############################################
### arredonda a hora
#############################################
dfOrigin$HoraAcident <- as.POSIXct(HoraAcident, format = "%H:%M", tz = "GMT")
dfOrigin$HoraAcident <- myRound(HoraAcident)


### agrupando hora em períodos
for (i in 1:nrow(dfOrigin)){
  if (dfOrigin[i,"HoraAcident"] >= 0 & dfOrigin[i,"HoraAcident"] < 5 ) {dfOrigin[i, "Periodo"] =  "Madrugada" } 
  if (dfOrigin[i,"HoraAcident"] >= 5 & dfOrigin[i,"HoraAcident"] < 12 ) {dfOrigin[i, "Periodo"] =  "Manhã" } 
  if (dfOrigin[i,"HoraAcident"] >= 12 & dfOrigin[i,"HoraAcident"] < 18 ) {dfOrigin[i, "Periodo"] =  "Tarde" }
  if (dfOrigin[i,"HoraAcident"] >= 18 & dfOrigin[i,"HoraAcident"] < 24 ) {dfOrigin[i, "Periodo"] =  "Noite" }
}


##  Adicionar BR na frente do número ex 101 => BR101   --------------------------------
for(i in 1:nrow(dfOrigin)){
  if(dfOrigin[i,"BR"] == 101) { dfOrigin[i,"BRajustada"] = "BR101" }
  if(dfOrigin[i,"BR"] == 104) { dfOrigin[i,"BRajustada"] = "BR104" }
  if(dfOrigin[i,"BR"] == 110) { dfOrigin[i,"BRajustada"] = "BR110" }
  if(dfOrigin[i,"BR"] == 116) { dfOrigin[i,"BRajustada"] = "BR116" }
  if(dfOrigin[i,"BR"] == 232) { dfOrigin[i,"BRajustada"] = "BR232" }
  if(dfOrigin[i,"BR"] == 316) { dfOrigin[i,"BRajustada"] = "BR316" }
  if(dfOrigin[i,"BR"] == 407) { dfOrigin[i,"BRajustada"] = "BR407" }
  if(dfOrigin[i,"BR"] == 408) { dfOrigin[i,"BRajustada"] = "BR408" }
  if(dfOrigin[i,"BR"] == 423) { dfOrigin[i,"BRajustada"] = "BR423" }
  if(dfOrigin[i,"BR"] == 424) { dfOrigin[i,"BRajustada"] = "BR424" }
  if(dfOrigin[i,"BR"] == 428) { dfOrigin[i,"BRajustada"] = "BR428" }
}

#############################################
### ordena pela data
#############################################
dfOrigin <- dfOrigin[order(c(as.Date(DataAcident))),]

#dfOrigin$Periodo <- c(as.factor(Periodo))
###############################################################################
###### Separando por BR: o dataset tem data Original ordenada 
###############################################################################
tdBr101 <- subset(dfOrigin,BR=='101')
tdBr104 <- subset(dfOrigin,BR=='104')
tdBr110 <- subset(dfOrigin,BR=='110')
tdBr116 <- subset(dfOrigin,BR=='116')
tdBr232 <- subset(dfOrigin,BR=='232')
tdBr316 <- subset(dfOrigin,BR=='316')
tdBr407 <- subset(dfOrigin,BR=='407')
tdBr408 <- subset(dfOrigin,BR=='408')
tdBr423 <- subset(dfOrigin,BR=='423')
tdBr424 <- subset(dfOrigin,BR=='424')
tdBr428 <- subset(dfOrigin,BR=='428')

#############################################
### gerando vários tipos de gráficos
#############################################
png("./graficos2/br101_%d.png")  ## 1
plot(tdBr101$DataAcident, tdBr101$KM, type = "l", xlab = "Data do Acidente", ylab = "BR 101 / Km", main = "Data do acidente em cada Km da BR 101")
#boxplot(tdBr101$DataAcident~tdBr101$KM, col = ifelse(tdBr101$KM < 60 && tdBr101$KM > 110,"red", "blue"), xlab="Data", ylab="Local do acidente(Km)")
hist(c(tdBr101$DataAcident), xlab = "Quilometro (km)", ylab = "Frequência",col = ifelse(tdBr101$KM <= 60, "blue","red") , main = "Frequência acidentes BR-101/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()


png("./graficos2/br104_%d.png")  ## 2
plot(tdBr104$DataAcident, tdBr104$KM, type = "l", xlab = "Data do Acidente", ylab = "BR 104 / Km", main = "Data do acidente em cada Km da BR 101")
#boxplot(tdBr101$DataAcident~tdBr101$KM, col = ifelse(tdBr101$KM < 60 && tdBr101$KM > 110,"red", "blue"), xlab="Data", ylab="Local do acidente(Km)")
hist(c(tdBr104$DataAcident), xlab = "Quilometro (km)", ylab = "Frequência", main = "Frequência acidentes BR-104/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()


png("./graficos2/br110_%d.png")  ## 3
plot(tdBr110$DataAcident, tdBr110$KM, type = "l", xlab = "Data do Acidente", ylab = "BR 110 / Km", main = "Data do acidente em cada Km da BR 110")
#boxplot(tdBr101$DataAcident~tdBr101$KM, col = ifelse(tdBr101$KM < 60 && tdBr101$KM > 110,"red", "blue"), xlab="Data", ylab="Local do acidente(Km)")
hist(c(tdBr110$DataAcident), xlab = "Quilometro (km)", ylab = "Frequência", main = "Frequência acidentes BR-110/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()


png("./graficos2/br116_%d.png")  ## 4
plot(tdBr116$DataAcident, tdBr116$KM, type = "l", xlab = "Data do Acidente", ylab = "BR 116 / Km", main = "Data do acidente em cada Km da BR 116")
#boxplot(tdBr101$DataAcident~tdBr101$KM, col = ifelse(tdBr101$KM < 60 && tdBr101$KM > 110,"red", "blue"), xlab="Data", ylab="Local do acidente(Km)")
hist(c(tdBr116$DataAcident), xlab = "Quilometro (km)", ylab = "Frequência", main = "Frequência acidentes BR-116/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()


png("./graficos2/br232_%d.png")  ## 5
plot(tdBr232$DataAcident, tdBr232$KM, type = "l", xlab = "Data do Acidente", ylab = "BR 232 / Km", main = "Data do acidente em cada Km da BR 232")
#boxplot(tdBr101$DataAcident~tdBr101$KM, col = ifelse(tdBr101$KM < 60 && tdBr101$KM > 110,"red", "blue"), xlab="Data", ylab="Local do acidente(Km)")
hist(c(tdBr232$DataAcident), xlab = "Quilometro (km)", ylab = "Frequência", main = "Frequência acidentes BR-232/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()


png("./graficos2/br316_%d.png")  ## 6
plot(tdBr316$DataAcident, tdBr316$KM, type = "l", xlab = "Data do Acidente", ylab = "BR 316 / Km", main = "Data do acidente em cada Km da BR 316")
#boxplot(tdBr101$DataAcident~tdBr101$KM, col = ifelse(tdBr101$KM < 60 && tdBr101$KM > 110,"red", "blue"), xlab="Data", ylab="Local do acidente(Km)")
hist(c(tdBr316$DataAcident), xlab = "Quilometro (km)", ylab = "Frequência", main = "Frequência acidentes BR-316/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()


png("./graficos2/br407_%d.png")  ## 7
plot(tdBr407$DataAcident, tdBr407$KM, type = "l", xlab = "Data do Acidente", ylab = "BR 407 / Km", main = "Data do acidente em cada Km da BR 407")
#boxplot(tdBr101$DataAcident~tdBr101$KM, col = ifelse(tdBr101$KM < 60 && tdBr101$KM > 110,"red", "blue"), xlab="Data", ylab="Local do acidente(Km)")
hist(c(tdBr407$DataAcident), xlab = "Quilometro (km)", ylab = "Frequência", main = "Frequência acidentes BR-407/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()


png("./graficos2/br408_%d.png")  ## 8
plot(tdBr408$DataAcident, tdBr408$KM, type = "l", xlab = "Data do Acidente", ylab = "BR 408 / Km", main = "Data do acidente em cada Km da BR 408")
#boxplot(tdBr101$DataAcident~tdBr101$KM, col = ifelse(tdBr101$KM < 60 && tdBr101$KM > 110,"red", "blue"), xlab="Data", ylab="Local do acidente(Km)")
hist(c(tdBr408$DataAcident), xlab = "Quilometro (km)", ylab = "Frequência", main = "Frequência acidentes BR-408/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()


png("./graficos2/br423_%d.png")  ## 9
plot(tdBr423$DataAcident, tdBr423$KM, type = "l", xlab = "Data do Acidente", ylab = "BR 423 / Km", main = "Data do acidente em cada Km da BR 423")
#boxplot(tdBr101$DataAcident~tdBr101$KM, col = ifelse(tdBr101$KM < 60 && tdBr101$KM > 110,"red", "blue"), xlab="Data", ylab="Local do acidente(Km)")
hist(c(tdBr423$DataAcident), xlab = "Quilometro (km)", ylab = "Frequência", main = "Frequência acidentes BR-423/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()


png("./graficos2/br424_%d.png")  ## 10
plot(tdBr424$DataAcident, tdBr424$KM, type = "l", xlab = "Data do Acidente", ylab = "BR 424 / Km", main = "Data do acidente em cada Km da BR 424")
#boxplot(tdBr101$DataAcident~tdBr101$KM, col = ifelse(tdBr101$KM < 60 && tdBr101$KM > 110,"red", "blue"), xlab="Data", ylab="Local do acidente(Km)")
hist(c(tdBr424$DataAcident), xlab = "Quilometro (km)", ylab = "Frequência", main = "Frequência acidentes BR-424/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()


png("./graficos2/br428_%d.png")  ## 11
plot(tdBr428$DataAcident, tdBr428$KM, type = "l", xlab = "Data do Acidente", ylab = "BR 428 / Km", main = "Data do acidente em cada Km da BR 428")
#boxplot(tdBr101$DataAcident~tdBr101$KM, col = ifelse(tdBr101$KM < 60 && tdBr101$KM > 110,"red", "blue"), xlab="Data", ylab="Local do acidente(Km)")
hist(c(tdBr428$DataAcident), xlab = "Quilometro (km)", ylab = "Frequência", main = "Frequência acidentes BR-428/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()




#install.packages("heatmaply")
#library(heatmaply)
#heatmaply(mtcars, k_col = 2, k_row = 3) %>% layout(margin = list(l = 130, b = 40))

#########################################################
### A) Installing and loading required packages
#########################################################

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}
#########################################################
### B) Reading in data and transform it into matrix format
#########################################################

#data <- read.csv("../datasets/heatmaps_in_r.csv", comment.char="#")
#rnames <- data[,1]                            # assign labels in column 1 to "rnames"
#mat_data <- data.matrix(data[,2:ncol(data)])  # transform column 2-5 into a matrix

rnames <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")                            # assign labels in column 1 to "rnames"
rownames(tdBr101) <- rnames

#mat_data <- data.matrix(data[,2:ncol(data)])  # transform column 2-5 into a matrix
#rownames(mat_data) <- rnames 

