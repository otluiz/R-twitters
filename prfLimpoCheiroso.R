setwd("~/workspace/R")
#source("src/prfLimpoCheiroso.R")
<<<<<<< HEAD

# free memory
rm(list = ls())
gc()
=======
# dados$x <- NULL ## remove a coluna x
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b

#dfn <- read.csv("./data/LimpoCheiroso.csv") ## carrega o data frame
#dfr <- read.csv("./data/paraAgrupaR.csv")

## write.csv(br232.novo,"./data/br232.csv", row.names = FALSE)


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
### carregando os data-frames das BRs retira-
### dos do dataset dfr
#############################################
<<<<<<< HEAD
br101 <- read.csv(file="./data/br101.csv", header=TRUE, sep=",") ### 1 (21.862 - 2 = 21,862 ajuste)
=======
br101 <- read.csv(file="./data/BR101/br101.csv", header=TRUE, sep=",") ### 1 (21.862 - 2 = 21,862 ajuste)
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b
br104 <- read.csv(file="./data/br104.csv", header=TRUE, sep=",") ### 2 (3.332 - 2 = 3330 ajuste)
br110 <- read.csv(file="./data/br110.csv", header=TRUE, sep=",") ### 3 (161 -1 = 160 ajuste )
br116 <- read.csv(file="./data/br116.csv", header=TRUE, sep=",") ### 4 (618 - 3 = 615)
### ------------------------------------------------------------------
br232 <- read.csv(file="./data/br232.csv", header=TRUE, sep=",") ### 5 (13.641 - 1 = 13640)
<<<<<<< HEAD
br316 <- read.csv(file="./data/br316.csv", header=TRUE, sep=",") ### 6 (1.432 - 2)
### ------------------------------------------------------------------
br407 <- read.csv(file="./data/br407.csv", header=TRUE, sep=",") ### 7 (1.829 - 4)
br408 <- read.csv(file="./data/br408.csv", header=TRUE, sep=",") ### 8 (2.022 - 2)

br423 <- read.csv(file="./data/br423.csv", header=TRUE, sep=",") ### 9 (1.934 - 4)

br424 <- read.csv(file="./data/br424.csv", header=TRUE, sep=",") ## 10 (1.025) ok
br428 <- read.csv(file="./data/br428.csv", header=TRUE, sep=",") ## 11 (2.500) ok
=======
br316 <- read.csv(file="./data/BR316/br316.csv", header=TRUE, sep=",") ### 6 (1.432 - 2)
### ------------------------------------------------------------------
br407 <- read.csv(file="./data/br407.csv", header=TRUE, sep=",") ### 7 (1.829 - 4)
br408 <- read.csv(file="./data/BR408/br408.csv", header=TRUE, sep=",") ### 8 (2.022 - 2)

br423 <- read.csv(file="./data/BR423/br423.csv", header=TRUE, sep=",") ### 9 (1.934 - 4)

br424 <- read.csv(file="./data/BR424/br424.csv", header=TRUE, sep=",") ## 10 (1.025) ok
br428 <- read.csv(file="./data/BR428/br428.csv", header=TRUE, sep=",") ## 11 (2.500) ok
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b
##### ------------------------------------------------------------------------


#############################################
<<<<<<< HEAD
### ordenando por hora
=======
### ordenando por data
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b
#############################################
br101.ord1 <- br101[order(br101$HoraAcident),] ### 1 ordenando
br104.ord1 <- br104[order(br104$HoraAcident),] ### 2 ordenando
br110.ord1 <- br110[order(br110$HoraAcident),] ### 3 ordenando
br116.ord1 <- br116[order(br116$HoraAcident),] ### 4 ordenando
br232.ord1 <- br232[order(br232$HoraAcident),] ### 5 ordenando
br316.ord1 <- br316[order(br316$HoraAcident),] ### 6 ordenando
br407.ord1 <- br407[order(br407$HoraAcident),] ### 7 ordenando
br408.ord1 <- br408[order(br408$HoraAcident),] ### 8 ordenando

br423.ord1 <- br423[order(br423$HoraAcident),] ### 9 ordenando

br424.ord1 <- br424[order(br424$HoraAcident),] ## 10 ordenando
br428.ord1 <- br428[order(br428$HoraAcident),] ## 11 ordenando



#############################################
<<<<<<< HEAD
### formatando a hora
=======
### formatando a data
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b
#############################################
x101 <- as.POSIXct(br101$HoraAcident, format="%H:%M:%S") ### 1 ajustando a data e hora
x104 <- as.POSIXct(br104.ord1$HoraAcident, format="%H:%M:%S") ### 2 ajustando a data e hora
x110 <- as.POSIXct(br110.ord1$HoraAcident, format="%H:%M:%S") ### 3 ajustando a data e hora
x116 <- as.POSIXct(br116.ord1$HoraAcident, format="%H:%M:%S") ### 4 ajustando a data e hora
x232 <- as.POSIXct(br232.ord1$HoraAcident, format="%H:%M:%S") ### 5 ajustando a data e hora
x316 <- as.POSIXct(br316.ord1$HoraAcident, format="%H:%M:%S") ### 6 ajustando a data e hora
x407 <- as.POSIXct(br407.ord1$HoraAcident, format="%H:%M:%S") ### 7 ajustando a data e hora
x408 <- as.POSIXct(br408.ord1$HoraAcident, format="%H:%M:%S") ### 8 ajustando a data e hora

x423 <- as.POSIXct(br423.ord1$HoraAcident, format="%H:%M:%S") ### 9

x424 <- as.POSIXct(br424.ord1$HoraAcident, format="%H:%M:%S") ## 10 ajustando a data e hora
x428 <- as.POSIXct(br428.ord1$HoraAcident, format="%H:%M:%S") ## 11 ajustando a data e hora




x101r <- myRound(x101) # 1
x104r <- myRound(x104) # 2
x110r <- myRound(x110) # 3
x116r <- myRound(x116) # 4
x232r <- myRound(x232) # 5
x316r <- myRound(x316) # 6
x407r <- myRound(x407) # 7
x408r <- myRound(x408) # 8
x423r <- myRound(x423) # 9 
x424r <- myRound(x424) # 10
x428r <- myRound(x428) # 11

br101$HoraAcident <- as.POSIXct(x101r, format = "%H:%M", tz = "GMT")
br104$HoraAcident <- as.POSIXct(x104r, format = "%H:%M", tz = "GMT")

br110$HoraAcident <- as.POSIXct(x110r, format = "%H:%M", tz = "GMT")
br116$HoraAcident <- as.POSIXct(x116r, format = "%H:%M", tz = "GMT")

br232$HoraAcident <- as.POSIXct(x232r, format = "%H:%M", tz = "GMT")

br316$HoraAcident <- as.POSIXct(x316r, format = "%H:%M", tz = "GMT")
br407$HoraAcident <- as.POSIXct(x407r, format = "%H:%M", tz = "GMT")

<<<<<<< HEAD
br408$HoraAcident <- as.POSIXct(x408r, format = "%H:%M", tz = "GMT")
=======
br408$HoraAcident <- as.POSIXct(x408, format = "%H:%M", tz = "GMT")
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b
br423$HoraAcident <- as.POSIXct(x423r, format = "%H:%M", tz = "GMT")

br424$HoraAcident <- as.POSIXct(x424r, format = "%H:%M", tz = "GMT")
br428$HoraAcident <- as.POSIXct(x428r, format = "%H:%M", tz = "GMT")



#############################################
### dividindo por trechos
#############################################
#br101.ord1.cate <- rep(c("madrugada","manhã","almoço","tarde","noite"), each=4372) ### 1
br101.ord1.cate <- rep(c("00:00 - 04:59","05:00 - 08:59","09:00 - 13:59","14:00 - 18:59","19:00 - 23:59"), each=4372) ### 5

#br104.ord1.cate <- rep(c("madrugada","manhã","almoço","tarde","noite"), each=666) ### 2
br104.ord1.cate <- rep(c("00:00 - 04:59","05:00 - 08:59","09:00 - 13:59","14:00 - 18:59","19:00 - 23:59"), each=666) ### 5

#br110.ord1.cate <- rep(c("madrugada","manhã","almoço","tarde","noite"), each=205) ### 3
br110.ord1.cate <- rep(c("00:00 - 04:59","05:00 - 08:59","09:00 - 13:59","14:00 - 18:59","19:00 - 23:59"), each=32) ### 5

br116.ord1.cate <- rep(c("madrugada","manhã","almoço","tarde","noite"), each=123) ### 4
br116.ord1.cate <- rep(c("00:00 - 04:59","05:00 - 08:59","09:00 - 13:59","14:00 - 18:59","19:00 - 23:59"), each=123) ### 5

br232.ord1.cate <- rep(c("00:00 - 04:59","05:00 - 08:59","09:00 - 13:59","14:00 - 18:59","19:00 - 23:59"), each=2728) ### 5

#br316.ord1.cate <- rep(c("madrugada","manhã","almoço","tarde","noite"), each=286) ### 6 (-2)
br316.ord1.cate <- rep(c("00:00 - 04:59","05:00 - 08:59","09:00 - 13:59","14:00 - 18:59","19:00 - 23:59"), each=286)

br407.ord1.cate <- rep(c("00:00 - 04:59","05:00 - 08:59","09:00 - 13:59","14:00 - 18:59","19:00 - 23:59"), each=365) ### 7 (-4)

br408.ord1.cate <- rep(c("00:00 - 04:59","05:00 - 08:59","09:00 - 13:59","14:00 - 18:59","19:00 - 23:59"), each=404) ### 8 (-2)

br423.ord1.cate <- rep(c("00:00 - 04:59","05:00 - 08:59","09:00 - 13:59","14:00 - 18:59","19:00 - 23:59"), each=205) ### 9 (-4)

br424.ord1.cate <- rep(c("00:00 - 04:59","05:00 - 08:59","09:00 - 13:59","14:00 - 18:59","19:00 - 23:59"), each=205) ## 10 ok

br428.ord1.cate <- rep(c("00:00 - 04:59","05:00 - 08:59","09:00 - 13:59","14:00 - 18:59","19:00 - 23:59"), each=500) ## 11 ok


#############################################
### gerando vários tipos de gráficos
#############################################
<<<<<<< HEAD
png("./graficos/br101_%d.png")  ## 1
plot(x101,br101.ord1$KM, col = ifelse(br101.ord1$KM < 40, "blue","red"), xlab = "Hora do Acidente", ylab = "Local do Acidente(KM)")
=======
png("./graficos/br1011_%d.png")  ## 1
plot(x101,br101.ord1$KM, col = ifelse(br101.ord1$KM > 105, "blue","red"))
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b
boxplot(br101.ord1$KM~br101.ord1.cate, col = ifelse(br101.ord1$KM < 60 && br101.ord1$KM > 110,"red", "blue"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(br101.ord1$KM, xlab = "Quilometro (km)", ylab = "Frequência",col = ifelse(br101.ord1$KM <= 60, "blue","red") , main = "Frequência acidentes BR-101/km ")
#ggplot(br101.ord1$KM, aes(br101$KM)) + geom_bar(position = "dodge")
dev.off()

png("./graficos/br104_%d.png") ## 2
plot(x104,br104.ord1$KM, col = ifelse(br104.ord1$KM < 60, "blue","red"), xlab = "Hora do Acidente", ylab = "Local do Acidente(KM)")
boxplot(br104.ord1$KM~br104.ord1.cate, col = ifelse(br104.ord1$KM < 60, "blue","red"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(br104.ord1$KM, xlab = "Quilometro (km)", ylab = "Frequência",col = ifelse(br104.ord1$KM <= 60, "blue","red") , main = "Frequência acidentes BR-104/km ")
dev.off()

png("./graficos/br110_%d.png") ## 3
plot(x110,br110.ord1$KM, col = ifelse(br110.ord1$KM < 90, "blue","red"), xlab = "Hora do Acidente", ylab = "Local do Acidente(KM)")
boxplot(br110.ord1$KM~br110.ord1.cate, col = ifelse(br110.ord1$KM < 90, "blue","red"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(br110.ord1$KM, xlab = "Quilometro (km)", ylab = "Frequência",col = ifelse(br110.ord1$KM <= 90, "blue","red") , main = "Frequência acidentes BR-110/km ")
dev.off()

png("./graficos/br116_%d.png") ## 4
plot(x116,br116.ord1$KM, col = ifelse(br116.ord1$KM < 30, "blue","red"), xlab = "Hora do Acidente", ylab = "Local do Acidente(KM)")
boxplot(br116.ord1$KM~br116.ord1.cate, col = ifelse(br116.ord1$KM < 20, "blue","red"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(br116.ord1$KM, xlab = "Quilometro (km)", ylab = "Frequência",col = ifelse(br116.ord1$KM <= 20, "blue","red") , main = "Frequência acidentes BR-116/km ")
dev.off()

png("./graficos/br232_%d.png") ## 5
plot(x232,br232.ord1$KM, col = ifelse(br232.ord1$KM < 180, "blue","red"), xlab = "Hora do Acidente", ylab = "Local do Acidente(KM)")
boxplot(br232.ord1$KM~br232.ord1.cate, col = ifelse(br232.ord1$KM < 60, "blue","red"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(br232.ord1$KM, xlab = "Quilometro (km)", ylab = "Frequência",col = ifelse(br232.ord1$KM <= 80, "blue","red") , main = "Frequência acidentes BR-232/km ")
#ggplot(br232, aes(x = x232r, y = br232.ord1$KM, colour = br232.ord1$KM)) + geom_point() + xlab = ("Hora do Acidente") + ylab = ("Local do Acidente(KM)")
dev.off()

png("./graficos/br316_%d.png") ## 6
plot(x316,br316.ord1$KM, col = ifelse(br316.ord1$KM < 200, "blue","red"), xlab = "Hora do Acidente", ylab = "Local do Acidente(KM)")
boxplot(br316.ord1$KM~br316.ord1.cate, col = ifelse(br316.ord1$KM < 200, "blue","red"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(br316.ord1$KM, xlab = "Quilometro (km)", ylab = "Frequência",col = ifelse(br316.ord1$KM <= 100, "blue","red") , main = "Frequência acidentes BR-316/km ")
dev.off()

png("./graficos/br407_%d.png") ## 7
plot(x407,br407.ord1$KM, col = ifelse(br407.ord1$KM > 115, "red","blue"), xlab = "Hora do Acidente", ylab = "Local do Acidente(KM)")
boxplot(br407.ord1$KM~br407.ord1.cate, col = ifelse(br407.ord1$KM > 119, "blue","red"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(br407.ord1$KM, xlab = "Quilometro (km)", ylab = "Frequência",col = ifelse(br407.ord1$KM < 130, "red","blue") , main = "Frequência acidentes BR-407/km ")
dev.off()

png("./graficos/br408_%d.png") ## 8
plot(x408,br408.ord1$KM, col = ifelse(br408.ord1$KM < 60, "blue","red"), xlab = "Hora do Acidente", ylab = "Local do Acidente(KM)")
boxplot(br408.ord1$KM~br408.ord1.cate, col = ifelse(br408.ord1$KM < 60, "blue","red"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(br408.ord1$KM, xlab = "Quilometro (km)", ylab = "Frequência",col = ifelse(br408.ord1$KM <= 80, "blue","red") , main = "Frequência acidentes BR-408/km ")
dev.off()

png("./graficos/br423_%d.png") ## 9
plot(x423,br423.ord1$KM, col = ifelse(br423.ord1$KM < 60, "blue","red"), xlab = "Hora do Acidente", ylab = "Local do Acidente(KM)")
boxplot(br423.ord1$KM~br423.ord1.cate, col = ifelse(br423.ord1$KM < 60, "blue","red"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(br423.ord1$KM, xlab = "Quilometro (km)", ylab = "Frequência",col = ifelse(br423.ord1$KM <= 80, "blue","red") , main = "Frequência acidentes BR-423/km ")
dev.off()
##------------------------------------------------------------------------------------------------------------------
png("./graficos/br424_%d.png") ## 10
plot(x424,br424.ord1$KM, col = ifelse(br424.ord1$KM < 60, "blue","red"), xlab = "Hora do Acidente", ylab = "Local do Acidente(KM)")
boxplot(br424.ord1$KM~br424.ord1.cate, col = ifelse(br424.ord1$KM < 60, "blue","red"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(br424.ord1$KM, xlab = "Quilometro (km)", ylab = "Frequência",col = ifelse(br424.ord1$KM <= 80, "blue","red") , main = "Frequência acidentes BR-424/km ")
dev.off()

png("./graficos/br428_%d.png") ## 11
plot(x428,br428.ord1$KM, col = ifelse(br428.ord1$KM < 110, "blue","red"), xlab = "Hora do Acidente", ylab = "Local do Acidente(KM)")
boxplot(br428.ord1$KM~br428.ord1.cate, col = ifelse(br428.ord1$KM < 110, "blue","red"), xlab="Período do dia", ylab="Local do acidente(Km)")
hist(br428.ord1$KM, xlab = "Quilometro (km)",col = ifelse(br428.ord1$KM <= 150, "blue","red") , main = "Frequência acidentes BR-428/km ")
dev.off()
