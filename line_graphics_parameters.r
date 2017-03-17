#!/usr/bin/Rscript

args = commandArgs(trailingOnly=TRUE)
dataLocation = args[1]
numberOfColumns = args[2]
fileName = args[3]
abs_path = args[4]

print("Here is data location:")
print(dataLocation)

myData <- read.csv(dataLocation, sep = ";")
fitness = paste0("fitne = ", myData[1,4])
std = paste0("std = ", myData[2,4])
#do it to avoid intefering with the maximum value of data
myData[1,4] = 0
myData[2,4] = 0

#paste0 concatenates two strings
fullFileName = paste0(abs_path,fileName)
#open png device
png(fullFileName, width = 600, height = 800)

#plot first line
#check if it is fitness plot

mar.default <- c(5,4,4,2) + 0.1
par(mfrow=c(3,1), mar = mar.default + c(4,5,2,0))

titlevec = c("Individual","Instintivo","Volitivo")

for(i in 1:3){
	
	plot(myData[,i], type = "l", cex.axis = 3.0, cex.lab = 3.0, ylim = c(min(myData), max(myData)), xlab = "Iteração", ylab = "Valor", col = "black", xaxt="n", yaxt = "n", ann = FALSE)
	title(main = titlevec[i], cex.main = 4.0, line = 2)
	mtext(side = 1, cex = 2.5, text = "Iteração", line = 6)
	mtext(side = 2, cex = 2.5, text = "Distância",  line = 5)
	axis(1, at = seq(0, 5000, by = 1000), cex.axis = 3.0, mgp = c(3, 2, 0), mar = mar.default + c(0,0,2,0))
	axis(2, at = c(0,1,2,3), cex.axis = 3.0, mgp = c(3, 1, 0), las = 2)
	box(which = "figure")

}

dev.off()

