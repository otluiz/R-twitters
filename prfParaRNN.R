#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

dfrnn <- read.csv("./data/RedeNeural/prfParaRNN.csv")
attach(dfrnn)
names(dfrnn)
str(dfrnn)

for(i in 1:nrow(dfrnn)){
  dfrnn[i,"tx_Gravidade"] = dfrnn[i,"tx_RestVisibi"]  * dfrnn[i,"tx_CondPista"]  * dfrnn[i,"tx_TracadoVia"] + dfrnn[i, "Gravidade"]
}

write.csv(dfrnn,"~/workspace/weka/prfRNNtxGravidade.csv", row.names = FALSE)

## mudar de ordem as colunas
movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}

#dd <- data.frame(b = factor(c("Hi", "Med", "Hi", "Low"), 
#                            levels = c("Low", "Med", "Hi"), ordered = TRUE),
#                x = c("A", "D", "A", "C"), y = c(8, 3, 9, 9),
#                 z = c(1, 1, 1, 2))
dfnovo <- movetolast(dfrnn, c("BR"))
#dm <- as.data.frame(dm)
dfrnn <- dfnovo
### conjunto de treino, amostra com 30%
smp_size = floor(0.70 * nrow(dfrnn))
set.seed(123456)

treino_ind <- sample(seq_len(nrow(dfrnn)), size = smp_size)

treino <- dfrnn[treino_ind,]
testes <- dfrnn[-treino_ind,]

nomeArq = "./data/RedeNeural/txGravidadeRNN.csv"
write.csv(treino,"~/workspace/weka/treinotxGrav2.csv", row.names = FALSE)
write.csv(testes,"~/workspace/weka/testetxGrav2.csv", row.names = FALSE)
