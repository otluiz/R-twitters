#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

df <- read.csv("./data/prfPeriodoTx.csv") ## carrega o data frame

#### limpando o dada frame
df$Gravidade <- NULL
#df$Delegacia <- NULL
df$probAcident <- NULL

### Ajuste do Campo BR
for(i in 1:nrow(df)){
  if(df[i, "BR"] == "101") { df[i, "BR"] = "BR101" }
  if(df[i, "BR"] == "104") { df[i, "BR"] = "BR104" }
  if(df[i, "BR"] == "110") { df[i, "BR"] = "BR110" }
  if(df[i, "BR"] == "116") { df[i, "BR"] = "BR116" }
  if(df[i, "BR"] == "232") { df[i, "BR"] = "BR232" }
  if(df[i, "BR"] == "316") { df[i, "BR"] = "BR316" }
  if(df[i, "BR"] == "407") { df[i, "BR"] = "BR407" }
  if(df[i, "BR"] == "408") { df[i, "BR"] = "BR408" }
  if(df[i, "BR"] == "423") { df[i, "BR"] = "BR423" }
  if(df[i, "BR"] == "424") { df[i, "BR"] = "BR424" }
  if(df[i, "BR"] == "428") { df[i, "BR"] = "BR428" }
}

write.csv(df,"./data/TodasBRs/arvoreConclusao.csv", row.names = FALSE)
dfA <- read.csv("./data/TodasBRs/arvoreConclusao.csv") ## carrega o data frame

library(RWeka)
write.arff(dfA,"./data/TodasBRs/arvoreConclusao.arff", eol = "\n")

