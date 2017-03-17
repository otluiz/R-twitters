setwd("~/workspace/R/src")
#source("src/prfLimpoCheiroso.R")
# dados$x <- NULL ## remove a coluna x

dfn <- read.csv("../data/LimpoCheiroso.csv") ## carrega o data frame

dfn$Binario <- NULL ## remove coluna Binario (não sei pra que coloquei isso)


## calcula e converte para booleano o atributo Gravidade------------------------------------------------
for(i in 1:nrow(dfn)){
  
  if(dfn[i,9] == 0 && dfn[i,10] == 0) { dfn[i,"meuboleano"] = FALSE } ## ninguém morreu
  else 
    if(dfn[i,9] >= 10 && dfn[i,10] == 0) { dfn[i,"meuboleano"] = TRUE } ## muitos feridos alguém morreu
  else 
    if(dfn[i,10] > 0) { dfn[i,"meuboleano"] = TRUE } ## algum morto
  
  dfn$meuboleano <- NULL  ## exclui a coluna meubooleano
  dfn$TotFerVivos <- NULL ## exclui a coluna TotFerVivos
  dfn$TotMortos <- NULL ## exclui a coluna TotMortos
}
###-----------------------------------------------------------------------------------------------------


tdBr101 <- subset(dfn,BR=='101')
tdBr104 <- subset(dfn,BR=='104')
tdBr110 <- subset(dfn,BR=='110')
tdBr116 <- subset(dfn,BR=='116')
tdBr232 <- subset(dfn,BR=='232')
tdBr316 <- subset(dfn,BR=='316')
tdBr407 <- subset(dfn,BR=='407')
tdBr408 <- subset(dfn,BR=='408')
tdBr423 <- subset(dfn,BR=='423')
tdBr424 <- subset(dfn,BR=='424')
tdBr428 <- subset(dfn,BR=='428')

write.csv(dfn,"../data/prfLimpoCheirosinho.csv", row.names = FALSE)

### correlação linear
lm(dfn)


### rodando CART como alternativa a Regressão Logística
library(rpart)
raw = subset(tdBr101, select = c("Gravidade", "CondPista", "TipoAcident", "CausaAcident", "TracadoVia","Hour","KMArredondado"))

#### omitindo qualquer "missing data"
raw = na.omit(raw);

### Dá um nome para o arquivo
png(file = "../graficos/AD_causa_acidente.png")

### definindo uma formula para calcular a causa do acidente
fmla = Gravidade ~ CausaAcident + CondPista + TipoAcident + TracadoVia + Hour + KMArredondado
### calcula uma árvore com a formula: fmla
fit = rpart(fmla, method = "class", data=raw)

### plotar a árvore
plot(fit, uniform = TRUE, main = "Classes de acordo com a Gravidade para BR=101")
text(fit, use.n = TRUE, all=TRUE, cex=0.8)

### salvando a arvore
dev.off()

### Dados de saída
printcp(fit) ### mostra os resultados
plotcp(fit)  ### visualisa os resultados em validação cruzada
summary(fit) ### sumarização detalhada dos partes (splits)
### ajuste do município para delegacia da prf ex: 11067 = Afranio, Petrolina,...
#tdBr101$Municipio <- factor(tdBr101$Municipio, label=c("1101"), levels = (tdBr101$Municipio))
#tdBr232$Municipio <- factor(tdBr232$Municipio, label=c("1101"), levels = (tdBr232$Municipio))
#tdBr408$Municipio <- factor(tdBr408$Municipio, label=c("1101"), levels = (tdBr408$Municipio))
