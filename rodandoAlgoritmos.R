setwd("~/workspace/R/src")

# dados$x <- NULL ## remove a coluna x

df <- read.csv("../data/prfCalculoTaxas.csv") ## carrega o data frame
qtdLinhas <- nrow(df)

### eliminando colunas que não serão usadas no algoritmo
levels(df)
df$BR <- NULL
df$