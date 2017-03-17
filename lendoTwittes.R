#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

#############################################
### carrega os dados
#############################################
install.packages("twitteR")
#installed.packages("ROAuth")
library(twitteR)

### habilita as funções de Text Mining
### constrõe um 'corpus', que é uma coleção de documentos texto
library(tm)

##---------------------------------------------------
### as ultimas 50 tendências do Twitter
#trendsBrasil$name[1:50]
### as últimas que falam de fora Temer
#imp <- searchTwitteR('fora temer', n = 50)
#imp[c(1, 25, 50)]
#tweet(" Fora Temer golpista")
##---------------------------------------------------

### usando a timeLine para pegar o máximo de tweets permitidos
twittes <- userTimeline("PRF191PE", n = 3200)


### conferindo a quantidade de twittes capturados
(n.twittes <- length(twittes)) 

### convertendo os Tweets para tipo data frame
twittes.df <- twListToDF(twittes)

### organizando as coisas de outra forma (fonte internet diferente dos slides Rdatamining)
twittes.df[c("id", "created", "screenName", "replyToSN", "favoriteCount", "retweetCount", "longitude", "latitude", "text")]


#### gerando um csv para Weka
#twittes.df$id <- NULL
#twittes.df$replyToSN <- NULL
#twittes.df$replyToSID <- NULL
#twittes.df$replyToUID <- NULL
#twittes.df$isRetweet <- NULL
#twittes.df$retweeted <- NULL
#twittes.df$latitude <- NULL
#twittes.df$longitude<- NULL
#twittes.df$screenName <- NULL
#twittes.df$statusSource <- NULL

### colocando em ordem descrescente da data para porterior retirar 1 mês
twittes.df <-twittes.df[order(twittes.df$created,  decreasing = TRUE),]

write.csv(twittes.df,"./data/twittesDF.csv", row.names = FALSE)

### imprimir o tweet #191 e produzir um texto ajustado para largura de slide
writeLines(strwrap(twittes.df$text, 60))


### Limpando o texto
### Constroe o "Corpus" e especifica que a fonte será um vetor de caracteres
meuCorpus <- Corpus(VectorSource(twittes.df$text))

### converte tudo para "caixa baixa"
meuCorpus <- tm_map(meuCorpus, content_transformer(tolower))

### remove as URLs que por ventura ainda apareçam
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
meuCorpus <- tm_map(meuCorpus, content_transformer(removeURL))

### remove algum outro caracter de pontuação: O exemplo foi para caracteres em Inglês
removeNumPont <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
meuCorpus <- tm_map(meuCorpus, content_transformer(removeNumPont))
meuCorpus <- tm_map(meuCorpus, removePunctuation)

### remove números
meuCorpus <- tm_map(meuCorpus, removeNumbers)

### remove stopwords 
minhaStopsWords <- c(stopwords('portuguese'))
meuCorpus <- tm_map(meuCorpus, removeWords, minhaStopsWords)

### mantém um a cópia do corpus
meuCorpusCopia <- meuCorpus

### stem words (palavras tronco)
meuCorpus <- tm_map(meuCorpus, stemDocument)
