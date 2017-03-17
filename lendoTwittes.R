<<<<<<< HEAD
##########################################################################################
### aponta para o diretório de trabalho e limpa o buffer de memória do R
##########################################################################################
=======
#############################################
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

<<<<<<< HEAD
##########################################################################################
### acessa autentica e carrega os dados do twitter
##########################################################################################
#install.packages("twitteR")
#install.packages("ROAuth")
library(ROAuth)
library(twitteR)

### Acesso e autenticação da aplicação no Twitter chamada "dadosMestrado"

consumer_key = 'QkvIzuPMG52V1pP9G5nRZmb5D'
consumer_secret = 'BR6yyrmrZpmsGT0YOJ4QBWEX1VbnCd4gwa0IiNm6nbm7ZzwdOC'
access_token = 	'528603134-tltzs4fmhiBdvY1UpZucqzwzNADcsyepQw2ZJ1bL'
access_secret = '58KZ6s8j2rF9w5b2bTdV2tfGRepg2rjHd1pscHxqNtFLc'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#############################################################################################
=======
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
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b
### as ultimas 50 tendências do Twitter
#trendsBrasil$name[1:50]
### as últimas que falam de fora Temer
#imp <- searchTwitteR('fora temer', n = 50)
#imp[c(1, 25, 50)]
#tweet(" Fora Temer golpista")
<<<<<<< HEAD
##########################################################################################
### usando a timeLine para pegar o máximo de tweets permitidos
##########################################################################################
twittes0 <- userTimeline("PRF191PE", n = 3200)
twittes1 <- userTimeline("AnonymousBRasil", n = 3200)
twittes2 <- userTimeline("@AnonymousBR", n = 3200)


### conferindo a quantidade de twittes capturados
(n.twittes0 <- length(twittes0))
(n.twittes1 <- length(twittes1))
(n.twittes2 <- length(twittes2))

### convertendo os Tweets para tipo data frame
twittes.df0 <- twListToDF(twittes0)
twittes.df1 <- twListToDF(twittes1)
twittes.df2 <- twListToDF(twittes2)
=======
##---------------------------------------------------

### usando a timeLine para pegar o máximo de tweets permitidos
twittes <- userTimeline("PRF191PE", n = 3200)


### conferindo a quantidade de twittes capturados
(n.twittes <- length(twittes)) 

### convertendo os Tweets para tipo data frame
twittes.df <- twListToDF(twittes)
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b

### organizando as coisas de outra forma (fonte internet diferente dos slides Rdatamining)
twittes.df[c("id", "created", "screenName", "replyToSN", "favoriteCount", "retweetCount", "longitude", "latitude", "text")]

<<<<<<< HEAD
#################################################################################################
#### gerando um csv para Weka
##########################################################################################

twittes.df$id <- NULL
twittes.df$replyToSN <- NULL
twittes.df$replyToSID <- NULL
twittes.df$replyToUID <- NULL
twittes.df$isRetweet <- NULL
twittes.df$retweeted <- NULL
twittes.df$latitude <- NULL
twittes.df$longitude<- NULL
twittes.df$screenName <- NULL
twittes.df$statusSource <- NULL

write.csv(twittes.df,"./data/twittesDF.csv", row.names = FALSE) ### gravando o data frame em formato csv

###################################################################################################
### colocando em ordem descrescente da data para porterior retirar 1 mês
##########################################################################################
twittes.df <-twittes.df[order(twittes.df$created,  decreasing = TRUE),]

=======

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
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b

### imprimir o tweet #191 e produzir um texto ajustado para largura de slide
writeLines(strwrap(twittes.df$text, 60))

<<<<<<< HEAD
##########################################################################################
### Limpando o texto
### habilita as funções de Text Mining
##########################################################################################
library(tm)

=======

### Limpando o texto
>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b
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
