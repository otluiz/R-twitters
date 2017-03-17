##########################################################################################
### aponta para o diretório de trabalho e limpa o buffer de memória do R
##########################################################################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

##########################################################################################
### acessa autentica e carrega os dados do twitter
##########################################################################################
library(ROAuth)
library(twitteR)
library(RCurl)
library(bitops)
library(tm)


### Acesso e autenticação da aplicação no Twitter chamada "dadosMestrado"

consumer_key = 'QkvIzuPMG52V1pP9G5nRZmb5D'
consumer_secret = 'BR6yyrmrZpmsGT0YOJ4QBWEX1VbnCd4gwa0IiNm6nbm7ZzwdOC'
access_token = 	'528603134-tltzs4fmhiBdvY1UpZucqzwzNADcsyepQw2ZJ1bL'
access_secret = '58KZ6s8j2rF9w5b2bTdV2tfGRepg2rjHd1pscHxqNtFLc'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

### pesquisa no Twitter
vpr_twitter <- searchTwitter("vemprarua", n = 1500, lang = "pt") #vemprarua

### converte lista para vetor
vpr_twitter_text <- sapply(vpr_twitter, function(x) x$getText())

### coloca cada observação em um vetor
vs <- VectorSource(vpr_twitter_text)

### armazena os vetores em documentos
temp <- Corpus(vs)

###########  Preprocessamento ################
### remove números
wc_corpus <- tm_map(temp, removeNumbers)

### remove pontuação
wc_corpus <- tm_map(wc_corpus, removePunctuation)

### remove palavras ruido
wc_corpus <- tm_map(wc_corpus, removeWords, stopwords('portuguese'))

### coloca tudo em letras minúsculas
wc_corpus <- tm_map(wc_corpus, content_transformer(tolower))

