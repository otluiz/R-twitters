#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

#############################################
### define variáveis para o token Twitter
#############################################
consumer_key    <- 	'QkvIzuPMG52V1pP9G5nRZmb5D'
consumer_secret <- 'BR6yyrmrZpmsGT0YOJ4QBWEX1VbnCd4gwa0IiNm6nbm7ZzwdOC'
access_token    <- '528603134-tltzs4fmhiBdvY1UpZucqzwzNADcsyepQw2ZJ1bL' 
access_secret   <- '58KZ6s8j2rF9w5b2bTdV2tfGRepg2rjHd1pscHxqNtFLc'

#############################################################
### Este é um exemplo do sítio: https://rstudio-pubs-static.s3.amazonaws.com/66739_c4422a1761bd4ee0b0bb8821d7780e12.html
library(SnowballC)
library(twitteR)
library(ROAuth)
token <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

### carrega a time line do Tweeter
tweets <- userTimeline("PRF191PE", n = 3200)

#########################################################################
###################  LIMPANDO O TEXTO   #################################
####################                  ###################################
### converte os dados para data frame
# tweets.df <- do.call("rbind", lapply(tweets, as.data.frame))
tweets.df <- twListToDF(tweets)
dim(tweets.df)
## [1] 454  16 : exemplo 1
## [1] 2873 16 : exemplo 2
library(tm)

### CARREGANDO O PACOTE: NLP (processamento lingugem natural) ###########
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))
# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))  #??

### remove as URLs que por ventura ainda apareçam
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

### remove algum outro caracter de pontuação: O exemplo foi para caracteres em Inglês
removeNumPont <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPont))
myCorpus <- tm_map(myCorpus, removePunctuation)

# add two extra stop words: 'available' and 'via' 
myStopwords <- c(stopwords("english"), "available", "via") # PARA DOCUMENTO EM INGLÊS
## add extra stop words
myStopwords <- c(stopwords("portuguese"), "ia", "faria", "ainda", "caminho", "alcool", "acid", "veic", "ferido")
# remove 'r' and 'big' from stopwords
myStopwords <- setdiff(myStopwords, c("vou", "fazer"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)

# inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
# The code below is used for to make text fit for paper width 
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(myCorpus[[i]]))
}

############################################################################
tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm

## Freqency words and Association #########################################
idx <- which(dimnames(tdm)$Terms == "h")
inspect(tdm[idx + (0:5), 101:110])
###########################################################################
#inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq=15))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=5)
df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)

## 
## Attaching package: 'ggplot2'
## 
## The following object is masked from 'package:NLP':
## 
##     annotate
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()

# which words are associated with 'c'?
findAssocs(tdm, "r", 0.2)
findAssocs(tdm, "h", 0.2)

# which words are associated with 'acidente'?
findAssocs(tdm, "acidente", 0.25)
findAssocs(tdm, "álcool", 0.2)
findAssocs(tdm, "habilitação", 0.2)

#### Gerea um gŕafico de frequência tipo NUVEM de PALAVRAS
library(wordcloud)

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)

#############################################################################
#########################             #######################################
#######################   CLUSTERIZANDO  ####################################
#########################             #######################################
#############################################################################
# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")

plot(fit)
rect.hclust(fit, k = 6) # cut tree into 6 c

m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 6 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

# partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m3, metric="manhattan")

library(fpc)
k <- pamResult$nc # number of clusters identified
pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) {
  cat("cluster", i, ": ",
      colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
}

# plot clustering result
layout(matrix(c(1, 2), 1, 2)) # set to two graphs per page
plot(pamResult, col.p = pamResult$clustering)

#############################################################################
#######################   TOPIC MODEL    ####################################
#############################################################################

dtm <- as.DocumentTermMatrix(tdm)
###########################################################################
###############  a matriz de documentos estava   ##########################
###############  com algumas entradas vazias     ##########################
###########################################################################
#dtm <- DocumentTermMatrix(crude, sparse=TRUE)
#dtm<-removeSparseTerms(dtm, sparse=0.98)
rowTotals <- apply(dtm , 1, sum)  #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]  #remove all docs without words
###########################################################################
library(topicmodels)
#install.packages("topicmodels", dependencies = TRUE)

lda <- LDA(dtm.new, k = 8) # find 8 topics
term <- terms(lda, 4) # first 4 terms of every topic
term

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
require(data.table) #fore IDate

topic <- topics(lda, 1)
#topics <- data.frame(date=as.IDate(tweets.df$created), topic)
topics <- data.frame(date=as.(tweets.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")

