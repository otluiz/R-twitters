<<<<<<< HEAD
#install.packages("devtools")
library(devtools)

install_github("twitteR", username="geoffjentry")
library(twitteR)



### Acesso e autenticação da aplicação no Twitter chamada "dadosMestrado"

consumer_key = 'QkvIzuPMG52V1pP9G5nRZmb5D'
consumer_secret = 'BR6yyrmrZpmsGT0YOJ4QBWEX1VbnCd4gwa0IiNm6nbm7ZzwdOC'
access_token = 	'528603134-tltzs4fmhiBdvY1UpZucqzwzNADcsyepQw2ZJ1bL'
access_secret = '58KZ6s8j2rF9w5b2bTdV2tfGRepg2rjHd1pscHxqNtFLc'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#setup_twitter_oauth("API Key", "API Secret")

rdmTweets <- userTimeline('rdatamining', n=200)
(nDocs <- length(rdmTweets))
rdmTweets[11:15]
for (i in 11:15) {cat(paste("[[", i, "]] ", sep="")) + writeLines(strwrap(rdmTweets[[i]]$getText(), width=73))}

df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
dim(df)

library(tm)
library(SnowballC)

install.packages("rJava")
library(rJava)

install.packages("RWekajars")
library(RWekajars)

install.packages("RWeka")
library(RWeka)


myCorpus <- Corpus(VectorSource(df$text))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
myStopwords <- c(stopwords("english"), "available", "via")
myStopwords <- setdiff(myStopwords, c("r", "big"))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)



myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)
for (i in 11:15) {cat(paste("[[", i, "]] ", sep="")) + writeLines(strwrap(myCorpus[[i]], width=73))}

myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=myCorpusCopy)

inspect(myCorpus[11:15])

sessionInfo()

R version 3.1.1 (2014-07-10)
Platform: x86_64-apple-darwin13.1.0 (64-bit)

locale:
  [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8

attached base packages:
  [1] stats     graphics  grDevices utils     datasets  methods  
[7] base     

other attached packages:
  [1] RWekajars_3.7.11-1 rJava_0.9-6        RWeka_0.4-23      
[4] SnowballC_0.5      tm_0.6             NLP_0.1-3         
[7] twitteR_1.1.8      devtools_1.5      

loaded via a namespace (and not attached):
  [1] bit_1.1-12     bit64_0.9-4    digest_0.6.4   evaluate_0.5.5
[5] grid_3.1.1     httr_0.4       memoise_0.2.1  parallel_3.1.1
[9] RCurl_1.95-4.1 rjson_0.2.14   slam_0.1-32    stringr_0.6.2 
[13] tools_3.1.1    whisker_0.3-2 
r
=======
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

### Este é um exemplo do sítio: https://rstudio-pubs-static.s3.amazonaws.com/66739_c4422a1761bd4ee0b0bb8821d7780e12.html
library(SnowballC)
library(twitteR)
library(ROAuth)
token <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

### carrega a time line do Tweeter
tweets <- userTimeline("RDataMining", n = 3200)

### comando para o segundo exemplo de carga dos dados
#load(file = twitterF)
#(n.tweet <- length(tweets))

#########################################################################
###################  LIMPANDO O TEXTO   #################################
####################                  ###################################
### converte os dados para data frame
# tweets.df <- do.call("rbind", lapply(tweets, as.data.frame))
tweets.df2 <- twListToDF(tweets)
dim(tweets.df2)

## [1] 454  16 : exemplo 1
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
# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"), "available", "via")
# remove 'r' and 'big' from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
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
########################################################################
### STEM COMPLETION
#myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion), dictionary = myCorpusCopy, lazy=TRUE)
### CONTA A FREQUENCIA DA PALAVRA "MINING"
#miningCases <- tm_map(myCorpusCopy$content,grep, pattern = "\\<mining")
#sum(unlist(miningCases))

## count frequency of "miners"
#minerCases <- tm_map(myCorpusCopy, grep, pattern = "\\<miners")
#sum(unlist(minerCases))
############################################################################
tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm

## Freqency words and Association #########################################
idx <- which(dimnames(tdm)$Terms == "r")
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

# which words are associated with 'r'?
findAssocs(tdm, "r", 0.2)

# which words are associated with 'mining'?
findAssocs(tdm, "social", 0.25)

#if("graph" %in% rownames(installed.packages()) == FALSE) {
#  installed.packages("graph")
#} 
#library(graph)
#if("Rgraphviz" %in% rownames(installed.packages()) == FALSE) {
#  installed.packages("Rgraphviz")
#} 
#library(Rgraphviz)

#### WORD CLOUD
library(wordcloud)

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)

######################################################################################
##################################              ######################################
###############################   CLUSTERIZANDO   ####################################
##################################              ######################################
######################################################################################
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
#########################              ######################################
#######################   TOPIC MODEL    ####################################
#########################              ######################################
#############################################################################
dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)
#install.packages("topicmodels", dependencies = TRUE)

lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 4) # first 4 terms of every topic
term

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
require(data.table) #fore IDate

topic <- topics(lda, 1)
topics <- data.frame(date=as.IDate(tweets.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")
#>>>>>>> 0da81a1a3ca9bdfc9b40db075008dbf89546629b
