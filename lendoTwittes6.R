#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

#############################################
### define variáveis para o token Twitter: NOVA CONFIGURAÇÃO
#############################################
consumer_key    <- 	'gMgM98BADb4kiH4Y0M7AD5gw3'
consumer_secret <- 'YA7l1GF6iLfdd8H0ybS9iBgBKXIhsWm0Nu89mHpnMnPcqpwYnZ'
access_token    <- '528603134-F4XrgrN8v5jwzxEvxeHIKjyMxajclVEVnToGYS7x' 
access_secret   <- 'GALsD4zAoopfgLrpBPR4apboUsN6tc0bMo3pLTeAc7nW1'

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
dfTweets <- twListToDF(tweets)
dim(dfTweets)
names(dfTweets)

write.csv(dfTweets,"./twitter/Twitter07042017.csv", row.names = FALSE)
### CARREGANDO O PACOTE: NLP (processamento lingugem natural) ###########
library(tm)

myCorpus <- Corpus(VectorSource(dfTweets$text))              # build a corpus, and specify the source to be character vectors
myCorpus1 <- tm_map(myCorpus, content_transformer(tolower))   # tm v0.6 # convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
myCorpus2 <- tm_map(myCorpus1, removePunctuation)              # remove punctuation
myCorpus3 <- tm_map(myCorpus2, removeNumbers)                  # remove numbers
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)     # remove URLs
### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
myCorpus4 <- tm_map(myCorpus3, content_transformer(removeURL))      # # remove as URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)    
myCorpus5 <- tm_map(myCorpus4, content_transformer(removeURL))       # remove as URLs que por ventura ainda apareçam
removeNumPont <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus6 <- tm_map(myCorpus5, content_transformer(removeNumPont))   # remove algum outro caracter de pontuação: O exemplo foi para caracteres em Inglês
myCorpus7 <- tm_map(myCorpus6, removePunctuation)

# add two extra stop words: 'available' and 'via' 
#myStopwords <- c(stopwords("english"), "available", "via") # PARA DOCUMENTO EM INGLÊS
## add extra stop words
myStopwords <- c(stopwords("portuguese"), "alcool", "acid", "veic", "ferido") #myStopwords
myStopwords <- setdiff(myStopwords, c("ia", "faria"))       # remove 'vou' and 'fazer' from stopwords
myCorpus8 <- tm_map(myCorpus7, removeWords, myStopwords)       # remove stopwords from corpus
dictCorpus <- myCorpus8                                       #￼# keep a copy of corpus to use later as a dictionary for stem completion 
myCorpus9 <- tm_map(myCorpus8, stemDocument)                   # stem words
#inspect(myCorpus)                                           # resultado ainda trás um num excessivo documentos
myCorpus10 <- tm_map(myCorpus9, stemCompletion, dictionary=dictCorpus) # stem completion
# the following stem completion works in tm v0.6 
tm_map(myCorpus10, content_transformer(function(x, d)
  paste(stemCompletion(strsplit(stemDocument(x), ' ')[[1]], d), collapse = ' ')), dictCorpus)
# fix up 1) differences between us and aussie english 2) general errors
#myCorpus <- tm_map(myCorpus, content_transformer(gsub), pattern = 'organiz', replacement = 'organ')
inspect(myCorpus10)




# inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
# The code below is used for to make text fit for paper width 
for (i in 1:15) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(myCorpus[[i]]))
}
## inspect(myCorpus) esse comando trás muita coisa

# ---- Building a Document-Term Matrix

myDtm <- TermDocumentMatrix(myCorpus10, control = list(wordLengths = c(1, Inf)))
inspect(myDtm)

# get tf-idf weighting 
myDtm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf), weighting = function(x) weightTfIdf(x, normalize = FALSE)))
inspect(myDtm)
# Based on the above matrix, many data mining tasks can be done, for example, clustering, classification and association analysis.

# ----- Frequent Terms and Associations

findFreqTerms(myDtm, lowfreq=2)

# which words are associated with "r"?
findAssocs(myDtm, 'r', 0.30)

#read 1000 txt articles from directory data/txt
#corpus  <-Corpus(DirSource("data/txt"), readerControl = list(blank.lines.skip=TRUE))

#or compute cosine distance among documents
#dissimilarity(tdm, method = "cosine")

library(wordcloud)
wordcloud(myCorpus, scale=c(5,0.5), 
          max.words=30, random.order=FALSE
          #, rot.per=0.35, use.r.layout=FALSE
          ,colors=brewer.pal(10, "Dark2")
)

data("crude")
# Term Document Matrix
#tdm <- TermDocumentMatrix(crude)
#findFreqTerms(tdm, 2, 3)

# Document Term Matrix
dtMatrix <- DocumentTermMatrix(crude)
# created a Term-Term Adjacency Matrix.
termMatrix <- t(as.matrix(dtMatrix)) %*% as.matrix(dtMatrix) 
# inspect terms numbered 5 to 10
termMatrix[5:10,5:10]

termMatrix <- termMatrix[1:20,1:20]

library(igraph)

# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$degree <- degree(g)
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam

# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)
