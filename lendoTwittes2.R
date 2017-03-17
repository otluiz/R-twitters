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

############################################################
### instala e carrega os dados
############################################################
if("twitteR" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("geoffjentry/twitteR")
}
if("ROAuth" %in% rownames(installed.packages()) == FALSE) {
  installed.packages("ROAuth")
}
if ("graphTweets" %in% rownames(install.packages()) == FALSE) {
  install.packages("graphTweets")
} # CRAN release
if ("igraph" %in% rownames(install.packages()) == FALSE) {
  install.packages("igraph")
}
if("twitteR" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("JohnCoene/graphTweets")
}
#############################################################
library(twitteR)
library(ROAuth)
token <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

library(devtools)
libs <- c("igraph", "twitteR", "graphTweets")
lapply(libs, library, character.only=TRUE)

### pesquisa tweets ###########################################################
#tweets <- userTimeline("PRF191PE", n = 3200)
tweets <- twitteR::searchTwitter("rstats", n = 200)

### unlist to data.frame
#tw_df <- twListToDF(tweets)
tweets <- twitteR::twListToDF(tweets)

### carrega graphTweets
library(graphTweets)

#edges <- getEdges(data = tweets, tweets = "text", source = "screenName")
#nodes <- getNodes(edges)

edges <- getEdges(data = tweets, tweets = "text", source = "screenName", str.length = NULL, "retweetCount", "favorited", "longitude", "latitude")
nodes <- getNodes(edges, source = "source", target = "target" , "retweetCount","favorited", "longitude", "latitude")

# plot
graph <- igraph::graph.data.frame(edges, directed=TRUE, vertices = nodes)
#plot(graph.vertex.size, V(graph)$retweetCount)
write.graph(graph, "../Gephi/graph4.graphml", format="graphml")

# create dynamic graph and open in Gephi
#dyn <- dynamise(tweets, tweets = "text", source = "screenName", 
#                start.stamp = "created", write = TRUE, open = TRUE)

#install.packages(c("tm", "wordcloud"))
#build graph and write graphml - select only the two first column of the edge table since the meta-data is with our node table
graph <- graph.data.frame(edges[,1:2], directed=TRUE, vertices=nodes)
write.graph(graph, "../Gephi/graph4.graphml", format="graphml")

#### tentando criar uma Nuvem de palavras
#install.packages(c("tm", "wordcloud"))
library(tm)
library(wordcloud)

### Limpando o texto
### Constroe o "Corpus" e especifica que a fonte será um vetor de caracteres
b = Corpus(VectorSource(tweets$text), readerControl = list(language = "eng"))

b <- tm_map(b, stemDocument, lazy = TRUE)
#b <- tm_map(b, tolower)
b <- tm_map(b, tolower, lazy = TRUE)
b <- tm_map(b, stripWhitespace, lazy = TRUE)
b <- tm_map(b, removePunctuation, lazy = TRUE)
### remove stopwords 
#minhaStopsWords <- c(stopwords('english'))
#b <- tm_map(b, removeWords, minhaStopsWords, lazy = TRUE)

inspect(b)

tdm <- TermDocumentMatrix(b)
ml  <- as.matrix(tdm)
vl  <- sort(rowSums(ml), decreasing = TRUE)
dl  <- data.frame(word = names(vl), freq = vl)
wordcloud(dl$word, dl$freq)
