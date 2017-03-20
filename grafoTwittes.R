#############################################
setwd("~/workspace/R/")

# free memory
rm(list = ls())
gc()

#############################################
### define variáveis para o token Twitter
#############################################
library(twitteR)
library(ROAuth)
library(RCurl)

#reqURL <- "https://api.twitter.com/oauth/request_token"
#accessURL <- "https://api.twitter.com/oauth/access_token"
#authURL <- "https://api.twitter.com/oauth/authorize"
#accessToken    <- '2782117-tltzs4fmhiBdvY1UpZucqzwzNADcsyepQw2ZJ1bL' 
#consumerKey <- 'QkvIzuPMG52V1pP9G5nRZmb5D' #  "rR16FxDLkTYmuVhqH4s4EQ"
#consumerSecret <- 'BR6yyrmrZpmsGT0YOJ4QBWEX1VbnCd4gwa0IiNm6nbm7ZzwdOC' #  "xrGr71kTfdT3ypWFURGxyJOC4Oqf46Rwu4qxyxoEfM"
#twitCred <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, requestURL=reqURL, accessURL=accessURL, authURL=authURL)
consumer_key    <- 	'gMgM98BADb4kiH4Y0M7AD5gw3'
consumer_secret <- 'YA7l1GF6iLfdd8H0ybS9iBgBKXIhsWm0Nu89mHpnMnPcqpwYnZ'
access_token    <- '528603134-F4XrgrN8v5jwzxEvxeHIKjyMxajclVEVnToGYS7x' 
access_secret   <- 'GALsD4zAoopfgLrpBPR4apboUsN6tc0bMo3pLTeAc7nW1'
token <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#tweets<- searchTwitter("#rstats", n=200) #Get the Tweets
#tweets <- twListToDF(tweets)

tweetPRF <- searchTwitter("@PRF191PE", n=200) #Get the Tweets PRF
### conferindo a quantidade de twittes capturados
(n.twittes <- length(tweetPRF))

#########################################################################
###################  LIMPANDO O TEXTO   #################################
####################                  ###################################
### converte os dados para data frame
# tweets.df <- do.call("rbind", lapply(tweets, as.data.frame))
dfTweets <- twListToDF(tweets)
dim(dfTweets)
names(dfTweets)
twtPRF <- twListToDF(twtPRF)


library(graphTweets)

## outro grafo
edgesPRF <- getEdges(data = twtPRF, tweets = "text", source = "screenName")
nodesPRF <- getNodes(edgesPRF)
g <- igraph::graph.data.frame(edgesPRF, directed=TRUE, vertices = nodesPRF)
plot(g)



### criando vértices e arestas com os dados #rstat  #####################
edges <- getEdges(data = tweets, tweets = "text", source = "screenName")
nodes <- getNodes(edges)

# plot
g <- igraph::graph.data.frame(edges, directed=TRUE, vertices = nodes)

plot(g)

#####################################################################
library(igraph)
par(mfrow=c(2,2),mar=c(0,0,0,0), oma=c(0,0,0,0))
g <- watts.strogatz.game(1,20,3,0.4)
layout.old <- layout.fruchterman.reingold(g)
for(i in 1:4){
  layout.new <- layout.fruchterman.reingold(g,params=list(niter=10,maxdelta=2,start=layout.old))
  plot(g,layout=layout.new)
  layout.old <- layout.new
}
########################################################################
library(igraph)
par(mfrow=c(2,2),mar=c(0,0,0,0), oma=c(0,0,0,0))
g <- watts.strogatz.game(1,20,3,0.4)
for(i in 1:4) plot(g,layout=layout.fruchterman.reingold,margin=0)

