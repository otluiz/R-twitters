#############################################
setwd("~/workspace/R/")

# free memory
rm(list=ls())
#install.packages(c("devtools", "rjson", "bit64", "httr"))
#install_github("twitteR", username="geoffjentry")

gc()
#############################################
library(SnowballC)
library(twitteR)
#-------[ TODO o dataset para ser separado em conjunto de treinamento e testes  ]------------------------
df.Tweets <- read.csv("./twitter/Twitter07042017.csv") ## carrega o data frame
df.graphs <- df.Tweets ## cópia para gerar grafos
#fTweets <- as.data.frame(do.call(rbind, rdmTweets))
names(df.Tweets) <- c('text')

library(tm)

#-------Constroi o Corpus
#meuCorpus <- Corpus(VectorSource(df.Tweets$text))

#------- Mostra o texto do 3º documento
#meuCorpus[[3]]$content 


##########################################################################
###################   PREPROCESSAMENTO   #################################
####################   limpar o textos  ##################################
##########################################################################
makeCorpus <- function(text){ #Function for making corpus and cleaning the tweets fetched
  #twitterdf <- do.call("rbind", lapply(text, as.data.frame)) #store the fetched tweets as a data frame
  #twitterdf$text <- sapply(twitterdf$text,function(row) iconv(row, "latin1", "ASCII", sub=""))#Removing emoticons from tweets
  meuCorpus <- Corpus(VectorSource(df.Tweets$text)) #-------Constroi o Corpus
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) #function to replace a pattern to white space using regex
  meuCorpus <- tm_map(meuCorpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)") #match rt or via
  meuCorpus <- tm_map(meuCorpus, toSpace, "@\\w+") #match @
  meuCorpus <- tm_map(meuCorpus, toSpace, "[ \t]{2,}") #match tabs
  meuCorpus <- tm_map(meuCorpus, toSpace, "[ |\n]{1,}") #match new lines
  meuCorpus <- tm_map(meuCorpus, toSpace, "^ ") #match white space at begenning
  meuCorpus <- tm_map(meuCorpus, toSpace, " $") #match white space at the end
  meuCorpus <- tm_map(meuCorpus, PlainTextDocument)
  meuCorpus <- tm_map(meuCorpus, removeNumbers)
  meuCorpus <- tm_map(meuCorpus, removePunctuation)
  meuCorpus <- tm_map(meuCorpus, toSpace, "http[[:alnum:]]*") #remove url from tweets
  meuCorpus <- tm_map(meuCorpus,removeWords,stopwords("en"))
  meuCorpus <- tm_map(meuCorpus, content_transformer(tolower))
  return(meuCorpus)
}

#---- Executa a função e carrega numa variável-----------
get <- makeCorpus(df.Tweets$text)

###### pesquisa alguns termos
for (i in 1:100){
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(corp[[i]], width=73))
}

#Wordcloud
library(RColorBrewer)
library(wordcloud)
makeWordcloud<-function (getText){ #plotting wordcloud
  twicorpus<-makeCorpus(getText)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf))) #Create TDM
  matrix<-as.matrix(myTdm)
  wordFreq <- sort(rowSums(matrix), decreasing=TRUE)#find frequency of words and sorting them in descending
  set.seed(375) 
  grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
  #wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F, random.color = TRUE, max.words=40, colors=grayLevels, vfont=c("gothic english","plain"))
  #wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F, random.color = TRUE, max.words=40, scale = c(6,.3), colors=brewer.pal(10, "Dark2")) #colors=grayLevels)
  wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F, random.color = TRUE, max.words=50, colors=brewer.pal(10, "Dark2")) #colors=grayLevels)
}


#-------------------- Palavra mais frequentes ---------------
library(ggplot2)
freqPlot<-function (getText){ #frequency plot of word count
  twicorpus<-makeCorpus(getText)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf)))
  matrix<-as.matrix(myTdm)
  termFrequency <- rowSums(matrix)
  termFrequency <- subset(termFrequency, termFrequency>=60)
  df <- data.frame(term=names(termFrequency), freq=termFrequency)
  ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + coord_flip()
}

#-------------------- Clustering ----------------------------
hCluster<-function (content){ #hierarchical clustering 
  twicorpus<-makeCorpus(content)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf)))
  myTdm2 <- removeSparseTerms(myTdm, sparse=0.98) #removing sparse terms
  m2 <- as.matrix(myTdm2)
  distMatrix <- dist(scale(m2)) #calculating distance between terms
  fit <- hclust(distMatrix, method="ward.D") #clustering terms
  plot(fit)
  rect.hclust(fit, k=5) #cutting the tree into 5 clusters
  (groups <- cutree(fit, k=5))
}

#------------------- Kmeans -----------------------------------
kMeans<-function (content){ #k-means clustering
  twicorpus<-makeCorpus(content)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf)))
  myTdm2 <- removeSparseTerms(myTdm, sparse=0.98)
  m2 <- as.matrix(myTdm2)
  m3 <- t(m2) # creating transpose of matrix
  set.seed(122)
  k <- 8
  kmeansResult <- kmeans(m3, k)
  round(kmeansResult$centers, digits=3) #cluster centers
  for (i in 1:k) { #printing 15 terms of each cluster
    cat(paste("cluster ", i, ": ", sep=""))
    s <- sort(kmeansResult$centers[i,], decreasing=T)
    cat(names(s)[1:15], "\n")
  }
}

#------------------- Kmediod -----------------------------------
kMediod<-function (content){
  twicorpus<-makeCorpus(content)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf)))
  myTdm2 <- removeSparseTerms(myTdm, sparse=0.95)
  m2 <- as.matrix(myTdm2)
  m3 <- t(m2)
  pamResult <- pamk(m3, metric="manhattan")
  return(pamResult)
}

#------------------- Sentiment Analysis -----------------------------------
install.packages("Stem")
library(twitteR)
library(sentiment)

tSentimen<-function (content){
  twicorpus<-makeCorpus(content)
  dataframe<-data.frame(text=unlist(sapply(twicorpus, `[`, "content")), stringsAsFactors=F) # storing corpus as data frame
  (poldat <- with(dataframe, polarity(text))) #getting polarity of the tweets
  return(poldat)
}


#Apenas por exemplo, edite a hashtag e o número de tweets para buscar
get<-fetchHashtag("#Protesto",200) # fetching tweets of Protesto

freqPlot(get) #creating frequency plots
corp<-makeCorpus(get) #creating corpus
makeWordcloud(get) #creating wordcloud
hCluster(get) #hierarchical clustering 
kMeans(get) #k-means clustering
getSentiment1<-tSentimen(get) #fetching sentiment polarity
table(getSentiment1$all$polarity>0)
plot(getSentiment1) #polarity plot
plot(getSentiment1$all$polarity,ylab="All Polarity",xlab="Documents",pch=20,cex=1,col="darkblue") #individual polarity plot
mean(getSentiment1$all$polarity)
