rdmTweets <- list(
  "Text Mining Tutorial http://t.co/jPHHLEGm",
  "He likes dogs r Singapore http://t.co/GPA0TyG5",
  "RDataMining: Easier Parallel Computing in R with snowfall and sfCluster http://t.co/BPcinvzK",
  "RDataMining: Tutorial: Parallel computing using R package snowfall http://t.co/CHBCyr76",
  "handling big data: Interacting with Data using the filehash Package for R http://t.co/7RB3sChx"
)

df <- as.data.frame(do.call(rbind, rdmTweets))
names(df) <- c('text')

library(tm)
# ---- build a corpus
myCorpus <- Corpus(VectorSource(df$text)) # VectorSource specifies the text source

myCorpus[[3]]$content # show text in the 3rd document

# ---- Transforming Text

myCorpus <- tm_map(myCorpus, content_transformer(tolower)) #  to lower case
myCorpus <- tm_map(myCorpus, removePunctuation) # remove punctuation
myCorpus <- tm_map(myCorpus, removeNumbers) # remove numbers
myStopwords <- c(stopwords('english'), "available", "dogs") # add two additional stopwords
myStopwords <- setdiff(myStopwords, c("r", "big")) # remove 'r' and 'big' from stopwords
myCorpus <- tm_map(myCorpus, removeWords, myStopwords) # remove stopwords
#fix up 1) differences between us and aussie english 2) general errors
myCorpus <- tm_map(myCorpus, content_transformer(gsub), pattern = 'organiz', replacement = 'organ')

# ---- Stemming Words

dictCorpus <- myCorpus # keep a copy as a dictionary for stem completion
#library("SnowballC") # for stemDocument
myCorpus <- tm_map(myCorpus, stemDocument) # stem words
inspect(myCorpus)
# myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus) # stem completion
# the following stem completion works in tm v0.6 
tm_map(myCorpus, content_transformer(function(x, d)
  paste(stemCompletion(strsplit(stemDocument(x), ' ')[[1]], d), collapse = ' ')), dictCorpus)
# fix up 1) differences between us and aussie english 2) general errors
myCorpus <- tm_map(myCorpus, content_transformer(gsub), pattern = 'organiz', replacement = 'organ')
inspect(myCorpus)
inspect(dictCorpus)

# ---- Building a Document-Term Matrix

myDtm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
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
          ,colors=brewer.pal.(10, "Set2")
)

#,colors=brewer.pal.info["Yellow",]
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
