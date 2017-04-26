# free memory
rm(list=ls())
gc()
#########################################################################################
setwd("~/workspace/R/")

########################################################################################
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
# install.packages('tm')
# install.packages('wordcloud')
#download.file("http://cran.cnr.berkeley.edu/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz", "Rstem_0.4-1.tar.gz")
#install.packages("Rstem_0.4-1.tar.gz", repos=NULL, type="source")
#download.file("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", "sentiment.tar.gz")
#install.packages("sentiment.tar.gz", repos=NULL, type="source")# Load libraries

library(twitteR)
library(SnowballC)

library(plyr)
library(ggplot2)
library(grid)
library(sentiment)
library(Rgraphviz)

#PRE-PROCESS DATA:
#  Text pre-processing is an important step to reduce noise from the data. Each step is discussed below 
#  convert to lower: this is to avoid distinguish between words simply on case
#  remove punctuation: punctuation can provide grammatical context which supports understanding. Often for initial analyses we ignore the punctuation
#  remove numbers: numbers may or may not be relevant to our analyses
#  remove stop words: stop words are common words found in a language. Words like for, of, are etc are common stop word
#  create document term matrix: a document term matrix is simply a matrix with documents as the rows and terms as the columns and a count of the frequency of words as the cells of the matrix
df.Tweets <- read.csv("./data/Twitter/twittesPRFnovos.csv",sep=",",header=TRUE, stringsAsFactors = F)
#df <- read.table("../input/data.csv",sep=",",header=TRUE)
df.graphs <- df.Tweets ## cópia para gerar grafos
## data frame para factor
df.Tweets$text <- factor(df.Tweets$text)

## iconv
## tweet #190
tweets.df<- df.Tweets[190, c("id", "created", "screenName", "replyToSN",
                 "favoriteCount", "retweetCount", "longitude", "latitude", "text")]

## imprime tweet #190 e faz um ajuste no texto fatiado pelo width
writeLines(strwrap(tweets.df$text[190], 60))
df.Tweets$text[190]
nrow(df.Tweets)
summary(df.Tweets)
prop.table(table(df.Tweets$favorited))

## Limpando o texto
library(tm)
treino_corpus <- Corpus(VectorSource(df.Tweets$text)) 
# ?Corpus
# ?VectorSource
length(treino_corpus)

passo1 <- tm_map(treino_corpus, content_transformer(tolower))
passo1[1]
passo1[[2]]
passo1[2:3]
inspect(passo1[2:3])
# t <- tolower("TEXT")
# t

## remove URL
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
passo2 <- tm_map(passo1, content_transformer(removeURL))
passo2[[1]][1]
passo2[[1]][1:4]

## remove pontuação
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
passo3 <- tm_map(passo2, content_transformer(removeNumPunct))

#corp <- tm_map(corp, removeNumbers)
## remove stopword em português
## pacote SnowballC
passo4 <- tm_map(passo3, removeWords, stopwords("portuguese"))
mStopword <- c(stopwords("portuguese"), "total", "consegue", "repetição","fiscalizaçãoaliz")
passo4 <- tm_map(passo4, removeWords, mStopword)
passo4[2]
passo4[[1]][1]
inspect(passo4[1:3])

## remove espaços em branco extras
passo4 <- tm_map(passo4, stripWhitespace)

## mantém uma cópia para completar mais tarde os radicais
mCorpusPasso4 <-passo4

## Stem document: palaveas ficam somente os radicais da lingua
passo5 <- tm_map(passo4, stemDocument, language = "portuguese") 
writeLines(strwrap(passo5[[190]]$content, 60))

## r refer card data mine now provid link packag cran packag
## mapreduc hadoop ad

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

passo5 <- lapply(passo5, stemCompletion2, dictionary = mCorpusPasso4)
passo5 <- Corpus(VectorSource(passo5))
writeLines(strwrap(passo5[[190]]$content, 60))
## completion always 
## r reference card data miner now provided link package cran
## package mapreduce hadoop add

## contar ferquencia palavras
wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}

n.acid <- wordFreq(mCorpusPasso4, "acid")
n.acidente <- wordFreq(mCorpusPasso4, "acidente")
cat(n.acid, n.acidente)

## reescrever palavras iguas mas não corrigidas pelo Stem
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}


passo5 <- replaceWord(passo5, "acident", "Acidente")
passo5 <- replaceWord(passo5, "acid", "Acidente")
passo5 <- replaceWord(passo5, "paraliz", "Paralização")
passo5 <- replaceWord(passo5, "fisc", "Fiscalização")
passo5 <- replaceWord(passo5, "protest", "Protesto")
passo5 <- replaceWord(passo5, "alcole", "alcoolemia")
passo5 <- replaceWord(passo5, "álcool", "Álcool")
passo5 <- replaceWord(passo5, "colisã", "Colisão")
#passo5 <- replaceWord(passo5, "vítim", "Vítima")
passo5 <- replaceWord(passo5, "pesso", "Pessoa")
passo5 <- replaceWord(passo5, "envolv", "Envolvidos")
passo5 <- replaceWord(passo5, "veic", "veículo")
#passo5 <- replaceWord(passo5, "veíc", "Veículo")
passo5 <- replaceWord(passo5, "rodov", "Rodovia")
passo5 <- replaceWord(passo5, "rodoviár", "Rodoviário")
passo5 <- replaceWord(passo5, "caminhã", "Caminhão")
#passo5 <- replaceWord(passo5, "balanco", "Balanço")

## Constroe Term Document Matrix
corp.tdm <- TermDocumentMatrix(passo5, control = list(wordLengths=c(1,Inf)))
corp.tdm

##<<TermDocumentMatrix (terms: 2368, documents: 2863)>>
##  Non-/sparse entries: 25352/6754232
##  Sparsity           : 100%
##  Maximal term length: 72
##  Weighting          : term frequency (tf)

idx <- which(dimnames(corp.tdm)$Terms %in% c("acid", "moto", "fiscal"))
as.matrix(corp.tdm[idx, 1:30])

## Topo da Frequencia de termos 
(freq.terms <- findFreqTerms(corp.tdm, lowfreq = 200))
term.freq <- rowSums(as.matrix(corp.tdm))
term.freq <- subset(term.freq, term.freq >= 200)
dfrm.freq <- data.frame(term = names(term.freq), freq = term.freq)

## plotando o gráfico de frequência da palavras
library(ggplot2)
ggplot(dfrm.freq, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Termos") + ylab("Cont") + coord_flip() + theme(axis.text = element_text(size = 7))


## Word cloud: This visualization generates words whose font size relates to its frequency.
## wordcloud(corp, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))
m <- as.matrix(corp.tdm)
word.freq <- sort(rowSums(m), decreasing = T)
library(RColorBrewer)
pal <- brewer.pal(8, 'Dark2')
#pal <- brewer.pal(9, "BuGn")[-(1:4)]

library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)


## Associações


