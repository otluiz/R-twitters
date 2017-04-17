#############################################
setwd("~/workspace/R/")

# free memory
rm(list=ls())
#install.packages(c("devtools", "rjson", "bit64", "httr"))
#install_github("twitteR", username="geoffjentry")

gc()
#############################################


install.packages("Stem")
library(sentiment)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(qdap, syuzhet, dplyr)
pacman::p_load_current_gh(c("trinker/stansent", "trinker/sentimentr"))

pres_debates2012 #nrow = 2912

tic <- function (pos = 1, envir = as.environment(pos)){
  assign(".tic", Sys.time(), pos = pos, envir = envir)
  Sys.time()
}

toc <- function (pos = 1, envir = as.environment(pos)) {
  difftime(Sys.time(), get(".tic", pos = pos, envir = envir))
}

id <- 1:2912



source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
# install.packages('tm')
# install.packages('wordcloud')
download.file("http://cran.cnr.berkeley.edu/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz", "Rstem_0.4-1.tar.gz")
install.packages("Rstem_0.4-1.tar.gz", repos=NULL, type="source")
download.file("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", "sentiment.tar.gz")
install.packages("sentiment.tar.gz", repos=NULL, type="source")# Load libraries

library(wordcloud)
library(tm)
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
df <- read.csv("./twitter/Twitter07042017.csv",sep=",",header=TRUE)
df <- read.table("../input/data.csv",sep=",",header=TRUE)
corp <- Corpus(VectorSource(df$Review)) 
corp <- tm_map(corp, tolower) 
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
# corp <- tm_map(corp, stemDocument, language = "english") 
corp <- tm_map(corp, removeWords, c("the", stopwords("portuguese"))) 
corp <- tm_map(corp, PlainTextDocument)
corp.tdm <- TermDocumentMatrix(corp, control = list(minWordLength = 3)) 
corp.dtm <- DocumentTermMatrix(corp, control = list(minWordLength = 3))

## Word cloud: This visualization generates words whose font size relates to its frequency.
wordcloud(corp, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))
