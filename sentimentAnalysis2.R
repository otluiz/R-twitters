# free memory
rm(list=ls())

gc()
#############################################
#setwd("/home/otluiz/workspace/CSV/review_polarity/txt_sentoken/Movie1.csv")

############################################
install.packages("RWeka")
install.packages("SnowballC")
install.packages("caret")
install.packages("rminer", dependencies = TRUE)
install.packages("kernlab")

library(tm)
library(wordcloud)
library(RWeka)
library(SnowballC)
library(rminer)
library(kernlab)
library(rpart)