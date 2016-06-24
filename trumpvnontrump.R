library(tm)
library(stringr)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)


full <- read.csv("D:/trump_transcript/transcript.csv", stringsAsFactors = FALSE)
trump <- subset(full, Speaker == "Trump")
not.trump <- subset(full, Speaker != "Trump")


review_text <- paste(trump$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency, 25)
rowSums(as.matrix(dtm2)) #2407
write.csv(frequency, file = "trump.csv")


review_text <- paste(not.trump$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency, 25)
rowSums(as.matrix(dtm2)) #3470
write.csv(frequency, file = "nottrump.csv")



