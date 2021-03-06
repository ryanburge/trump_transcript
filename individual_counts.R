huck <- subset(full, Speaker == "Huckabee")
review_text <- paste(huck$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #531


carson <- subset(full, Speaker == "Carson")
review_text <- paste(carson$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #386

graham <- subset(full, Speaker == "Graham")
review_text <- paste(graham$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #269


falwell <- subset(full, Speaker == "Falwell")
review_text <- paste(falwell$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #758

dobson <- subset(full, Speaker == "Dobson")
review_text <- paste(dobson$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #597

unidentified <- subset(full, Speaker == "Unidentified")
review_text <- paste(unidentified$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #97

floyd <- subset(full, Speaker == "Floyd")
review_text <- paste(floyd$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #109

jeremiah <- subset(full, Speaker == "Jeremiah")
review_text <- paste(jeremiah$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #99

rodriguez <- subset(full, Speaker == "Rodriguez")
review_text <- paste(rodriguez$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #79

perkins <- subset(full, Speaker == "Perkins")
review_text <- paste(perkins$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #102

shackelford <- subset(full, Speaker = "Shackelford")
review_text <- paste(shackelford$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #153

robison <- subset (full, Speaker == "Robison")
review_text <- paste(robison$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
dtm <- DocumentTermMatrix(wordCorpus)
dtm2 <- as.matrix(dtm)
rowSums(as.matrix(dtm2)) #290
