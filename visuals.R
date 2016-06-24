count <- read.csv("D:/trump_transcript/count.csv", stringsAsFactors = FALSE)

count$area.colour <- c("one", "two", "two", "two", "two", "two" , "two", "two", "two", "two", "two", "two", "two")
ggplot(count, aes(x=reorder(speaker, count), y = percent*100, fill=area.colour)) + geom_bar(stat="identity") + coord_flip() + ylab("Percentage of Words") + xlab("Speaker") +scale_fill_manual(values = c("red", "forestgreen")) + theme(legend.position="none")


count <- read.csv("D:/trump_transcript/totalcount.csv", stringsAsFactors = FALSE)

cbPalette <- c("red", "forestgreen")
ggplot(count, aes(1, count, fill=speaker)) + geom_bar(stat="identity") + theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(axis.title.x = element_blank()) + labs(fill="") + scale_fill_manual(values=cbPalette) + ylab("Word Count") 


library(RColorBrewer)
pal <- brewer.pal(9,"Reds")
pal <- pal[-(1:4)]

review_text <- paste(trump$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)

wordcloud(words = wordCorpus, scale=c(5,0.1), max.words=100, random.order=FALSE,
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

review_text <- paste(not.trump$Text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)

pal <- brewer.pal(9,"YlGn")
pal <- pal[-(1:4)]
wordcloud(words = wordCorpus, scale=c(5,0.1), max.words=100, random.order=FALSE,
          rot.per=0.35, use.r.layout=FALSE, colors=pal)



mySentiment <- get_nrc_sentiment(trump$Text)
trump <- cbind(trump, mySentiment)
sentimentTotals <- data.frame(colSums(trump[,c(3:12)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for Trump's Statements")

mySentiment <- get_nrc_sentiment(not.trump$Text)
not.trump <- cbind(not.trump, mySentiment)
sentimentTotals <- data.frame(colSums(not.trump[,c(3:12)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Other's Statements")






