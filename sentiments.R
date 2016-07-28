shell.exec('https://www.youtube.com/watch?v=oP3c1h8v2ZQ')
# sentiment analysis
rm(list = ls())
wd <- 'C:/Users/KLN/some_r'
setwd(wd)
source('util_fun.R')

###### word level lexicons/dictionaries 
# handheld with some help
matt.v <- paste(scan('data/kjv_books/Matthew.txt', what = 'character', sep='\n', encoding = 'UTF-8'), collapse = " ")
# sentence tokenizer
library(NLP)
library(openNLP)
token_sent <- function(text, lang = "en") {
  sentannotator <- Maxent_Sent_Token_Annotator(language = lang)
  text <- as.String(text)# convert to string
  sentbound <- annotate(text, sentannotator)
  sentences <- text[sentbound]# extract sentences
  return(sentences)# return sentences
}
# sentiment function
lexicon_scr <- function(sentences,lexicon){
  token_word <- strsplit(tolower(sentences), "[^A-Za-z']+")# tokenize sentences
  sentiment.mat = matrix()
  for(i in 1:length(token_word)){
    tmp <- lexicon$value[which(lexicon$word %in% token_word[[i]])]# valence
    w <- length(tmp)# number of words
    if (length(tmp) > 0){
      sentiment.mat[i] <- sum(tmp)/w}
    else{sentiment.mat[i] = 0}
  }
  # sentiment.mat <- TTR::SMA(sentiment.mat,n = 10)# optional smoothing
  return(sentiment.mat)
}
# extract sentences
sent.ch <- token_sent(matt.v)
# import sentiment lexicon
afinn.dt <- read.table('AFINN-111.txt', header = FALSE, sep = '\t',quote = "\"")
names(afinn.dt) <- c('word','value')
# test the sentiment code
test.v <- c('I love whales. I hate Ahab because he is the epitome of whaling')
test.ch <- token_sent(test.v)
print(lexicon_scr(test.ch,afinn.dt))
# run on matt
mattsentiment.v <- lexicon_scr(sent.ch,afinn.dt)
par(mfrow = c(3,1))
hist(mattsentiment.v)
plot(mattsentiment.v,type = 'l', xlab = 'Narrative Time', ylab = 'Sentiment')
plot(TTR::SMA(mattsentiment.v,10),type = 'l', xlab = 'Narrative Time', ylab = 'Sentiment')

# aesthetics
library(ggplot2)
mattsentiment.df <- data.frame(line = 1:length(mattsentiment.v), sentiment = TTR::SMA(mattsentiment.v,10))
dev.new()
ggplot(data = mattsentiment.df, aes(x = line, y = sentiment)) +
  geom_bar(stat = "identity", colour ="#FF9999")+
  theme_minimal() +
  xlab("Narrative Time (line)")+
  ylab("Sentiment") +
  labs(title = expression(paste("Sentiment in ", italic("Matthew")))) 


### with Syuzhet library
library(syuzhet)
library(tm)
library(qdap)#qual
ls("package:syuzhet")
ls("package:qdap")
help(package = 'qdap')
# tokenize at sentence level
text_sent <- get_sentences(matt.v)
head(text_sent)
# AFINN sentiment lexicon
text_afinn <- get_sentiment(text_sent, method = 'afinn')
# explore
text_sent[which(text_afinn == max(text_afinn))]
text_sent[which(text_afinn == min(text_afinn))]
text_sent[which(text_afinn > (mean(text_afinn)+sd(text_afinn)*2))]
text_sent[which(text_afinn < (mean(text_afinn)-sd(text_afinn)*2))]

dev.new()
par(mfrow = c(2,2))

hist(text_afinn)

plot(text_afinn,type = 'l')
#chunck text
text_afinn_val <- get_percentage_values(text_afinn, bin = 100)
hist(text_afinn_val)
plot(text_afinn_val,type = 'l')

# the NRC lexicon
matt_nrc <- get_nrc_sentiment(text_sent)
# several sentiment factors
head(matt_nrc)
# explore
text_sent[which(matt_nrc$fear > 4)]

# bit more efficient with dplyr and ggplot
library(dplyr)
library(stringr)
process_sentiment <- function (atext, amethod) {
  chunkedtext <- data_frame(x = atext) %>% 
  group_by(linenumber = ceiling(row_number() / 10)) %>% 
  summarize(text = str_c(x, collapse = " "))
  mySentiment <- data.frame(cbind(linenumber = chunkedtext$linenumber, 
                                sentiment = get_sentiment(chunkedtext$text, method = amethod)))
}
matt_sentiment.df <- rbind(process_sentiment(text_sent,"afinn") %>% mutate(method = "AFINN"),
               process_sentiment(text_sent,"bing") %>% mutate(method = "Bing et al"),
               process_sentiment(text_sent,"nrc") %>% mutate(method = "NRC"))

library(ggplot2)
dev.new()
ggplot(data = matt_sentiment.df, aes(x = linenumber, y = sentiment, fill = method)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~method, nrow = 3) +
  theme_minimal() +
  ylab("Sentiment") +
  labs(title = expression(paste("Sentiment in ", italic("Matthew")))) 

### sentiment as proxy for plot structure
# fft transformation
afinn_fft <- get_transformed_values(text_afinn)
dev.new()
par(mfrow = c(2,1))
plot(text_afinn_val, type = 'l')
plot(afinn_fft, type = 'l')
text_sent_100[which(afinn_fft == min(afinn_fft))]
# discrete cosine transformation
afinn_cos <- get_dct_transform(text_afinn)
dev.new()
par(mfrow = c(2,1))
plot(text_afinn_val, type = 'l')
plot(afinn_cos, type = 'l')
bins = 100
text_sent_100 <- slice_text(text_sent,bins)
text_sent_100[which(afinn_cos == max(afinn_cos))]
text_sent_100[which(afinn_cos == min(afinn_cos))]
# plot comparison
bible_fft <- get_transformed_values(get_sentiment
                                    (get_sentences(paste(scan('data/kjv.txt', 
                                                              what = 'character', sep='\n', encoding = 'UTF-8'), collapse = " ")),method = 'afinn'))
koran_fft <- get_transformed_values(get_sentiment
                                      (get_sentences(paste(scan('data/koran.txt', 
                                                                what = 'character', sep='\n', encoding = 'UTF-8'), collapse = " ")),method = 'afinn'))
dev.new()
par(mfrow = c(2,1))
plot(bible_fft, type = 'l', main = 'Bible, KJV' ,xlab = 'Narrative time', ylab = 'Sentiment',col = 'red',lwd = 3)
plot(koran_fft, type = 'l', main = 'Koran, Arberry Translation', xlab = 'Narrative time', ylab = 'Sentiment',col = 'red',lwd = 3)
#####
library(quanteda)

### scaling with tm
library(tm)
dd = "C:/Users/KLN/some_r/data/kjv_books";
books.cor  <- Corpus(DirSource(dd, encoding = "UTF-8"), readerControl = list(language = "lat"))
names(books.cor) <- gsub("\\..*","",names(books.cor))# remove ending
filenames <- names(books.cor)
books.cor <- tm_map(books.cor, PlainTextDocument)
books.cor <- tm_map(books.cor, content_transformer(tolower))
books.cor <- tm_map(books.cor, removePunctuation)
books.cor <- tm_map(books.cor, removeNumbers)
books.cor <- tm_map(books.cor, stripWhitespace)
names(books.cor) <- filenames
# sentiment for each document
afinncorpus <- function(corpus){
  sent <- rep(0,length(corpus))
  for(i in 1:length(corpus)){
    sent[i] <- get_sentiment(paste(corpus[[i]]$content, collapse = " "),method = 'afinn')
  }
  return(sent)
}
sent.v <- afinncorpus(books.cor) 
dev.new(); barplot(sent.v, main="KJV sentiments", horiz=TRUE)
# use metadata
tmp <- read.csv('kjv_metadata.csv',header = TRUE)
head(tmp)
for (i in 1:length(books.cor)){
  books.cor[[i]]$meta$heading <- as.character(tmp$filename[[i]])# pre-defined tag
  books.cor[[i]]$meta$collection <- as.character(tmp$collection[[i]])# user-defined tag
}
nt.cor <- books.cor[meta(books.cor, "collection") == 'new']# new testament
old.cor <- books.cor[meta(books.cor, "collection") == 'old']# new testament
nt.sent.v <- afinncorpus(nt.cor) 
ot.sent.v <- afinncorpus(old.cor)
par(mfrow = c(1,2)); 
barplot(nt.sent.v, main="KJV NT", horiz=TRUE); abline(v = mean(nt.sent.v), col = 'red')
barplot(ot.sent.v, main="KJV OT", horiz=TRUE); abline(v = mean(ot.sent.v), col = 'red')
###### sentiment classifier
## annotate sentences with with class valence and split in training and test set
# training set
pos_sent <- rbind(c('I love text mining','positive'), c('Melville is amazing','positive'), c('I feel great today','positive'),
                  c('I am excited about data','positive'), c('She is my best friend','positive'))
neg_sent <- rbind(c('I do not like text mining','negative'), c('Melville is terrible','negative'), c('I feel tired today','negative'),
                  c('I am not interested in data','negative'),c('She is my adversary','negative'))
# test set
test_sent <- rbind(c('I feel love for him','positive'),c('George is my friend','positive'),c('I am not tired','positive'),
                   c('do not like him','negative'),c('your review is terrible','negative'))
sent <- rbind(pos_sent, neg_sent, test_sent)
print(sent)
library(RTextTools) # quick and dirty text classification that use tm
library(e1071) # extensive stats library for Naive Bayes algorithm
dtm = create_matrix(sent[, 1], language = "english", removeStopwords = FALSE, 
                       removeNumbers = TRUE, stemWords = FALSE, tm::weightTfIdf)
dtm.mat = as.matrix(dtm)
sentiment.class = naiveBayes(dtm.mat[1:10, ], as.factor(sent[1:10, 2]))
predict.f = predict(sentiment.class, dtm.mat[11:15, ])
## diagnostics
# confusion matrix
table(sent[11:15, 2], predict.f)
# accuracy
recall_accuracy(sent[11:15, 2], predict.f)
