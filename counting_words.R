# word counting
rm(list = ls())

wd <- 'C:/Users/KLN/some_r'
setwd(wd)
source('util_fun.R')
load('kjv.RData')

### word frequencies

# build word (raw) frequency table
text.freqs.t <- table(tokens.fil.v)
head(text.freqs.t)
# sort list
text.freqs.t <- sort(text.freqs.t, decreasing = TRUE)
text.freqs.t[1:20]

# relative frequency table
text.freqsrel.t <-  text.freqs.t/sum(text.freqs.t)*100
text.freqsrel.t[1:20]
# plot word distribution
n <- length(text.freqs.t)
x <- 1:n
y <- as.numeric(text.freqs.t[1:n])
plot(x,y, main = "KJV meets Zipf's law", log='xy', xlab ='Rank', ylab ='Frequency' ,axes= T )


### individual word frequencies
library(SnowballC)
positive <- wordStem(c('laughter','happiness','love','laughed','excellent'),'english') # terms from labMT
negative <- wordStem(c('murder','death','kill','died','war'),'english')
sentiments.dt <- data.frame(positive = rep(0,5), negative = rep(0,5))
for (w in 1:length(positive)){
  sentiments.dt[w,1] <- text.freqs.t[positive[w]]
  sentiments.dt[w,2] <- text.freqs.t[negative[w]]
}
sentiments.dt

## token distribution analysis
time.v <- seq(1,length(tokens.v),by = 1)# alternative time.v <- 1:length(tokens.v)
# find occurrences of 
war.v <- which(tokens.v == 'war')
war.count.v <- rep(NA,length(time.v))
war.count.v[war.v] <- 1
dev.new()
plot(war.count.v, main="Dispersion plot of 'war' in KJV",
     xlab="Narrative time",ylab="war",type="h",ylim=c(0,1),yaxt='n')
# function for distributional plot
token.dist <- function(token,tokens){
  t <- 1:length(tokens)
  c <- rep(NA,length(time.v))
  c[tokens == token] <- 1
  plot(c, main=paste("Dispersion plot of",token),
       xlab="Narrative time",ylab=token,type="h",ylim=c(0,1),xaxt='n',yaxt='n')
}
par(mfrow=c(2,1))
token.dist('love',tokens.v)
l <- which(tokens.v == 'gospel'); l <- l[1];# identify new testament
abline(v=l,col = 'red',lwd = 2)
token.dist('war',tokens.v)
abline(v=l,col = 'red',lwd = 2)

### word counting with tm
library(tm)

## building a document-term matrix 

# word frequencies
books.dtm <- DocumentTermMatrix(books.cor, control = list(minWordLength = 2))
# tf-idf weighting
books.tfidf.dtm <- DocumentTermMatrix(books.cor, control = list(weighting = function(x)weightTfIdf(x,normalize = FALSE)))
books.tfidf.dtm <- DocumentTermMatrix(books.cor, control = list(weighting = weightTfIdf , minWordLength = 2))
# including n-grams
library(RWeka)
help(package = 'RWeka')
books.ng.dtm <- DocumentTermMatrix(books.cor, control=list(tokenize = NGramTokenizer))


# explore dtm
dim(books.dtm)
books.dtm$dimnames$Docs
inspect(books.dtm[1,])
inspect(books.dtm[,1])
inspect(books.dtm[1:5, 1000:1005])
# build sentiment matrix for initial books in Old Testament and New Testament
metadata.dt <- read.csv('kjv_metadata.csv',header = TRUE)
penta <- books.dtm$dimnames$Docs[which(metadata.dt[,3] == 'pentateuch')]
evang <- books.dtm$dimnames$Docs[which(metadata.dt[,3] == 'historical' & metadata.dt[,2] == 'new')]
sentiment.mat <- inspect(books.dtm[c(penta,evang),c(positive,negative)])

# find frequent terms
findFreqTerms(books.dtm, lowfreq = 500, highfreq = Inf)
