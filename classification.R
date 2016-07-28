# classification
rm(list = ls())
wd <- 'C:/Users/KLN/some_r'
setwd(wd)
source('util_fun.R')


input.dir <- 'C:/Users/KLN/some_r/data/nt_hist'
files.v <- dir(path = input.dir, pattern='.*txt')

# tokenize text in directory
maketext <- function(files,directory){
  text.word.l <- list() # set up empty list
  for(i in 1:length(files)){ # loop over the files in directory
    text.v <- scan(paste(directory, files[i], sep="/"), what="character", sep="\n") # read a file
    text.v <- paste(text.v, collapse=" ") # collapse lines
    text.lower.v <- tolower(text.v) # casefolding
    text.words.v <- strsplit(text.lower.v, "\\W") # tokenize
    text.words.v <- unlist(text.words.v) # transform list to vector
    text.words.v <- text.words.v[which(text.words.v!="")] # remove blanks
    text.word.l[[files[i]]] <- text.words.v # update list
  }
  names(text.word.l) <- gsub("\\..*","",files)
  return(text.word.l)
}  

text.word.l <- maketext(files.v,input.dir)
texttitle.l <- gsub("\\..*","",files.v)

# slice tokenized text in n chuncks
nchunk <- function(tokens,n){
  maxlen <- length(tokens)/n
  x <- seq_along(tokens)
  chunks.l <- split(tokens, ceiling(x/maxlen))
}

nslice.l <- lapply(text.word.l,nchunk,100)
# unlist multiple list while preserving sublist names
text.l <- unlist(nslice.l, recursive=FALSE)
# get class information
class.v <- gsub('\\..*','',names(text.l))
# create corpus from chunks
library(tm)
text.l <- lapply(text.l, paste, collapse = " ")
text.vs <- VectorSource(text.l)
text.cor <- Corpus(text.vs)
# clean and filter
text.cor <- tm_map(text.cor, removeNumbers)
text.cor <- tm_map(text.cor, removeWords, stopwords("english"))
text.cor <- tm_map(text.cor, stemDocument)
text.cor <- tm_map(text.cor, stripWhitespace)

## create document term matrix
text.dtm <- DocumentTermMatrix(text.cor)
text.dtm <- docsparse(25,text.dtm)
print(text.dtm)
# transform to matrix object
text.mat <- as.matrix(text.dtm)
rownames(text.mat) <- names(text.l)



# exclude Thomas and build data frame
idx <- which(class.v != 'Thomas')
feat1.df <- data.frame(book = class.v[idx],text.mat[idx,])
# naive bayes classifier (categorical data, but assumes independence)
library(e1071)
model.nb <- naiveBayes(book ~ ., data = feat1.df)
pred.v <- predict(model.nb, feat1.df)
# conditional posterior probabilities
predraw.v <- predict(model.nb, feat1.df,type = 'raw')
confusion.mat <- as.matrix(table(pred.v,feat1.df$book))


accuracy <- sum(diag(confusion.mat))/sum(confusion.mat)
print(accuracy)
# plot confusion
library(ggplot2)
dev.new()
plot <- ggplot(as.data.frame(confusion.mat))
plot + geom_tile(aes(x=pred.v, y=Var2, fill=Freq)) + 
  scale_x_discrete(name="Actual Class") + 
  scale_y_discrete(name="Predicted Class") + 
  scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) + 
  labs(fill="Frequency")
# chance level
table(feat1.df$book)
chance <- 100/sum(table(feat1.df$book))
# predict class of Thomas (early or late)
idx <- which(class.v == 'Thomas')
feat2.df <- data.frame(book = class.v[idx],text.mat[idx,]) 
predthom.v <- predict(model.nb, feat2.df)
plot(table(predthom.v))


predAct <- data.frame(pred.v,feat1.df$book)
prf(predAct)


##############################
#scaling with RTextTools and tm
library(RWeka)
text.dtm <- DocumentTermMatrix(text.cor, control=list(tokenize = NGramTokenizer))
text.dtm <- docsparse(5,text.dtm)
library(RTextTools)
## separate training and testing set and create a container
# random sample for testing  data from data set
trainidx.v <- 1:nrow(text.dtm)
testidx.v <- sort(sample(trainidx.v, nrow(text.dtm)*.1, replace = FALSE, prob = NULL))
trainidx.v <- sort(trainidx.v[! trainidx.v%in%testidx.v])

# change object type, create_analytics() only handles numeric
classnum.v <- as.numeric(as.factor(class.v))
  # to transform back to original
  factor(classnum.v, labels = unique(class.v))
# create container
container <- create_container(text.dtm, classnum.v, trainSize=trainidx.v,
                              testSize=testidx.v, virgin=FALSE)
# training models
mdl1.l <- train_models(container, algorithms='SVM')
mdl2.l <- train_models(container, algorithms = c('SVM','NNET','TREE') )

# Classifying data
res.df <- classify_models(container, mdl1)
head(res.df)
confusion.mat <- as.matrix(table(res.df$SVM_LABEL, container@testing_codes))
rownames(confusion.mat) <- colnames(confusion.mat) <- unique(class.v)
print(confusion.mat)
accuracy <- sum(diag(confusion.mat))/sum(confusion.mat)

# performance metrics
analytics <- create_analytics(container, results)
class(analytics)
summary(analytics)
