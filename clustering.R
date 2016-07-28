### unsupervised learning: clustering
rm(list = ls())
wd <- 'C:/Users/KLN/some_r'
setwd(wd)
source('util_fun.R')
library(tm)
library(RWeka)



### partioning words in sentences ~ association mining
##
matt.v <- paste(scan('data/kjv_books/Matthew.txt', what = 'character', sep='\n', encoding = 'UTF-8'), collapse = " ")
# sentence tokenizer
token_sent <- function(text, lang = "en") {
  sentannotator <- openNLP::Maxent_Sent_Token_Annotator(language = lang)
  text <- NLP::as.String(text)# convert to string
  sentbound <- NLP::annotate(text, sentannotator)
  sentences <- text[sentbound]# extract sentences
  return(sentences)# return sentences
}
sent.v <- token_sent(matt.v)
sent.cor <- Corpus(VectorSource(sent.v))
sent.cor <- tm_map(sent.cor, PlainTextDocument)
sent.cor <- tm_map(sent.cor, content_transformer(tolower))
sent.cor <- tm_map(sent.cor, removePunctuation)
sent.cor <- tm_map(sent.cor, removeNumbers)
sent.cor <- tm_map(sent.cor, removeWords, stopwords("english"))
sent.cor <- tm_map(sent.cor, stripWhitespace)
# load keyword types
load('matt_entities.RData')
print(person.v)
# make dictionary
dict.l = tolower(c('Christ','God','Jesus','John','John Baptist','Lord','Mary','Mary Magdalene', 'Moses','Satan','Spirit'))
# build sentence-term matric with dictionary and n-grams tokenization
sent.mat <- as.matrix(TermDocumentMatrix(sent.cor, control=list(dictionary = dict.l, tokenize = NGramTokenizer)) )
head(sent.mat)
# prune matrix (at least one keyword in sentence)
sent.mat <- sent.mat[,which(colSums(sent.mat) > 0)]

## partional prototype-based exclusive clustering: k-means with euclidean distance*
# for reproducibility 
set.seed(1234)
# length-normalize the vectors (Manning, p. 121)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
sentnorm.mat <- norm_eucl(sent.mat)
# k number of groups
k = 3
sent.cl <- kmeans(sentnorm.mat, k)
# cluster object
sent.cl
# classification
sent.cl$cluster
# goodness of the classification
as.numeric(sent.cl[6])/as.numeric(sent.cl[3])# %%% BSS/TSS
plot(prcomp(sentnorm.mat)$x, col=sent.cl$cl)
text(prcomp(sentnorm.mat)$x[,1],prcomp(sentnorm.mat)$x[,2],rownames(sentnorm.mat))
# plot clusters using the first 2 principal components
x <- prcomp(sentnorm.mat)$x[,1]; y <- prcomp(sentnorm.mat)$x[,2]; names <- capname(rownames(sentnorm.mat))
cols = as.double(sent.cl$cluster)
plot(x, y, type='p', pch=20, col=cols, cex = 2,xlab='Comp.1', ylab='Comp.2', xlim = c(-.4, 1), ylim = c(-.7,.5))
text(x, y, names, col=cols, cex=.8, pos=4)

## agglomerative hierarchical overlapping clustering
library(proxy)
sent.dist <- dist(sentnorm.mat, method="cosine")# use dot product and euclid dist as length normalizer
sent.hc <- hclust(sent.dist, method="average")
# plot with dendrogram
plot(sent.hc)
cl <- cutree(sent.hc, 3) # prune tree in three paths 
print(cl)

### validation
library('clValid')
# internal
intern <- clValid(sent.mat, 2:6, clMethods=c("hierarchical","kmeans"),validation="internal")
summary(intern)
optimalScores(intern)# only view optimal values
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(intern, legend=FALSE)
plot(nClusters(intern),measures(intern,"Dunn")[,,1],type="n",axes=F, xlab="",ylab="")
legend("center", clusterMethods(intern), col=1:9, lty=1:9, pch=paste(1:9))
par(op)
# stability
stab <- clValid(sent.mat, 2:6, clMethods=c("hierarchical","kmeans"),validation="stability")
optimalScores(stab)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(stab, measure=c("APN","AD","ADM"),legend=FALSE)
plot(nClusters(stab),measures(stab,"APN")[,,1],type="n",axes=F,xlab="",ylab="")
legend("center", clusterMethods(stab), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

### scaling to document clustering with tm
load('kjv.RData')

# extract NT subcorpus using metadata
collection.v <- meta(books.cor, type = 'local',"collection")
books.cor <- books.cor[collection.v == 'new']
class.v <- unlist(meta(books.cor, type = 'local',"class"))

# build document term matrix
books.dtm <- DocumentTermMatrix(books.cor)
print(books.dtm)
# remove sparse terms by setting minimum representation in documents
docsparse <- function(mindocs,dtm){
  n = length(row.names(dtm))
  sparse <- 1 - mindocs/n;
  dtmreduce <- removeSparseTerms(dtm, sparse)
  return(dtmreduce)
}
books.dtm <- docsparse(2,books.dtm)
print(books.dtm)
# transform to matrix
books.mat <- as.matrix(books.dtm)

## kmeans
books.mat <- norm_eucl(books.mat)

# graphical approach to determining number of clusters: plot of total within-groups sums of squares against number of k and find knee
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
max_k = 10
dev.new()
wssplot(books.mat,nc = max_k) 

# 3 sub-groups or clusters
k = 3
books.cl <- kmeans(books.mat, k)
# classification
books.cl$cluster
x <- prcomp(books.mat)$x[,1]; y <- prcomp(books.mat)$x[,2]; names <- capname(rownames(books.mat))
cols = as.double(books.cl$cluster)
dev.new()
par(mfrow = c(1,2))
plot(x, y, type='p', pch=20, col=cols, cex = 2,xlab='Comp.1', ylab='Comp.2', xlim = c(-.4,.7),ylim = c(-.7,.3))
text(x, y, names, col=cols, cex=.8, pos=4)
# add metadata
plot(x, y, type='p', pch=20, col=cols, cex = 2,xlab='Comp.1', ylab='Comp.2',xlim = c(-.4,.7),ylim = c(-.7,.3))
text(x, y, class.v, col=cols, cex=.8, pos=4)


## agglomerative hierarchical overlapping clustering
library(proxy)
books.dist <- dist(books.mat, method="cosine")# use dot product and euclid dist as length normalizer
books.hc <- hclust(books.dist, method="average")
# plot with dendrogram
dev.new()
plot(books.hc)
print(cl)
# same level labels
plot(books.hc, hang = -1)
# changing shape by using dendrogram object
books.den <- as.dendrogram(books.hc)
plot(books.den,'triangle')
library(ape)
dev.new()
plot(as.phylo(books.hc), cex = 0.9)
dev.new()
plot(as.phylo(books.hc), type = "unrooted", cex = 1)
dev.new()
plot(as.phylo(books.hc), type = "fan")





