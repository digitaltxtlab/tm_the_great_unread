# word associations

rm(list = ls())
wd <- 'C:/Users/KLN/some_r'
setwd(wd)
source('util_fun.R')
load('kjv.RData')

# recursive import text function
input.dir <- "data/kjv_books"
files.v <- dir(input.dir,"\\.txt$")
maketext.l <- function(files.v, input.dir){
  text.word.l <- list() # set up empty list
  for(i in 1:length(files.v)){ # loop over the files.v in input.dir
    text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n") # read a file
    text.v <- paste(text.v, collapse=" ") # collapse lines
    text.lower.v <- tolower(text.v) # casefolding
    text.words.v <- strsplit(text.lower.v, "\\W") # tokenize
    text.words.v <- unlist(text.words.v) # transform list to vector
    text.words.v <- text.words.v[which(text.words.v!="")] # remove blanks
    text.word.l[[files.v[i]]] <- text.words.v # update list
    # add more preprocessing steps
  }
  return(text.word.l)
}
text.l <- maketext.l(files.v,input.dir)
names(text.l) <- gsub("\\..*","",names(text.l))

text.l

# function for displaying file names of list corpus
showfiles <- function(filename.v){
  for(i in 1:length(filename.v)){
    cat(i, filename.v[i], "\n", sep=" ")# concatenate and print
  }
}



# keyword in context search
kwicsearch <- function(data.l){
  showfiles(names(data.l))
  fileid <- as.numeric(readline('What file would you like to examine? Enter file number: \n'))
  context <- as.numeric(readline('How large a span would you like? Enter a number for LHS & RHS: \n'))# left/right hand side
  keyword <- tolower(readline('Enter a keyword: \n'))
  hits.v <- which(data.l[[fileid]] == keyword)
  output.l <- list()
  if(length(hits.v > 0)){
    for(h in 1:length(hits.v)){
      start <- hits.v[h] - context
      if(start < 1){
        start <- 1
      }
      end <- hits.v[h]+context
      cat(data.l[[fileid]][start:end], '\n')
      output.l[[h]] <- paste(data.l[[fileid]][start:end], collapse = ' ')
    }
    
  }
  return(output.l)
}

kwicsearch(text.l)

### one collocation on a node in text
# get a text from the list corpus
atext.v <- paste(text.l[[which(names(text.l) == 'Matthew')]], collapse = ' ')
# a bit more quick and dirty preprocessing
library(tm)
atext.cor <- Corpus(VectorSource(atext.v))
atext.cor <- tm_map(atext.cor, removeNumbers)
#atext.cor <- tm_map(atext.cor, removeWords, stopwords('english'))
#atext.cor <- tm_map(atext.cor, stemDocument)

# use n-gram tokenizer
library(RWeka)
span <- 5 # word span on either side (LHS/RHS)
span1 <- 1+span*2 # window size
ngramtokenizer <- function(x) NGramTokenizer(x, Weka_control(min = span1, max = span1))
tdm <- TermDocumentMatrix(atext.cor, control = list(tokenize = ngramtokenizer))
inspect(tdm)

# set node word and find ngrams with node
word <- 'hate' 
nodengrams <- tdm$dimnames$Terms[grep(word, tdm$dimnames$Terms)]

# sort out ngrams that does not have the word in the middle (remove dublicated)
nodengrams <- nodengrams[sapply(nodengrams, function(i) {
  tmp <- unlist(strsplit(i, split=" "))# tokenize
  tmp <- tmp[length(tmp) - span]# middle word
  tmp} == word)]# is middle the node
# find collocate
word1 <- 'love'
nodengrams2 <- nodengrams[grep(word1, nodengrams)]
# number of collocations
length(nodengrams2)

# calculate pointwise mutual information
A <- length(which(text.l[[which(names(text.l) == 'Matthew')]] == word))
B <- length(which(text.l[[which(names(text.l) == 'Matthew')]] == word1))
AB <- length(nodengrams2)
N <- length(text.l[[which(names(text.l) == 'Matthew')]])
span <- 5
# Church and Hanks' association ratio
MI <- log2((AB/N)/(A/N*B/N))
# ~ 
MI <- log2((AB*N)/(A*B))
# alternative (more conservative)
MI <- log10((AB*N)/(A*B*span))/log10(2)


##### a slightly different take on associations that scale with tm
rm(list = ls())
library(tm)
dd = "C:/Users/KLN/some_r/data/kjv_books";
books.cor  <- Corpus(DirSource(dd, encoding = "UTF-8"), readerControl = list(language = "lat"))
names(books.cor) <- gsub("\\..*","",names(books.cor))# remove ending
filenames <- names(books.cor)
books.cor <- tm_map(books.cor, PlainTextDocument)
books.cor <- tm_map(books.cor, content_transformer(tolower))
books.cor <- tm_map(books.cor, removePunctuation)
books.cor <- tm_map(books.cor, removeNumbers)
books.cor <- tm_map(books.cor, removeWords, stopwords("english"))
#books.cor <- tm_map(books.cor, stemDocument)
books.cor <- tm_map(books.cor, stripWhitespace)
books.dtm <- DocumentTermMatrix(books.cor)
books.dtm$dimnames$Docs <- filenames

l.v <- as.vector(inspect(books.dtm[,'love']))
h.v <- as.vector(inspect(books.dtm[,'hate']))
cor(l.v,h.v)
###
help(package = 'tm')
assoc.l <- findAssocs(books.dtm, c('love','hate'), c(.46,.8))
assoc.l[[1]]
assoc.l[[2]]

# correlation of word vectors (highest correlation)
books.mat <- as.matrix(books.dtm)
dim(books.mat)
rownames(books.mat) <- filenames

love.v <- books.mat[,'love']
hateth.v <- books.mat[,'hateth']
print(cor(love.v,hateth.v, method = 'pearson'))


hate.v <- books.mat[,'hate']
print(cor(love.v,hate.v, method = 'pearson'))

terms <- c('love','loveth','hate','hateth')
# euclidean length between two vectors
euclid.dst <- dist(t(books.mat[,terms])) # transpose matrix

# cosine similarity between words to overcome effects of document length in document clustering
install.packages("lsa")
library(lsa)
?cosine
cosine(love.v,hate.v) ==  (hate.v%*%love.v)/(sqrt(sum(hate.v^2))*sqrt(sum(love.v^2)))
cosine.mat <- cosine(books.mat[,terms])
# cluster for visualization
hc.dst <- hclust(as.dist(cosine.mat))# apply clustering 
plot(hc.dst)# plot dendrogram
# or plot principal components
plot(prcomp(cosine.mat)$x)
text(prcomp(cosine.mat)$x[,1],prcomp(dist.mat)$x[,2],rownames(dist.mat))