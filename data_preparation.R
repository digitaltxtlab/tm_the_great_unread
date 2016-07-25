# preprocessing
rm(list = ls())
wd <- 'C:/Users/KLN/some_r'
#wd = '~/courses/au_summer_university/summer_u2016/classes/tutorials'
setwd(wd)
getwd()
source("util_fun.R")# input and parse file
### data extraction
# make data directory
dd <-paste(wd,'/data',sep="")
dir.create(dd)
# download file
filename.v = paste(dd,'/kjv.txt',sep="")
if (!file.exists(filename.v)) { download.file('http://www.gutenberg.org/cache/epub/10/pg10.txt', destfile = filename.v) }
# import text lines
text.v <- scan(filename.v, what = 'character', sep='\n', encoding = 'UTF-8')
#print(text.v)
#head(text.v)
#tail(text.v)
# separate data from metadata
start.v <- which(text.v == '*** START OF THIS PROJECT GUTENBERG EBOOK THE KING JAMES BIBLE ***')
end.v <- which(text.v == '*** END OF THIS PROJECT GUTENBERG EBOOK THE KING JAMES BIBLE ***')
metadata.v <- text.v[c(1:start.v,end.v:length(text.v))]# create metadata variable
textlines.v <- text.v[(start.v+2):(end.v-2)]# remove metadata and title/end
head(textlines.v)
tail(textlines.v)
print(length(text.v))
#collapse lines
text.v <- paste(textlines.v, collapse = " ")
length(text.v)

### preprocesing

## casefolding
text.v <- tolower(text.v)

## tokenization
tokens.l <- strsplit(text.v, '\\W')# split on all non-alphanumeric characters with regex meta character
class(tokens.l)
str(tokens.l)
tokens.l[[1]][1:20]
# names(tokens.l) <- 'token'
# tokens.l$token[100]
# alternate for other character systems
# tokensalt.l <- strsplit(text.v, '\\s')# split on whitespace characters


# the love-hate ratio of KJV
lhr <- sum(tokens.l[[1]] == 'love')/sum(tokens.l[[1]] == 'hate')

# transform list to vector
tokens.v <- unlist(tokens.l)
tokens.v[1:25]# notice blanks and numerals

## filtering
# remove blanks (or any other token)
idx <- which(tokens.v!="")
tokens.v <- tokens.v[idx]
tokens.v[1:25]
n1 <- length(tokens.v)

# remove numerals
idx <- grep("\\D",tokens.v)# Not a digit [^0-9]
tokens.fil.v <- tokens.v[idx]
tokens.fil.v[1:25]

# remove stop words
length(tokens.fil.v)
stopword.v = unlist(lapply(read.csv("stoplist.csv"),as.character),use.names=FALSE)
for (w in 1:length(stopword.v)){
  idx <- which(tokens.fil.v!=stopword.v[w])
  tokens.fil.v <- tokens.fil.v[idx]
}
length(tokens.fil.v)
tokens.fil.v[1:25]

# stem words with porter stemmer
library(SnowballC)
getStemLanguages()
tokens.fil.v <- wordStem(tokens.fil.v,'english')
tokens.fil.v[1:25]

# data reduction from preprocessing
n2 <- length(tokens.fil.v)
print(n2/n1)

# update the love-hate ratio
lhr[2] <- sum(tokens.fil.v == 'love')/sum(tokens.fil.v == 'hate')

par(mfrow = c(2,1))
barplot(c(n1,n2), main = 'data reduction', names.arg=c('Before','After'))
barplot(lhr, main = 'Love-Hate Ratio in KJV', names.arg=c('Before','After'))

# save tokenized and filtered data
save(filename.v,tokens.v,tokens.fil.v,file = 'kjv.RData')

## synonym substitution based on WordNet database
load('kjv.RData')
library("wordnet")
setDict("C:/Program Files (x86)/WordNet/2.1/dict")# set dictionary
term <- 'joy'
syn.v <- synonyms(term,'NOUN')
# function for substituting synonyms
syno_replace <- function(term,tokens){
  synos <- synonyms(term,'NOUN')
  for (i in 1:length(synos)){
    idx <- which(tokens == synos[i])
    tokens[idx] <- term
  }
  return(tokens)
}
length(which(tokens.v == term))
tokens.v <- syno_replace(term,tokens.v)
length(which(tokens.v == term))

# similar for strings
str <- 'a man is prepared for warfare and going to war'
gsub('warfare','war',str)
syno_str_replace <- function(term,str){
  synos <- synonyms(term,'NOUN')
  for (i in 1:length(synos)){
    str <- gsub(synos[i],term,str)
  }
  return(str)
}
str <- syno_str_replace('war',str)
nchar(text.v)
text.v <- syno_str_replace('joy',tolower(text.v))
nchar(text.v)

###  preprocessing with tm (& NLP) package(s)
library(tm)
help(package = tm)

# [back to] line 30
text.v <- paste(textlines.v, collapse = " ")

# convert to corpus
text.vs <- VectorSource(text.v)# create a corpus from character vectors
text.cor <- Corpus(text.vs)
print(text.cor)

# preprocess
#text.cor <- tm_map(text.cor, PlainTextDocument)
text.cor <- tm_map(text.cor, content_transformer(tolower))
text.cor <- tm_map(text.cor, removePunctuation)
text.cor <- tm_map(text.cor, removeNumbers)
text.cor <- tm_map(text.cor, removeWords, stopwords("english"))
text.cor <- tm_map(text.cor, stemDocument)
text.cor <- tm_map(text.cor, stripWhitespace)
# visualize
library(wordcloud)
dev.new()
wordcloud(text.cor,scale=c(3,1),max.words=50,random.order=FALSE,rot.per=0.40, use.r.layout=FALSE)

# append corpus to save file
resave(text.cor, file = 'kjv.RData')

# scale to multiple documents with tm
library(tm)
sessionInfo()# check attached packages
dd = "C:/Users/KLN/some_r/data/kjv_books";
setwd(dd)
# import plain text files in the directory dd containing Latin (lat) texts
books.cor  <- Corpus(DirSource(dd, encoding = "UTF-8"), readerControl = list(language = "lat"))
names(books.cor) <- gsub("\\..*","",names(books.cor))# remove ending
filenames <- names(books.cor)
# view documents
books.cor[[2]]$content
books.cor[[2]]$meta
# preprocess
books.cor <- tm_map(books.cor, PlainTextDocument)
books.cor <- tm_map(books.cor, content_transformer(tolower))
books.cor <- tm_map(books.cor, removePunctuation)
books.cor <- tm_map(books.cor, removeNumbers)
books.cor <- tm_map(books.cor, removeWords, stopwords("english"))
books.cor <- tm_map(books.cor, stemDocument)
books.cor <- tm_map(books.cor, stripWhitespace)

# remove sparse items

### add metadata
names(books.cor) <- filenames
# metadata at corpus level w. user-defined tags
meta(books.cor, type = 'corpus')
meta(books.cor, tag = 'collection', type = 'corpus') <- "bible" 
meta(books.cor, tag = 'version', type = 'corpus') <- "kjv" 
meta(books.cor, type = 'corpus')

# metadata at document level
meta(books.cor, type = 'local')
books.cor[[2]]$meta$author <- 'paul'
books.cor[[2]]$meta$description <- 'letter'
books.cor[[2]]$meta$language <- 'english'
books.cor[[2]]$meta$origin <- 54

# add external metadata at document level
tmp <- read.csv('kjv_metadata.csv',header = TRUE)
head(tmp)
for (i in 1:length(books.cor)){
  books.cor[[i]]$meta$heading <- as.character(tmp$filename[[i]])# pre-defined tag
  books.cor[[i]]$meta$collection <- as.character(tmp$collection[[i]])# user-defined tags
  books.cor[[i]]$meta$class <- as.character(tmp$class[[i]])
}
meta(books.cor, type = 'local', tag = 'heading')
meta(books.cor, type = 'local', tag = 'collection')

# predefined attributes
# filter on metadata
idx <- meta(books.cor, type = 'local',"collection") == 'new'
nt.cor <- books.cor[idx]# new testament

# append corpus to save file
resave(books.cor, file = 'kjv.RData')