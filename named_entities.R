# NER
rm(list = ls())
wd <- 'C:/Users/KLN/some_r'
setwd(wd)
source('util_fun.R')
## need some java
#install.packages("rJava")
library(rJava)
# english language model from datacube
#install.packages("openNLPmodels.en",
#                 repos = "http://datacube.wu.ac.at/",
#                 type = "source")
library(NLP)
library(openNLP)
library(RWeka)

# import text
matt.v <- scan('data/kjv_books/Matthew.txt', what = 'character', sep='\n', encoding = 'UTF-8')
# use a pipe and another base function
library(dplyr)
matt.v <- readLines('data/kjv_books/Matthew.txt') %>% paste(collapse = ' ')

## build annotators for words and sentences
# NLP assumes string (not native to R)
matt.str <- as.String(matt.v)
class(matt.str)
# annotator (model) for Java library
wordanno <- Maxent_Word_Token_Annotator()
sentanno <- Maxent_Sent_Token_Annotator()
# apply and inspect annotators (first sentence then word)
matt.an <- annotate(matt.str, list(sentanno, wordanno))
head(matt.an)
tail(matt.an)
# annotate document
matt_annotated <- AnnotatedPlainTextDocument(matt.v, matt.an)
sents(matt_annotated) %>% head(2)
words(matt_annotated) %>% tail(8)

## annotate entities: person and location
personanno <- Maxent_Entity_Annotator(kind = 'person')
locationanno  <- Maxent_Entity_Annotator(kind = 'location')

# second argument for annotate()
pipelineanno <- list(sentanno, wordanno, personanno, locationanno)

matt.an <- annotate(matt.v,pipelineanno) 
# annotate document
matt_annotated <- AnnotatedPlainTextDocument(matt.v,matt.an)
# functions for extracting entities from annotated document (like words()/sents())
# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  doc.str <- doc$content # text as string string
  doc.an <- annotations(doc)[[1]] # get annotations
  if(hasArg(kind)) {
    kind.l <- sapply(doc.an$features, '[[', "kind")# extract all annotations of kind
    doc.str[doc.an[kind.l == kind]]
  } else {
    doc.str[doc.an[doc.an$type == "entity"]] # or all annotations of class entity
  }
}

###
person.v <- unique(entities(matt_annotated,'person'))
location.v <- unique(entities(matt_annotated,'location'))
#save(person.v, location.v, file = 'matt_entities.RData')


# scaling for several texts with pipes and lapply
library(NLP)
library(openNLP)
library(dplyr)

filenames.v <- Sys.glob("data/kjv_books/*.txt")# wildcard expansion
filenames.v <- filenames.v[c(60,61,57)]# Synoptic Gospels
basename(filenames.v)
# piping through the documents
texts.l <- filenames.v %>%
  lapply(readLines) %>%
  lapply(paste0, collapse = " ") %>%
  lapply(as.String)
names(texts.l) <- gsub("\\..*","",basename(filenames.v))
# list of strings
str(texts.l, max.level = 1)
class(texts.l)
class(texts.l[[1]])
# annotation 
entity.an <- function(doc, pipeline) {
  annotations <- annotate(doc, pipeline)
  AnnotatedPlainTextDocument(doc, annotations)
}


# second argument for entity.an
pipelineanno <- list(
  Maxent_Sent_Token_Annotator(),
  Maxent_Word_Token_Annotator(),
  Maxent_Entity_Annotator(kind = "person"),
  Maxent_Entity_Annotator(kind = "location")
)

synop_annotated <- texts.l %>% 
  lapply(entity.an, pipelineanno)

person.l <- synop_annotated %>%
  lapply(entities, kind = 'person') 
location.l <- synop_annotated %>%
  lapply(entities, kind = 'location') 

n1 <- location.l %>% sapply(length)
n2 <- location.l %>% lapply(unique) %>% sapply(length)
N <- synop_annotated %>% lapply(words) %>% sapply(length)
locationratio.v <- n1/N # relative frequency of location
uniqueratio.v <- n1/n2 # average use of locations
# plot relative importance of location
names(locationratio.v) <- gsub('\\..*',"",names(locationratio.v))
barplot(locationratio.v, main = 'Location Entities', xlab = 'Book', ylab = 'Relative frequency')
# geocode and map locations
library(ggmap)
library(rworldmap)
alllocations.v <- union(location.l[["Mark"]], location.l[["Matthew"]]) %>% union(location.l[["Luke"]])
locations_geocoded.df <- geocode(alllocations.v)
dev.new()
newmap <- getMap(resolution = "low")
# plot(newmap)
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)
points(locations_geocoded.df$lon, locations_geocoded.df$lat, pch = 19, col = "red")
# text(locations_geocoded.df$lon, locations_geocoded.df$lat, alllocations.v)

