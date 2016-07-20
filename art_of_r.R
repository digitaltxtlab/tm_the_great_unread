# introduction to R as a programming language
rm(list = ls())
wd = 'C:/Users/KLN/some_r'
setwd(wd)
dir()
drseuss.v <- scan("dr_seuss.txt", what = "character", sep=" ")

### filtering: The extraction of subsets of vectors
a.v <- c(5,2,-3,8)
b.v <- a.v[a.v*a.v > 8]
# step by step
aa.v <- a.v*a.v
idx <- aa.v > 8
a.v[idx]
# replace elements
a.v <- c(1,3,8,2,20)
a.v[a.v < 3] = 0
# filtering with which
which(a.v < 3)

## filtering matrices
a.m <- matrix(c(1:3,2:4),nrow = 3)
idx = a.m[,2] >= 3
a.m[idx,]
which(a.m >=3)# using which to extract elements
a.m[6] == a.m[3,2]# indexing methods

### control structures
# if, else
x <- 1:5
if (sample(x,1) == 5){
  print('x is equal to 5')
}else{
  print('x is less than five')
}
# for loop
for (i in 1:5){
  print(i)
}
# combining if and for
y <- rep(0,length(drseuss.v))
for (i in 1:length(drseuss.v)){
  print(drseuss.v[i])
  if (drseuss.v[i] == 'I'){
    y[i] = 1
  }else if (drseuss.v[i] == 'Sam'){
    y[i] = 2
  }else{
    y[i] = 0
  }
}
par(mfrow=c(1,2))
plot(y,xlab = 'Time')
barplot(c(sum(y == 1),sum(y == 2)), xlab = 'Word', ylab = 'Raw frequency', names.arg=c('I','Sam'))

# vectorize if, else
y = ifelse(drseuss.v == 'I',1,0)


### functions
add <- function(a,b){
  return(a+b)
}
add(2,3)
body(add)
formals(add)
environment(add)
args(add)
# apply a function to each element of a vector
a.v <- 1:5
y = sapply(a.v, add, 1)

word_dispersion <- function(x,word){
  x <- tolower(x)
  y <- ifelse(x == word,1,0)
  plot(y,xlab = 'Time')
}
word_dispersion(drseuss.v,'green')

word_dispersions <- function(x, words){
  x <- tolower(x)
  words <- tolower(words)
  y <- rep(0,length(x))
  for (i in 1:length(words)){
    y[x == words[i]] <- i
  }
  plot(y,xlab = 'Time',yaxt = "n")
  axis(2,at = 1:length(words),labels = words)
  return(y)
}
w.v <- c('Sam', 'green', 'I') 
wdisp.v = word_dispersions(drseuss.v,w.v)

## applying functions to matrix rows and columns
a.m <- matrix(c(1:3,2:4),nrow = 3)
apply(a.m, 2,sum)

### more little more on lists
a.l = list(words = c('I','am', 'Sam'), freqs = c(71,3,2))
attributes(a.l)# list tags
a.l$relfreqs <- a.l$freqs/length(drseuss.v)# add list elements
a.l$freqs <- NULL# deleting list elements

# create text concordance list
findwords <- function(tf){
  txt <- scan(tf,"")# read in the words from the file, into a vector of mode char
  wl <- list()
  for (i in 1:length(txt)){
    wrd <- txt[i] # ith word in input file
    wl[[wrd]] <- c(wl[[wrd]],i)# assign ith index to word attribute in list 
  }
  return(wl)
}
seuss_concordance.l <- findwords("dr_seuss.txt")