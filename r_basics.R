# introduction to basic use of R

### navigating directories
getwd()# current working directory
dir()# files in directory
wd = 'C:/Users/KLN/some_r'# change to your path and make course directory: paste(getwd(),'/tmgu',sep = '')
# wd = 'C:/Users/KLN/Documents/courses/au_summer_university/summer_u2016/classes/tutorials'
setwd(wd)

### use R for basic calculations
2+3
3-2
2*3
3/2
# R is a functional language (avoid explicit iteration), express iterative behavior implicitly
"+"(2,3)
3-2 # press ENTER -> "+" dcontinuation character in console
3-     2 # spaces are ignored
2+3;3-2 # multiple commands on same line
# operator precedence (and logical operators)
2+3*4 == 2+(3*4)
2+3*4 != (2+3)*4
# exponentiation
3^2 == 3*3
2^3 == 2*2*2
# some additional operators
sqrt(3^2)
abs(-2)
log(3) # base e
log(exp(3))
log(3, base = 10) # base 10

### declarations - storing objects in the R environment

## vector: an vector/array of elements where all elements must be same primitive data type

# vector types: numeric, character, logical, factor 
x.num <- c(1,3,5, 5, 1); print(x.num)
x.char <- c('All hope abandon,', 'ye who enter here!'); print(x.char)
x.log <- x.num == 5; print(x.log)
x.fac <- as.factor(x.num); print(x.fac)

# numeric
a.vec <- 2+3# scalar
mode(a.vec)# check object data type
print(a.vec)
a.vec# shorthand
print(a.Vec)# case sensitive

twoelement.vec <- c(2,3)# combine two elements in longer array
length(twoelement.vec)# length of array
#2element.vec <- c(2,3) # numbers cannot start a variable name
a2element.vec <- c(2,3)
threeelement.vec <- c(twoelement.vec,1)

# vector indexing
threeelement.vec[1]
threeelement.vec[3]
threeelement.vec[0]# notice that R use one-based indexing
threeelement.vec[-1]# all except 1
threeelement.vec[-1:-3]

# character strings
str.vec <- 'this is a character string'
mode(str.vec)
length(str.vec)
str2.vec <- c('this is a character string', 'this is another string')
length(str2.vec)
str3.vec <- c(str2.vec, 'and yet another') # add element
strcoer.vec(a.vec,str.vec)#  numeric values are being coerced into character strings when the two vectors are combined

## Data structures: matrix, list, data frame

x.mat <- matrix(data = c(1,2,3,3,2,1),nrow = 2); print(a.mat)
x.ls <- list(x.num,x.char); print(x.ls)
x.dt <- data.frame(x.num[1:2],x.char); print(x.dt)


## matrix: n-dimensional arrays (vector with dimensions added to it)
a.mat <- matrix(c(1,3,4,2),nrow = 2)
a.mat <- rbind(c(1,4),c(3,2))
mode(a.mat)
class(a.mat)
a.matprint(a.mat)
length(a.mat)# number of elements
nrow(a.mat)# number of rows
ncol(a.mat)# number of columns
dim(a.mat)# dimensions
dimnames(a.mat) <- list(c('doc1','doc2'), c('term1','term2')) # dimensions names
print(a.mat)

# matrix indexing
a.mat[3] == a.mat[1,2]
a.mat[2,]# row two
a.mat[,1]# column one 

## list:  a structure that may contain object of any other types. 
a.ls = list(somechar = c('term1','term2', 'term3'), somenum = c(2,3,4))
print(a.ls)
a.ls[[2]]# index list elements
a.ls$somechar# tags
a.ls$somechar == a.ls[[1]]
a.ls[[1]][3]# index members
mode(a.ls)
mode(a.ls$somenum)
str(a.ls)# print internal structure

## data frames: table with variables in columns and cases in rows (share properties of matrices and lists)
  # great for metadata
a.dt <- data.frame(author = c('Chaucer','Shakespeare','Austen'), born = c(1343,1564,1775))
print(a.dt)
a.dt[[1]]# index
a.dt$author# tag
a.dt$author[3]
a.dt[,1]# matrix
str(a.dt)
class(a.dt)

## logical operators and vectors
# exactly equal to
a.vec <- c(1,3,5,5,1,2)
idx <- a.vec == 5 # indexing
a.vec[idx]
a.vec[a.vec == 5]
# not equal to
a.vec != 5
# less than
a.vec < 5
# greater than
a.vec > 5
# logical indexing in character vectors
a.str <- c('Andersen','Kierkegaard','Grundtvig')
a.str == 'Grundtvig'# also for characters
drseuss <- strsplit('I am Daniel, I am Sam, Sam I am',' '); print(sum(drseuss[[1]] == 'I'))
plot(drseuss[[1]] == 'I',type = 'b', xlab = 'Time', ylab = 'Occurrences of I')

### save and remove variables
save.image('some_image.RData')# save session workspace
ls()# show workspace
save(a.dt, a.mat, file = 'some_data.RData')# save specific variables
rm(a.dt,a.mat)# remove specific variables
rm(list = ls())# remove all content of workspace
load("some_data.RData")# load data file
load("some_image.RData")

### getting help
?save
help(save)
example(save)
help(package="tm")
help.search("tokenize")
# if you have loaded a package namespace and attached it to the search list
library(tm)
ls('package:tm')# list content 
