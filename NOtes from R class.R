<<<<<<< Updated upstream
## data frame
=======
 ## data frame
>>>>>>> Stashed changes
 #data frames also have a special attribute called row.names
# can create a data frame by read.table() or read.csv()# 
# can be converted to a matric by data.matrix()
### For Loops   #######################

x <- c("a", "b", "c", "d")
for (i in 1:4) { print (x[i]) }

for (i in seq_along(x)){ print(x[i])}
for (letter in x) { print (letter) }
for (i in 1:4) print (x[i])

######### nested for loops ########
# Matrices are a special type of vector in R, they have a dimension.
## matrices are constructed column-wise.
m <- matrix (nrow=2, ncol=3)
m
dim(m)
attributes(m)
m<-1:10
dim(m)<-c(2,5)
m

####### binding columns and rows
x <- 1:3
y<-10:12

cbind(x,y)

rbind(x,y)
#########################
### Factors unordered or ordered represents categorical data
<<<<<<< Updated upstream
 "Male" and "Female" is better than 1 and 2
=======
# "Male" and "Female" is better than 1 and 2
>>>>>>> Stashed changes
 
 x<-factor(c("yes", "yes", "no", "yes", "no"))
x
table(x)
unclass(x)
#can set the order of level of a factor. 
x<-factor(c("yes", "yes", "no", "yes", "no"),levels= c("yes", "no"))
    x       ##################

x <- matrix (1:6, 2, 3)
x
y <- matrix (1:6)
y
<<<<<<< Updated upstream
=======
###############################################################################
rm(list=ls())
setwd("~/Documents/R.coursera")
list.files()
gco.v1.df<-read.csv(file="gco.v1.df.csv")
table(gco.v1.df$cluster)
gco.v1.df.by.state <-split(gco.v1.df, gco.v1.df$state, drop=FALSE)


list.ids <- unique(gco.v1.df$store.id)
x <- list(a = 1:5, b = rnorm(10))

str(list.ids)
lapply(split(gco.v1.df$malibu.coconut.rum.1.75l.price, gco.v1.df$state, drop=FALSE), mean, na.rm=TRUE)
sapply(split(gco.v1.df$malibu.coconut.rum.1.75l.price, gco.v1.df$state, drop=FALSE), mean, na.rm=TRUE)

##############

setwd("~/Documents/R.coursera/R.coursera.programing 1 class")
getwd()
pollutantmean(specdata,)
#############################

x <- c(rnorm(10), runif(10), rnorm(10))
f <- gl(3, 10)
>>>>>>> Stashed changes
