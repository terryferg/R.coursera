## data frame
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
 "Male" and "Female" is better than 1 and 2
 
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
