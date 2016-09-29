#### examples Test file #1 by Terry Ferg

hw1.data.df<-read.csv(file ="hw1_data.csv" )  # reading in a .csv file


head(hw1.data.df)
dim(hw1.data.df)
tail(hw1.data.df)
table(hw1.data.df$Ozone)
summary(hw1.data.df$Ozone)
mean(hw1.data.df$Ozone, na.rm=TRUE)
mean(hw1.data.df$Solar.R[>31 and ])
mean(hw1.data.df[ which(hw1.data.df$Ozone>31 & hw1.data.df$Temp>90),])
mean(Q18$Solar.R)

mean(hw1.data.df$Temp[hw1.data.df$Month=='6'],)
summary(hw1.data.df$Ozone[hw1.data.df$Month=='5'])

# based on variable values
newdata <- mydata[ which(mydata$gender=='F' 
                         & mydata$age > 65), ]