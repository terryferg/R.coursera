
# The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter (PM) air pollution at 332 locations in the United States. Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:
        
#        Date: the date of the observation in YYYY-MM-DD format (year-month-day)
# sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
# nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)
# For this programming assignment you will need to unzip this file and create the directory 'specdata'. Once you have unzipped the zip file, do not make any modifications to the files in the 'specdata' directory. In each file you'll notice that there are many days where either sulfate or nitrate (or both) are missing (coded as NA). This is common with air pollution monitoring data in the United States.

# Part 1

# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate)
# across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory',
# 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate
# matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant 
# across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows

## removing all objects from a workspace
rm(list = ls())
setwd("~/Documents/R.coursera/R.coursera.programing 1 class")
list.files()


 # Write a function that reads a directory full of files and reports the number of completely
# observed cases in each data file. The function should return a data frame where the first
# column is the name of the file and the second column is the number of complete cases.
##.
setwd("~/Documents/R.coursera/R.coursera.programing 1 class")
getwd()

source("pollutantmean.R")
pollutantmean("~/Documents/R.coursera/R.coursera.programing 1 class/specdata","sulfate",1:10)
pollutantmean("~/Documents/R.coursera/R.coursera.programing 1 class/specdata","sulfate",70:72)
pollutantmean("~/Documents/R.coursera/R.coursera.programing 1 class/specdata","sulfate",23)
pollutantmean("~/Documents/R.coursera/R.coursera.programing 1 class/specdata","sulfate",3:50)


###############################################################################################
Part 2
###############################################################################################
##  Write a function that reads a directory full of files and reports the number of completely observed cases
##  in each data file. The function should return a data frame where the first column is the name of the file and
##  the second column is the number of complete cases. A prototype of this function follows
##  
##  You can see some example output from this function. The function that you write should be able to match this output.
Please save your code to a file named complete.R. To run the submit script for this part, make sure your working directory
has the file complete.R in it.

# Write a function that reads a directory full of files and reports the number of completely
# observed cases in each data file. The function should return a data frame where the first
# column is the name of the file and the second column is the number of complete cases.

source("complete.R")
complete <- function(directory, id= 1:332) {
        setwd(directory) ## sets the directory
        totaldata.df<- data.frame() # sets an empty dataframe as a starting point for rbind
        complete.data.df <- data.frame()  # once we have an aggregate datafile then we will subset to get only complete cases
        list.our.files<-list.files()  # creates a list of the datafiles in the directory of interest
        for (i in id){
                totaldata.df <- rbind(totaldata.df, read.csv(list.our.files[i])) ##loops through the data files rbinding them together
        }
        complete.data.df <- totaldata.df[complete.cases(totaldata.df),]
        new.df<-as.data.frame(table(complete.data.df$ID))
        colnames(new.df) <- c("file.id", "nobs")
        new.df <<-new.df
        print(new.df)
}
complete("~/Documents/R.coursera/R.coursera.programing 1 class/specdata", 1)
##   id nobs
## 1  1  117
complete("~/Documents/R.coursera/R.coursera.programing 1 class/specdata", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
complete("~/Documents/R.coursera/R.coursera.programing 1 class/specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463
complete("~/Documents/R.coursera/R.coursera.programing 1 class/specdata", 3)
##   id nobs
## 1  3  243

complete <- function(directory, id= 1:332) {
        setwd(directory) ## sets the directory
        totaldata.df<- data.frame() # sets an empty dataframe as a starting point for rbind
        complete.data.df <- data.frame()  # once we have an aggregate datafile then we will subset to get only complete cases
        list.our.files<-list.files()  # creates a list of the datafiles in the directory of interest
        for (i in id){
                totaldata.df <- rbind(totaldata.df, read.csv(list.our.files[i])) ##loops through the data files rbinding them together
        }
        complete.data.df <- totaldata.df[complete.cases(totaldata.df),]
        new.df<-as.data.frame(table(complete.data.df$ID))
        colnames(new.df) <- c("file.id", "nobs")
        new.df <<-new.df
        print(new.df)
}
rm(list=ls())
setwd("/Users/terryferg/Documents/R.coursera/R.coursera.programing 1 class")
source("complete.R")
#list.files()
complete("/Users/terryferg/Documents/R.coursera/R.coursera.programing 1 class/specdata", 1:332)
tail(totaldata.df)
head(complete.data.df)
names(complete.data.df)
new.df<<-as.data.frame(table(complete.data.df$ID))
colnames(new.df) <- c("file.id", "nobs")
head(new.df)
tail(new.df)


###############################################################################################
##  part 3
##
##  Write a function that takes a directory of data files and a threshold for complete cases and 
##  calculates the correlation between sulfate and nitrate for monitor locations where the number of 
##  completely observed cases (on all variables) is greater than the threshold. The function should 
##  return a vector of correlations for the monitors that meet the threshold requirement. If no 
##  monitors meet the threshold requirement, then the function should return a numeric vector of
##  length 0. A prototype of this function follows
##
##
##  For this function you will need to use the 'cor' function in R which calculates the correlation 
##  between two vectors. Please read the help page for this function via '?cor' and make sure that you 
##  know how to use it.
##
##  You can see some example output from this function. The function that you write should be able
##  to match this output. Please save your code to a file named corr.R. To run the submit script for 
##  this part, make sure your working directory has the file corr.R in it.


source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)
## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
cr <- corr("specdata", 400)
head(cr)
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
cr <- corr("specdata", 5000)
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
length(cr)
## [1] 0
cr <- corr("specdata")
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
length(cr)
## [1] 323
######################################################################################################

corr <- function(directory, threshold = 0) {
        ## This function takes a directory of data files and a threshold for complete cases and 
        ##  calculates the correlation between sulfate and nitrate for monitor locations where the number of 
        ##  completely observed cases (on all variables) is greater than the threshold. The function should 
        ##  return a vector of correlations for the monitors that meet the threshold requirement. If no 
        ##  monitors meet the threshold requirement, then the function should return a numeric vector of
        ##  length 0.
        ## needs
}






###############################################################################################
You can see some example output from this function. The function that you write should be able to match this
output. Please save your code to a file named pollutantmean.R.

Part 2

Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. A prototype of this function follows


You can see some example output from this function. The function that you write should be able to match this output. Please save your code to a file named complete.R. To run the submit script for this part, make sure your working directory has the file complete.R in it.

Part 3

Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. A prototype of this function follows


For this function you will need to use the 'cor' function in R which calculates the correlation between two vectors. Please read the help page for this function via '?cor' and make sure that you know how to use it.

You can see some example output from this function. The function that you write should be able to match this output. Please save your code to a file named corr.R. To run the submit script for this part, make sure your working directory has the file corr.R in it.
######################################################################################################
setwd("~/Documents/R.coursera/R.coursera.programing 1 class/specdata")
####

rm(list=ls())
##### using a vector to store aggregate data
a <- 1:10
x <- 11:20
start <- length(a) + 1  ## length(a) is the original ending index
end <- start + length(x) - 1
a[start:end] <- x ### adds 11:20 to the existing 1:10 vector by selecting the 11th spot and filling from that spot with x!
a
###### ############ example teaching ###########################
In the following example, we’re going to use a list to simulate reading data 
from three files. The main reason for doing that is so we can reference the list
using an index instead of reading from a file.

First, we initialize an empty results vector. Then we initialize start and end 
indices to zero.

Inside the loop, we retrieve a sample vector and get it’s size. We set start to
the previous end plus one and set end to start plus the size of the sample 
vector minus one. Note in the example that
we use an if statement to check the size. We only want to add to our results 
when size > 0.
#################################### ok lets apply above: 
## Sample data
a <- 1:10
b <- 11:15
c <- numeric() #initialize an empty results vector
vectors <- list(a=a, b=b, c=c)

## Results vector
results <- numeric() #initialize an empty results vector

## Initialize start and end indices
start <- end <- 0

for(index in seq_along(vectors)) {
        values <- vectors[[index]]
        size <- length(values)
        
        if(size > 0) {
                start <- end + 1
                end <- start + size - 1
                results[start:end] <- values
        }
}

results
################################
## Sample data
frames <- list(data.frame(a=1:10, b=21:30), data.frame(a=11:15, b=31:35))
class(frames)
## Results vector
results <- list(a=numeric(), b=numeric())

## Initialize start and end indices
start.a <- start.b <- end.a <- end.b <- 0

for(index in seq_along(frames)) {
        values <- frames[[index]]
        size.a <- length(values$a)
        size.b <- length(values$b)
        
        if(size.a > 0) {
                start.a <- end.a + 1
                end.a <- start.a + size.a - 1
                results$a[start.a:end.a] <- values$a
        }
        if(size.b > 0) {
                start.b <- end.b + 1
                end.b <- start.b + size.b - 1
                results$b[start.b:end.b] <- values$b
        }
}