#### Terry Ferg's assignment 1, part 3 
#################################### part 3 ###################################.

# Write a function that takes a directory of data files and a threshold for 
# complete cases and calculates the correlation between sulfate and nitrate 
# for monitor locations where the number of completely observed cases (on all
# variables) is greater than the threshold. The function should return a vector
# of correlations for the monitors that meet the threshold requirement. If no
# monitors meet the threshold requirement, then the function should return a
# numeric vector of length 0. A prototype of this function follows

# corr <- (directory, threshold = 0){
##  'directory' is a character vector of length 1 indicating 
##  the locatio of the CSV files.
##  'threshold' is a numeric vector of length 1 indicating the
##  number of completely observed observations  (on all variables)
##  required to compute the correlation between
##  nitrate and sulfate; the degfault is 0.
##  Return a numeric vector of correlation. Do not round the 
##  results.
#  }
## WIP




corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating the location of
  ## the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the number of
  ## completely observed observations (on all variables) required to compute
  ## the correlation between nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  complete.data.df = complete(directory)
  ids = complete.data.df[complete.data.df["nobs"] > threshold, ]$id
  corrr = numeric()
  for (i in ids) {
     
    newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                             ".csv", sep = ""))
    dff = newRead[complete.cases(newRead), ]
    corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
  }
  return(corrr)
}
complete <- function(directory, id = 1:332) {
  f <- function(i) {
    data = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                          ".csv", sep = ""))
    sum(complete.cases(data))
  }
  nobs = sapply(id, f)
  return(data.frame(id, nobs))
}

