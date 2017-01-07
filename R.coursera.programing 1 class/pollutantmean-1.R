
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ### terry ferg's Coursera course: R Assignment 1, part 1.
  # Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a
  # specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and
  # 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the
  # directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors,
  # ignoring any missing values coded as NA. 
  
  setwd(directory) ## sets the directory
  totaldata.df<-data.frame()    # sets an empty dataframe as a starting point for rbind
  list.our.files<-list.files()  # creates a list of the datafiles in the directory of interest
  for (i in id){
    totaldata.df <- rbind(totaldata.df, read.csv(list.our.files[i])) ##loops through the data files rbinding them together
  }
  mean(totaldata.df[, pollutant], na.rm = TRUE)  
  }
