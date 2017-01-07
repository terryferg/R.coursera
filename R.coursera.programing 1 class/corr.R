### corr function by Terry Ferg
##

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
        new.df <-new.df
        print(new.df)
}
corr <- function(directory, threshold = 0) {
        ## This function takes a directory of data files and a threshold for complete cases and 
        ##  calculates the correlation between sulfate and nitrate for monitor locations where the number of 
        ##  completely observed cases (on all variables) is greater than the threshold. The function should 
        ##  return a vector of correlations for the monitors that meet the threshold requirement. If no 
        ##  monitors meet the threshold requirement, then the function should return a numeric vector of
        ##  length 0.
        ## needs
        ## take the complete function from before and make a minor adjustment: create the
        ## datafile using all files, then one can select the ones needed in the corr function.
        ## need to identify completely observed case from earlier function
        setwd(directory) ## sets the directory
        totaldata.df<- data.frame() # sets an empty dataframe as a starting point for rbind
        complete.data.df <- data.frame()  # once we have an aggregate datafile then we will subset to get only complete cases
        list.our.files <- list.files()  # creates a list of the datafiles in the directory of interest
                for (i in 1:332) {
                totaldata.df <- rbind(totaldata.df, read.csv(list.our.files[i])) ##loops through the data files rbinding them together
                 }
        complete.data.df <- totaldata.df[complete.cases(totaldata.df),]
        new.df<-as.data.frame(table(complete.data.df$ID))
        colnames(new.df) <- c("file.id", "nobs")
        new.df <-new.df
        print(new.df)
} 
 


