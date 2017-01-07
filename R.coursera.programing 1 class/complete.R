### Complete.R
##  Write a function that reads a directory full of files and reports the number of completely observed cases
##  in each data file. The function should return a data frame where the first column is the name of the file and
##  the second column is the number of complete cases. 
complete <- function(directory, id= 1:332) {
        setwd(directory) ## sets the directory
        totaldata.df<- data.frame() # sets an empty dataframe as a starting point for rbind
        complete.data.df <- data.frame()  # once we have an aggregate datafile then we will subset to get only complete cases
        list.our.files<-list.files()  # creates a list of the datafiles in the directory of interest
        for (i in id){
                totaldata.df <- rbind(totaldata.df, read.csv(list.our.files[i])) ##loops through the data files rbinding them together
        }
        totaldata.df<<-totaldata.df
        complete.data.df <<- totaldata.df[complete.cases(totaldata.df),]
        new.df<<-as.data.frame(table(complete.data.df$ID))
        colnames(new.df) <- c("file.id", "nobs")
        new.df <<-new.df
        print(new.df)
} 

complete("/Users/terryferg/Documents/R.coursera/R.coursera.programing 1 class/specdata", 1:332)