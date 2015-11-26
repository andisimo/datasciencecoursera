complete <- function(directory, id = 1:332) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'id' is an integer vector indicating the monitor ID numbers
     ## to be used
     
     ## Return a data frame of the form:
     ## id nobs
     ## 1  117
     ## 2  1041
     ## ...
     ## where 'id' is the monitor ID number and 'nobs' is the
     ## number of complete cases
     
     returnFrame <- data.frame()
     
     for (idnum in id)
     {
          if (idnum < 10)
               idnumformatted <- paste("00", idnum, sep="")
          else if (idnum < 100)
               idnumformatted <- paste("0", idnum, sep="")
          else
               idnumformatted <- as.character(idnum)
          
          dataset <- read.csv(paste(directory, "/", idnumformatted, ".csv", sep=""))
          completeCases <- complete.cases(dataset)
          stagingFrame <- data.frame(id = idnum, nobs = length(dataset[completeCases,1]))
          
          returnFrame <- rbind(returnFrame, stagingFrame)
     }
     
     return(returnFrame)
}
