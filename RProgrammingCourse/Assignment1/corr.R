corr <- function(directory, threshold = 0) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'threshold' is a numeric vector of length 1 indicating the
     ## number of completely observed observations (on all
     ## variables) required to compute the correlation between
     ## nitrate and sulfate; the default is 0
     
     ## Return a numeric vector of correlations
     ## NOTE: Do not round the result!
     
     correlations <- vector()
     
     for (idnum in 1:332)
     {
          if (idnum < 10)
               idnumformatted <- paste("00", idnum, sep="")
          else if (idnum < 100)
               idnumformatted <- paste("0", idnum, sep="")
          else
               idnumformatted <- as.character(idnum)
          
          dataset <- read.csv(paste(directory, "/", idnumformatted, ".csv", sep=""))
          completeCases <- complete.cases(dataset)
          if(length(dataset[completeCases,1]) > threshold)
          {
               correlation <- cor(dataset[completeCases, 2], dataset[completeCases, 3])
               correlations <- c(correlations, correlation)
          }
     }
     
     return(correlations)
}