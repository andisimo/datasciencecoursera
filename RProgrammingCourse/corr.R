corr <- function(directory, threshold = 0)
{
     id <- 1:332
     returnvec <- vector()
     
     for(idnumber in id)
     {
          #transform id integer with no leading 0's into string to match filename
          if(idnumber < 10)
          {
               idnumberstring <- paste("00", idnumber, sep="")
          } else if(idnumber < 100)
          {
               idnumberstring <- paste("0", idnumber, sep="")
          } else
          {
               idnumberstring <- as.character(idnumber)
          }
          
          #construct directory location
          location <- paste(directory, "/", idnumberstring, ".csv", sep = "")
          
          #read file
          csv <- read.csv(location)
          
          #flag rows with NA data
          good <- complete.cases(csv[,2:3])
          
          #isolate the good rows in their own data frame
          goodframe <- csv[good, ]
          
          #compare to threshold
          if(length(goodframe[,4]) >= threshold)
          {
               returnvec <- c(returnvec, cor(goodframe$nitrate, goodframe$sulfate))
          }
     }
     
     return(returnvec)
}
