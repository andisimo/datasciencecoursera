complete <- function(directory, id = 1:332)
{
     dframe <- data.frame()
     
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
          
          #create a data frame with one row and 2 columns - ID and NOBS (a count of the good rows)
          thisframe <- data.frame("id" = idnumber, "nobs" = length(goodframe[,4]))
          
          #append thisframe to the data frame you will return
          dframe <- rbind(dframe, thisframe)
     }
     
     return(dframe)
}