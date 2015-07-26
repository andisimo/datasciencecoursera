pollutantmean <- function (directory, pollutant, id = 1:332)
{
     dframe <- data.frame()
     
     for(idnumber in id)
     {
          #transform id integer with no leading 0's into string to match filename
          if(idnumber < 10)
          {
               idnumber <- paste("00", idnumber, sep="")
          } else if(idnumber < 100)
          {
               idnumber <- paste("0", idnumber, sep="")
          } else
          {
               idnumber <- as.character(idnumber)
          }
          
          #construct directory location
          location <- paste(directory, "/", idnumber, ".csv", sep = "")
          
          #read file
          csv <- read.csv(location)
          
          #append the file to dframe
          dframe <- rbind(dframe, csv)
     }
     
     #return mean
     mean(dframe[[pollutant]], na.rm = TRUE)
}