pollutantmean <- function(directory, pollutant, id = 1:332) 
{
     ##Directory: 
     ##"C:/Users/anschult/Documents/Git/datasciencecoursera/RProgrammingCourse/specdata/"
     
     dframe <- data.frame()
     
     for (idnum in id)
     {
          if (idnum < 10)
               idnum <- paste("00", idnum, sep="")
          else if (idnum < 100)
               idnum <- paste("0", idnum, sep="")
          else
               idnum <- as.character(idnum)
          
          dataset <- read.csv(paste(directory, "/", idnum, ".csv", sep=""))
          dframe <- rbind(dframe, dataset)
          
          x <- 1
     }
     
     mean(dframe[[pollutant]], na.rm=TRUE)
}
