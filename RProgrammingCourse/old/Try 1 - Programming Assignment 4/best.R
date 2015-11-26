best <- function(state, outcome) 
{
     outcomecolumn <- NULL
     
     if(outcome == "heart attack")
     {
          outcomecolumn <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
     }else if(outcome == "heart failure")
     {
          outcomecolumn <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
     }else if(outcome == "pneumonia")
     {
          outcomecolumn <- "Hospital.30.Day.Readmission.Rates.from.Pneumonia"
     }else
     {
          error <- stop("The outcome argument is invalid", call. = TRUE)
          return(error)
     }
     
     ## Read outcome data
     healthdata <- read.csv(paste(getwd(), "/outcome-of-care-measures.csv", sep = ""), colClasses = "character")
     
     sortedData <- healthdata[order(paste("healthdata$", outcomecolumn, sep = ""))]
     
     #splitData <- split(sortedData, sortedData[,7])
     #lapply(splitData, )
     
     return(sortedData[1, 2])
     
     ## Check that state and outcome are valid
     ## Return hospital name in that state with lowest 30-day death
     ## rate
}