rankall <- function(outcome, num = "best")
{
     outcomes <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
     
     ##check and transform outcome name
     outcome <- tolower(outcome)
     
     if(!any(outcome %in% c("heart attack", "heart failure", "pneumonia")))
          stop("invalid outcome")
     
     splitOutcome <- strsplit(outcome, " ")[[1]]
     formattedName <- paste(splitOutcome, collapse=".")
     outcomeColumnName <- paste("hospital.30.day.death..mortality..rates.from", formattedName, sep=".")
     
     ##get column number
     colnum <- which( tolower(colnames(outcomes)) == outcomeColumnName)
     
     ##get rid of extra columns
     smallFrame <- cbind(outcomes[, 2], outcomes[, 7], outcomes[colnum])
     names(smallFrame) <- c("hospital", "state", "performance")
     
     ##remove NAs
     bad <- is.na(smallFrame[, 3])
     completeFrame <- smallFrame[!bad, ]
     
     ##evaluate best outcome for each state
     stateNames <- unique(outcomes[, 7])
     finalFrame <- data.frame(hospital = NULL, state = NULL)
     for(thisState in stateNames)
     {     
          tempFrame <- completeFrame[completeFrame[["state"]] == thisState, ]
          
          ##evaluate best outcome
          ##order by 2 cols: http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns
          orderedFrame <- tempFrame[ order(tempFrame[,3], tempFrame[,1]), ]
          
          ##transform num if necessary
          rank <- num
          if(num == "best")
               { rank <- as.numeric(1) }
          else if(num == "worst")
               { rank <- as.numeric(nrow(orderedFrame)) }
          
          if(rank > nrow(orderedFrame))
               { finalFrame <- rbind(finalFrame, data.frame(hospital = NA, state = thisState)) }
          else
               { finalFrame <- rbind(finalFrame, orderedFrame[rank, 1:2]) }
     }
     
     return(finalFrame[ order(finalFrame[, 2]), ])
}