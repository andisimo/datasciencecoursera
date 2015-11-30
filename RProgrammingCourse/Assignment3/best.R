best <- function(state, outcome)
{
     outcomes <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
     
     ##check and transform state name
     state <- toupper(state)
     stateNames <- unique(outcomes[, 7])
     
     if(!any(state %in% stateNames))
          stop("invalid state")
     
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
     names(smallFrame) <- c("name", "statename", "performance")
     
     ##subset by state
     smallFrame <- smallFrame[smallFrame[["statename"]] == state, ]
     
     ##remove NAs
     bad <- is.na(smallFrame[, 3])
     completeFrame <- smallFrame[!bad, ]
     
     ##evaluate best outcome
     mini <- min(completeFrame[, 3])
     bests <- completeFrame[completeFrame[["performance"]] == mini, ]
     if (length(bests) > 1)
     {
          bests <- bests[order(bests$name), ]
     }
     
     return(as.vector(bests[1, 1]))
}