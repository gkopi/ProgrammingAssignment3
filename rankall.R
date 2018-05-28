rankall <- function(outcome, num = "best") {
  ## Read outcome data
  setwd("/Users/gabriel/Classes/CourseraDataScience/datasciencecoursera/ProgrammingAssignment3")
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!isOutcomeValid(outcome)) stop("invalid outcome")
  
  #create empty matrix for the results
  results <- matrix(nrow = 0, ncol = 2)
  names <- c("hospital","state")

  #get numerical outcome index and convert that column's data to numeric type
  numericalColumnIndex <- getNumericalColumnIndex(outcome)
  outcomeData[, numericalColumnIndex] <- as.numeric(outcomeData[, numericalColumnIndex])
  
  # For each state, find the hospital of the given rank
  states = sort(unique(outcomeData$State))
  for(state in states) {
    subsetByState <- outcomeData[outcomeData$State == state,]
    hospital <- getXRankedHospitalForAState(subsetByState, numericalColumnIndex, num)
    results = rbind(results, c(hospital, state))
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  result.df = as.data.frame(results)
  colnames(result.df) <- names
  result.df
}

getXRankedHospitalForAState <- function(data, numericalColumnIndex, rank) {
  numericalRank <- getNumericalRank(data, numericalColumnIndex, rank)
  if(is.na(numericalRank)) value <- NA
  else {
    orderedData <- data[order(data[,numericalColumnIndex], data[,2]),]
    value = orderedData[numericalRank,2]
  }
  value
}

#process the 2 text based ranks, then check to see if the rank is too high and will produce an NA
getNumericalRank <- function(outcomeData, numericalColumnIndex, rank) {
  if(rank == "best") rank <- 1
  nonNAEntries <- na.omit(outcomeData[,numericalColumnIndex])
  if(rank == "worst") {
    rank <- length(nonNAEntries)
  } else if (rank > length(nonNAEntries)) NA
  rank
}

isOutcomeValid <- function(outcome) {
  validOutcomes <- c("heart attack", "heart failure","pneumonia")
  is.element(outcome, validOutcomes)
}

getNumericalColumnIndex <- function(outcome) {
  if(outcome == "heart attack") numericalColumnIndex <- 11
  else if(outcome == "heart failure") numericalColumnIndex <- 17
  else numericalColumnIndex <- 23
  numericalColumnIndex
}
