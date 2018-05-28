best <- function(state, outcome) {
## Read outcome data
  setwd("/Users/gabriel/Classes/CourseraDataScience/datasciencecoursera/ProgrammingAssignment3")
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
## Check that state and outcome are valid
  if(!isOutcomeValid(outcome)) stop("invalid outcome")
  if(!isStateValid(state, outcomeData)) stop("invalid state")
## Return hospital name in that state with lowest 30-day death
  if(outcome == "heart attack") hospital <- lowestHeartAttackHospital(outcomeData, state)
  if(outcome == "heart failure") hospital <- lowestHeartFailureHospital(outcomeData, state)
  if(outcome == "pneumonia") hospital <- lowestPneumoniaHospital(outcomeData, state)
## rate
  print(hospital)
}

isOutcomeValid <- function(outcome) {
  validOutcomes <- c("heart attack", "heart failure","pneumonia")
  is.element(outcome, validOutcomes)
}

isStateValid <- function(state, outcomeData) {
  states <- unique(outcomeData$State)
  is.element(state, states)
}

lowestHeartAttackHospital <- function(outcomeData, state) {
  outcomeData[, 11] <- as.numeric(outcomeData[, 11])
  subsetByState <- outcomeData[outcomeData$State == state,]
  modifiedOutcome <- subsetByState[order(subsetByState[,11], subsetByState[,2]),]
  modifiedOutcome[1,2]
}

lowestHeartFailureHospital <- function(outcomeData, state) {
  outcomeData[, 17] <- as.numeric(outcomeData[, 17])
  subsetByState <- outcomeData[outcomeData$State == state,]
  modifiedOutcome <- subsetByState[order(subsetByState[,17], subsetByState[,2]),]
  modifiedOutcome[1,2]
}

lowestPneumoniaHospital <- function(outcomeData, state) {
  outcomeData[, 23] <- as.numeric(outcomeData[, 23])
  subsetByState <- outcomeData[outcomeData$State == state,]
  modifiedOutcome <- subsetByState[order(subsetByState[,23], subsetByState[,2]),]
  modifiedOutcome[1,2]
}