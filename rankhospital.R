rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  setwd("/Users/gabriel/Classes/CourseraDataScience/datasciencecoursera/ProgrammingAssignment3")
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!isOutcomeValid(outcome)) stop("invalid outcome")
  if(!isStateValid(state, outcomeData)) stop("invalid state")
  if(!isNumValid(num, outcomeData)) NA
  
  num = convertNum(num)
  ## Return hospital name in that state with the given rank
  if(outcome == "heart attack") hospital <- rankedHeartAttackHospital(outcomeData, state, num)
  if(outcome == "heart failure") hospital <- rankedHeartFailureHospital(outcomeData, state, num)
  if(outcome == "pneumonia") hospital <- rankedPneumoniaHospital(outcomeData, state, num)
  
  print(hospital)
  ## 30-day death rate
}

rankedHeartAttackHospital <- function(outcomeData, state, num) {
  outcomeData[, 11] <- as.numeric(outcomeData[, 11])
  subsetByState <- outcomeData[outcomeData$State == state,]
  
  validLength <- getValidLength(subsetByState, 11)
  modifiedOutcome <- subsetByState[order(subsetByState[,11], subsetByState[,2]),]
  
  if(num == "worst") value = modifiedOutcome[validLength,2]
  else if(num>validLength) value = NA
  else value = modifiedOutcome[num,2]
  value
}

rankedHeartFailureHospital <- function(outcomeData, state, num) {
  outcomeData[, 17] <- as.numeric(outcomeData[, 17])
  subsetByState <- outcomeData[outcomeData$State == state,]
  
  validLength <- getValidLength(subsetByState, 17)
  modifiedOutcome <- subsetByState[order(subsetByState[,17], subsetByState[,2]),]
  
  if(num == "worst") value = modifiedOutcome[validLength,2]
  else if(num>validLength) value = NA
  else value = modifiedOutcome[num,2]
  value
}

rankedPneumoniaHospital <- function(outcomeData, state, num) {
  outcomeData[, 23] <- as.numeric(outcomeData[, 23])
  subsetByState <- outcomeData[outcomeData$State == state,]
  
  validLength <- getValidLength(subsetByState, 23)
  modifiedOutcome <- subsetByState[order(subsetByState[,23], subsetByState[,2]),]
  
  if(num == "worst") value = modifiedOutcome[validLength,2]
  else if(num>validLength) value = NA
  else value = modifiedOutcome[num,2]
  value
}

getValidLength <- function(outcomeData, colNum) {
  om <- na.omit(outcomeData[,colNum])
  length(om)
}

isNumValid <- function(num, outcomeData) {
  if(num == "worst") TRUE
  if(num == "best") TRUE
  if(num < 1) FALSE
  if(num > dim(outcomeData)[1]) FALSE
  TRUE
}

isOutcomeValid <- function(outcome) {
  validOutcomes <- c("heart attack", "heart failure","pneumonia")
  is.element(outcome, validOutcomes)
}

isStateValid <- function(state, outcomeData) {
  states <- unique(outcomeData$State)
  is.element(state, states)
}

convertNum <- function(num) {
  if(num == "worst") num #todo
  if(num == "best") 1
  num
}