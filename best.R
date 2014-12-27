best <- function(state, outcome) {
  data <- read.csv(file="outcome-of-care-measures.csv",header=T,colClasses = "character",na.strings="Not Available")
  
  conditions=c("heart attack","heart failure","pneumonia")
  
  if(!any(state == data$State)) {
    stop("invalid state")
  }
  
  if(!is.element(outcome,conditions)){
    stop("invalid outcome")
  }
  
  i=0
  if(outcome == conditions[1]) {
    i=11
  } else if(outcome == conditions[2]) {
    i=17
  } else if(outcome == conditions[3]) {
    i=23
  }
  
  data.state <- data[data$State == state, ]
  data.state[, i] <- as.numeric(x = data.state[, i])
  data.state <- data.state[complete.cases(data.state), ]
  return.names <- data.state[(data.state[, i] == min(data.state[, i])), ]$Hospital.Name
  return_value = sort(return.names) [1]
  return(return_value)
}