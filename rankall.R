rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv(file="outcome-of-care-measures.csv",header=T,colClasses = "character",na.strings="Not Available")
  ## Check that state and outcome are valid
  conditions=c("heart attack","heart failure","pneumonia")
  
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
  ## For each state, find the hospital of the given rank
  
  result_df = data.frame()
  for(state in unique(data$State)) {
    data.state <- data[data$State == state, ]
    data.state[, i] <- as.numeric(x = data.state[, i])
    data.state <- data.state[complete.cases(data.state), ]
    
    hospital_ranks <- order(data.state[,i])
    
    if(num=="best") {
      rank <- 1
    } else if(num=="worst") {
      rank <- length(data.state[, i])
    } else {
      rank <- num
    }
    
    data_index <- hospital_ranks[rank] 
    hospital <- data.state[data_index,]$Hospital.Name
    if(!is.na(hospital)) {
      state_df <- data.frame(cbind(hospital, state), row.names=state)
      result_df <- rbind(result_df, state_df)
    }
  }
  ## Return a data frame with the hospital names and the (abbreviated) state name
  return(result_df)
}