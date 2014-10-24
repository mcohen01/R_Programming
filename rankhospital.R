library(sqldf)
  
rankhospital <- function(state, outcome, num) {
  rows <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # validate state
  if (length(rows$State[rows$State == toupper(state)]) == 0) {
    stop("invalid state")
  }
  
  # validate outcome
  outcome <- tolower(outcome)
  field <- if (outcome == "pneumonia") {
    rows[[23]] <- suppressWarnings(as.numeric(rows[[23]]))
    "Pneumonia"    
  } else if (outcome == "heart attack") {
    rows[[11]] <- suppressWarnings(as.numeric(rows[[11]]))
    "Heart.Attack"
  } else if (outcome == "heart failure") {
    rows[[17]] <- suppressWarnings(as.numeric(rows[[17]]))
    "Heart.Failure"
  } else {
    stop("invalid outcome")
  }
  
  field <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", field, sep="")
  column <- gsub("\\.", "_", field)
  df <- sqldf(paste("select Hospital_Name, ", column, 
                    " from rows where State = '", toupper(state),
                    "' and ", column, "<> 'Not Available' order by ", column, ",Hospital_Name",  sep=""))
  index <- if (is.numeric(num)) {
    num
  } else if (num == "worst") {
    length(df[,1])
  } else if (num == "best") {
    1
  }
  df[index, ]$Hospital_Name
}