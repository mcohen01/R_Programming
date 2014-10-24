library(sqldf)

rankall <- function(outcome, num) {
  rows <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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
  df <- sqldf(paste("select Hospital_Name as hospital, State as state, ", column, 
                    " as cnt from rows where ", column, 
                    "<> 'Not Available' order by state, cnt, hospital",  sep=""))
  
  states <- tbl_df(df) %>%
            distinct(state) %>%
            select(state)
  
  ranking <- function(num) {
    if (is.numeric(num)) {
      num
    } else if (num == "worst") {
      0
    } else if (num == "best") {
      1
    }
  }
  
  state_rank <- function(state_abbr, num) {
    rows <- tbl_df(df) %>%
            filter(state == state_abbr) %>%
            select(hospital, state) %>%
            as.data.frame()
    rows[if (num >0) num else length(rows$hospital),]
  }
  
  states <- sapply(states$state, state_rank, ranking(num))
  as.data.frame(t(states))
  
}