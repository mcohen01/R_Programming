library(dplyr)

asFile <- function(n) {
  if (n < 10) {
    paste("00", n, ".csv", sep="")
  } else if (n < 100) {
    paste("0", n, ".csv", sep="")
  } else {
    paste(n, ".csv", sep="")
  }
}

rawData <- function(id, directory) {
  path <- paste(directory, "/", asFile(id), sep="")
  d <- read.csv(path)
  length((d %>% na.omit())$Date)
}

complete <- function(directory, id = 1:332) {
  nobs <- sapply(id, rawData, directory)
  data.frame(id, nobs)  
}