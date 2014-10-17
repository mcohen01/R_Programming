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

rawData <- function(id, directory, threshold) {
  rows <- paste(directory, "/", asFile(id), sep="") %>%
  read.csv %>% 
  na.omit
  if (length(rows$nitrate) > threshold) {
    cor(rows$sulfate, rows$nitrate)
  }
}

corr <- function(directory, threshold = 0) {
  1:332 %>%
  sapply(rawData, directory, threshold) %>%
  unlist
}