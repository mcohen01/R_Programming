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

rawData <- function(id, directory, pollutant) {
  path <- paste(directory, "/", asFile(id), sep="")
  d <- read.csv(path)[pollutant]
  d[pollutant][!is.na(d[pollutant])]
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  sapply(id, rawData, directory, pollutant) %>%
  unlist %>%
  mean
}