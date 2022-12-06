rm(list = ls())
christmas_packages <- c("dplyr","tidyr", "tidyverse", "stringr")
lapply(christmas_packages, library, character.only=TRUE)
#===========================Advent of code day 6===============================#
#Part1
datastream <- 
  read.table("~/Dropbox/Jobb/adventofcode/d6/inputd6.txt")

readStream <- function(stream, n){
  #loop to create 4 letter strings
  len <- str_length(stream)
  quartets <- c()
  
  for (i in 1:len){
    quartets[i] <- 
      str_sub(stream,i,i+(n-1))
    if (length(unique(utf8ToInt(quartets[i]))) == n){
      x <- i + (n-1)
      break
    }
  }
  return(x)
}
readStream(datastream, 4)
# [1] 1538
readStream(datastream, 14)
# [1] 2315