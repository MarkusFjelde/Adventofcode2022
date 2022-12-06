christmas_packages <- c("dplyr","tidyr", "tidyverse", "stringr")
lapply(christmas_packages, library, character.only=TRUE)
#===========================Advent of code day 6===============================#
#Part1
datastream <- 
  read.table("~/Dropbox/Jobb/adventofcode/d6/inputd6.txt")

readStream <- function(stream, n){
  #loop to create 4 letter strings
  len <- str_length(stream[1])
  quartets <- c()
  
  counter <- 1
  for (i in 1:len){
    quartets[i] <- 
      str_sub(stream[1],counter,counter+(n-1))
    counter <- counter + 1
  }
  
  #Make df to hold utf transposed strings
  toInt <- data.frame(
    matrix(nrow = len, ncol = 1)
  )
  colnames(toInt) <- "startSignal"
  
  #load utf transposed values as how many unique "strings" are in each  
  for (i in 1:len){
    toInt[i,] <- length(unique(utf8ToInt(quartets[i])))
  }
  #Find "strings" that are length 14
  first <- which(toInt$startSignal == n) 
  #Find the index of first occurrence
  return(first[1] + (n-1))
}

#Part1
readStream(datastream, 4)
# [1] 1538

#Part2
readStream(datastream, 14)
# [1] 2315
