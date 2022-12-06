christmas_packages <- c("dplyr","tidyr", "tidyverse", "stringr")
lapply(christmas_packages, library, character.only=TRUE)
#===========================Advent of code day 6===============================#
#Part1
datastream <- 
  read.table("~/Dropbox/Jobb/adventofcode/d6/inputd6.txt")

#loop to create 4 letter strings
len <- str_length(datastream)
quartets <- c()

counter <- 1
for (i in 1:len){
  
  quartets[i] <- 
    str_sub(datastream$V1,counter,counter+3)
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
first <- which(toInt$startSignal == 4) 
#Find the index of first occurrence
first[1] + 3

#Part2
#loop to create 14 letter strings
quartetsMessage <- c()
counter <- 1
for (i in 1:len){
  
  quartetsMessage[i] <- 
    str_sub(datastream$V1,counter,counter+13)
  counter <- counter + 1
  
}
#Make df to hold utf transposed "strings"
toIntMessage <- data.frame(
  matrix(nrow = len, ncol = 1)
)
colnames(toIntMessage) <- "startSignal"

#load utf transposed values as how many unique "strings" are in each  
for (i in 1:len){
  
  toIntMessage[i,] <- length(unique(utf8ToInt(quartetsMessage[i])))
  
}
#Find "strings" that are length 14
first2 <- which(toIntMessage$startSignal == 14)
#Find the index of first occurrence
first2[1] + 13