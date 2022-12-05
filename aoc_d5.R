christmas_packages <- c("dplyr","tidyr", "tidyverse", "stringr")
lapply(christmas_packages, library, character.only=TRUE)
#===========================Advent of code day 5===============================#
#Part1
steps <- 
  read.table("~/Dropbox/Jobb/adventofcode/d5/inputd5.txt", skip = 10)

steps <- steps %>% 
  select(2,4,6) %>% rename(move = V2, from = V4, to = V6)

crates <- 
  read.fwf("~/Dropbox/Jobb/adventofcode/d5/inputd5.txt", 
           widths = c(3,4,4,4,4,4,4,4,4), n = 8, header = FALSE)

cratesvec <- c()
for (i in 1:9) {
  cratesvec[i] <- paste(crates[,i], collapse = "")
  cratesvec <- gsub(" ", "", cratesvec)
  cratesvec <- as.character(gsub("[", "", cratesvec, fixed = TRUE))
  cratesvec <- as.character(gsub("]", "", cratesvec, fixed = TRUE))
}

cratesdf <- as.data.frame(cratesvec)

#Part1&2
len <- length(steps$move)
for (i in 1:len){
  move <- steps$move[i]
  from <- steps$from[i]
  to <- steps$to[i]
  cratesdf$cratesvec[to] <- 
    paste0(intToUtf8(rev(utf8ToInt(str_sub(cratesdf$cratesvec[from],1,move)))), cratesdf$cratesvec[to])
  ##Part2 string to paste not reversed: str_sub(cratesdf$cratesvec[from],1,move)
  cratesdf$cratesvec[from] <-
    sub(str_sub(cratesdf$cratesvec[from],1,move), "", cratesdf$cratesvec[from])
  
}

topStacks <- c()
for (i in 1:9){
  topStacks[i] <- 
    str_sub(cratesdf$cratesvec[i], 1,1)
}

paste(topStacks, collapse = "")
#[1] "SHQWSRBDL"
#[1] "DTQZHBRS"
