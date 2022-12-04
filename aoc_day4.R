christmas_packages <- c("dplyr","tidyr", "tidyverse", "stringr")
lapply(christmas_packages, library, character.only=TRUE)
#===========================Advent of code day 4===============================#
#Part1
Rooms <- 
  read.table("~/Dropbox/Jobb/adventofcode/d4/inputd4.txt", 
             sep = ",", col.names = c("id1", "id2"))
id1 <-  
  as.data.frame(str_split_fixed(Rooms$id1, "-", 2))
names(id1) <- c("start1","stop1")
id2 <- 
  as.data.frame(str_split_fixed(Rooms$id2, "-", 2))
names(id2) <- c("start2","stop2")

Rooms <-
  bind_cols(id1, id2)

Rooms <-
  mutate_all(Rooms, function(x) as.numeric(as.character(x)))

Rooms <- Rooms %>% 
  add_column(whole = NA)
  
for (i in 1:1000){
  if(Rooms$start2[i] >= Rooms$start1[i] & Rooms$stop2[i] <= Rooms$stop1[i]){
    Rooms$whole[i] <- TRUE
  } else if(Rooms$start1[i] >= Rooms$start2[i] & Rooms$stop1[i] <= Rooms$stop2[i]){
    Rooms$whole[i] <- TRUE
  }
    else {
      Rooms$whole[i] <- FALSE
  }
}
length(Rooms$whole[Rooms$whole == TRUE])
#[1] 651

#Part2
for (i in 1:1000){
  whole1 <- Rooms$start1[i]:Rooms$stop1[i]
  whole2 <- Rooms$start2[i]:Rooms$stop2[i]
  
  if(whole1[i] %in% whole2[i]){
    Rooms$whole[i] <- TRUE
  } else if (whole2[i] %in% whole1[i]) {
    Rooms$whole[i] <- TRUE
  } else {
    Rooms$whole[i] <- FALSE
  }
}
length(Rooms$whole[Rooms$whole == TRUE])
#[1] 956