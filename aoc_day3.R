christmas_packages <- c("dplyr","tidyr", "stringr", "tidyverse")
lapply(christmas_packages, library, character.only=TRUE)
#===========================Advent of code day 3===============================#
#Part1
Rucksacks <- 
  read.table("~/Dropbox/Jobb/adventofcode/d3/inputd3.txt", 
             col.names = "items")

#index in vector is equiv to priority
PriorityLetters <- c(letters, LETTERS)

#Split strings in half, split into "one-letter" vectors, compare and check which
#letter is shared with intersect
RuckS <- Rucksacks %>% 
  mutate(first_half = str_sub(items, 1, str_length(items)/2)) %>% 
  mutate(second_half = str_sub(items, str_length(items)/2 + 1, end = str_length(items))) %>% 
  mutate(first_half = strsplit(first_half, split = "")) %>% 
  mutate(second_half = strsplit(second_half, split = "")) %>% 
  mutate(sharedLetters = mapply(intersect, first_half, second_half)) %>% 
  arrange(sharedLetters)

#loop to match shared letters to their priorities
for (i in 1:300) {
  RuckS$priority[i] <- 
    match(RuckS$sharedLetters[i], PriorityLetters)
}

sum(RuckS$priority)
# [1] 8123
#==============================================================================#
#Part2
RuckT <- Rucksacks %>%
  mutate(items = strsplit(items, split = "")) %>% 
  mutate(badgeGroups = as.integer(gl(n(), 3, n()))) %>% 
  group_by(badgeGroups) %>% mutate(intersect = list(Reduce(intersect, items)))
  
#loop to match shared letters to their priorities
x <- as.vector(unlist(RuckT$intersect))

for (i in 1:300){
  RuckT$priority[i] <- 
    match(x[i], PriorityLetters)
}
sum(RuckT$priority)/3
# [1] 2620