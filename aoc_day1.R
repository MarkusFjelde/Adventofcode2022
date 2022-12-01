christmas_packages <- c("dplyr","tidyr")
lapply(christmas_packages, library, character.only=TRUE)
#===========================Advent of code day 1===============================#
SnackElves <- 
  read.table("~/Dropbox/Jobb/adventofcode/d1/inputd1.txt", 
             blank.lines.skip = F, col.names = "calories")

SnackElves <- as_tibble(SnackElves)

SnackiestElves <- SnackElves %>% 
  mutate(group = ifelse(is.na(calories), 1, 0)) %>% 
  mutate(total = cumsum(group)) %>% 
  na.omit() %>% 
  group_split(total) %>% 
  flatten() %>% 
  lapply(sum)

max(unlist(SnackiestElves))

#[1] 69795

#=============================================================================#
#Part 2
sum(head(sort(unlist(SnackiestElves), decreasing = T),3))

#[1] 208437