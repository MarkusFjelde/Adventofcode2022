christmas_packages <- c("dplyr","tidyr")
lapply(christmas_packages, library, character.only=TRUE)
#===========================Advent of code day 1===============================#
#Part 1
Strategy <- 
  read.table("~/Dropbox/Jobb/adventofcode/d2/inputd2.txt", 
             col.names = c("play", "response"))

StrategyN <- Strategy %>% 
  mutate(single_points = 
           ifelse(response == "X", 1, 
                  ifelse(response == "Y", 2, 3))) %>% 
  mutate(game_points = ifelse(play == "A" & response == "Y", 6, 
       ifelse(play == "A" & response == "Z", 0, 
              ifelse(play == "B" & response == "X", 0, 
                     ifelse(play == "B" & response == "Z", 6,
                            ifelse(play == "C" & response == "X", 6, 
                                   ifelse(play == "C" & response == "Y", 0, 3))))))) %>% 
  mutate(sumPoints = rowSums(across(where(is.numeric))))

StrategyN %>% summarise(sum(sumPoints))

# [1] 11841

#Part 2
StrategyM <- Strategy %>% 
  mutate(game_points = ifelse(response == "X", 0, 
                              ifelse(response == "Y", 3, 6))) %>% 
  mutate(single_points = ifelse(play == "A" & response == "X", 3, 
       ifelse(play == "A" & response == "Y", 1,
              ifelse(play == "A" & response == "Z", 2,
                     ifelse(play == "B" & response == "X", 1,
                            ifelse(play == "B" & response == "Y", 2,
                                   ifelse(play == "B" & response == "Z", 3,
                                          ifelse(play == "C" & response == "X", 2,
                                                 ifelse(play == "C" & response == "Y", 3, 1))))))))) %>%
  mutate(sumPoints = rowSums(across(where(is.numeric))))

StrategyM %>% summarise(sum(sumPoints))

# [1] 13022


