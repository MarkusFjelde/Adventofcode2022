#===========================Advent of code day 1===============================#
#Part 1
elves.cal <- 
  read.table("~/Dropbox/Jobb/adventofcode/d1/inputd1.txt", blank.lines.skip = F)

elves <- rowSums(is.na(elves.cal) == ncol(elves.cal))
foodGroups <- cumsum(elves) + 1
elf <- split(elves.cal[!elves,], foodGroups[!elves])

MyElfSums <- lapply(elf, sum)

getMyElf <- as.data.frame(do.call(cbind, MyElfSums))

print(max(getMyElf))

#[1] 69795


#=============================================================================#
#Part 2
getMyElf <- as.data.frame(t(getMyElf))
names(getMyElf) <- "calories"

sum(head(getMyElf[order(getMyElf$calories, decreasing = T), ], 3))

#[1] 208437