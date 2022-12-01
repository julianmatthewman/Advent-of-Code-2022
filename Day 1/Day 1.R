v <- readLines("Day 1/input.txt")

lst <- split(v[v!=""], cumsum(v=="")[v!=""]) #The list starts with element `0` for some reasons, so rename in enxt step to start with 1
names(lst) <- paste0(seq_along(lst))
lst <- lapply(lst, as.numeric)
sums <- unlist(lapply(lst, sum))
max(sums)

# Part 2
sum(sort(sums, decreasing = TRUE)[1:3])

