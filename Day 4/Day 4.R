v <- readLines("Day 4/input.txt")
l <- v |> 
	strsplit(",") |> 
	lapply(strsplit, "-")

result <- c()
for (i in 1:1000) {
	first <- l[[i]][[1]][[1]]:l[[i]][[1]][[2]]
	second <- l[[i]][[2]][[1]]:l[[i]][[2]][[2]]
	result <- c(result, all(first %in% second) | all(second %in% first))
}
length(result[result])

#Part 2: just replace all with any
result <- c()
for (i in 1:1000) {
	first <- l[[i]][[1]][[1]]:l[[i]][[1]][[2]]
	second <- l[[i]][[2]][[1]]:l[[i]][[2]][[2]]
	result <- c(result, any(first %in% second) | any(second %in% first))
}
length(result[result])

