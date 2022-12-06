v <- readLines("Day 5/input.txt")
#s for stacks
s <- lapply(v[1:8], substring, first = (4*1:9)-2, last = (4*1:9)-2) |> 
	unlist() |> 
	matrix(ncol=9, byrow = TRUE) |> 
	as.data.frame() |> 
	as.list() |>
	lapply(function(x) {subset(x, x!=" ")})
s	
	
library(tidyverse)
#l for list (of instructions)
l <- v[11:length(v)] |> str_extract_all("\\d+") |> lapply(as.numeric)
l
for (inst in l) {
	for (n in 1:inst[[1]]) {
		s[[inst[[3]]]] <- c(s[[inst[[2]]]][1], s[[inst[[3]]]]) #Take the first  element from the column specified in the second list element
		s[[inst[[2]]]] <- s[[inst[[2]]]][-1] #get rid of the taken element
	}
}
s

#Part 2
#Relead v, s and l
# Instead of a loop for each crate, just subset the range of crates
for (inst in l) {
		s[[inst[[3]]]] <- c(s[[inst[[2]]]][1:inst[[1]]], s[[inst[[3]]]]) #Take the first  element from the column specified in the second list element
		s[[inst[[2]]]] <- s[[inst[[2]]]][-c(1:inst[[1]])] #get rid of the taken element
}
s

