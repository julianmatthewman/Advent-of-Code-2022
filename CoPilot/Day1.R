
# Puzzle 1 ----------------------------------------------------------------

#read a txt file from my downloads folder as a vector
x <- readLines("~/Downloads/input.txt")
x

#split the vector into a list of vectors, splitting at each blank element
y <- split(x, cumsum(x==""))
y

#remove the blank elements in each vector of the list
z <- lapply(y, function(x) x[x!=""])
z

#convert the character vectors to integer vectors
z <- lapply(z, function(x) as.integer(x))
z

#get the sum of each vector
z <- lapply(z, sum)
z

#get the sum of the three largest sums
sum(tail(sort(unlist(z)), 3))