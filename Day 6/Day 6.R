v <- readLines("Day 6/input.txt")
l <- v |> strsplit("")
l <- l[[1]]
l


markers <- c(FALSE, FALSE, FALSE)
for (i in 4:length(l)) {
	nextmarker <- !any(duplicated(l[(i-3):i]))
	markers <- c(markers, nextmarker)
}
which(markers)[[1]]

#Part 2 (just change to 14 markers instead of 4)
markers <- c(rep(FALSE, 13))
for (i in 14:length(l)) {
	nextmarker <- !any(duplicated(l[(i-13):i]))
	markers <- c(markers, nextmarker)
}
which(markers)[[1]]
