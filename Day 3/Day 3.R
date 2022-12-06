library(purrr)
v <- readLines("Day 3/input.txt")
first <- substring(v, 1, nchar(v)/2) |> strsplit("")
second <- substring(v, 1+nchar(v)/2) |> strsplit("")

dups <- map2_chr(first, second, ~ unique(.x[.x %in% .y]))
sum(match(dups, c(letters, LETTERS)))

#Part 2
l <- v |> strsplit("") |> lapply(unique)
badges <- c()
for (i in 1:100*3) {
	badges <- c(badges, l[[i]][l[[i]] %in% l[[i-1]] & l[[i]] %in% l[[i-2]]])
}
sum(match(badges, c(letters, LETTERS)))

