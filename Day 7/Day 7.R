v <- readLines("Day 7/input.txt")
v

l <- list()
i <- 3
wd <- NULL
while (i <= length(v)) {
	while (!grepl("\\$ cd", v[i]) & i <= length(v)) {
		if (grepl("dir ", v[i])) {
			dirname <- substring(v[i], first = 5)
			l[[c(wd, dirname)]] <- list()
			i <- i+1
			# eval(dplyr::sym(paste(wd, collapse = "$")))[substring(v[i], first = 5)] = list()
		} else {
			size <- strsplit(v[i], " ")[[1]][[1]]
			filename <- strsplit(v[i], " ")[[1]][[2]]
			l[[c(wd, filename)]] <- as.numeric(size)
			i <- i+1
		}
	}
	if (grepl("\\$ cd \\w", v[i])) {
		wd <- c(wd, substring(v[i], first = 6)) #Get folder name after "$ cd "
		i <- i+2
	} else if (grepl("\\$ cd ..", v[i])) {
		wd <- wd[-length(wd)]
		i <- i+1
	}
}

#Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?
sizes <- NULL
for (i in seq_len(purrr::vec_depth(l))) {
	size <- unlist(map_depth(l, .depth = i, ~sum(unlist(.x[is.list(.x)])), .ragged = TRUE))
	sizes <- c(sizes, size)
}
sizes[sizes<=100000 & sizes!=0] |> sum()


#Part 2
maxsize <- 70000000
reqsize <- 30000000
currsize <- unlist(l) |> sum()
unused <- maxsize-currsize
min_to_delete <- reqsize-unused
min(sizes[sizes>min_to_delete])
