library(tidyverse)
v <- readLines("Day 10/input.txt")
nums <- as.numeric(str_extract(v, "[-+]?\\d+")) |> replace_na(0)
nums

nums2 <- NULL
for (num in nums) {
	if (num==0) {
		nums2 <- c(nums2, 0)
	} else {
		nums2 <- c(nums2, 0,num)
	}
}

x <- 1
for (i in 2:length(nums2)) {
 x <- c(x, x[length(x)]+nums2[[i-1]])
}
signal_strength <- x*seq_along(x)
sum(signal_strength[c(20, 60, 100, 140, 180, 220)])


#Part 2

#Sprite position (+2)
x

#Pixel drawn
ind <- rep(1:40, 6)
pixels <- ifelse(ind == x | ind == x+1 | ind == x+2, "#",".")
writeLines(tapply(pixels, rep(1:6, each= 40), paste, collapse=""))
