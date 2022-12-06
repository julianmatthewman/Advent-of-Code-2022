library(tidyverse)
v <- read.table("Day 2/input.txt")
names(v) <- c("you", "me")
head(v)


# A/X for Rock: 1
# B/Y for Paper: 2
# C/Z for Scissors: 3

v <- v %>% 
	mutate(
		points=case_when(
			you=="A" & me=="X" ~ 3,
			you=="A" & me=="Y" ~ 6,
			you=="A" & me=="Z" ~ 0,
			you=="B" & me=="X" ~ 0,
			you=="B" & me=="Y" ~ 3,
			you=="B" & me=="Z" ~ 6,
			you=="C" & me=="X" ~ 6,
			you=="C" & me=="Y" ~ 0,
			you=="C" & me=="Z" ~ 3,
		),
		points=case_when(
			me=="X" ~ points + 1,
			me=="Y" ~ points + 2,
			me=="Z" ~ points + 3,
		)
	)
sum(v$points)


# Part 2
# A/X for Rock: 1
# B/Y for Paper: 2
# C/Z for Scissors: 3
# X means you need to lose
# Y means you need to end the round in a draw
# Z means you need to win

v <- v %>% 
	mutate(
		resp=case_when(
			you=="A" & me=="X" ~ "Z",
			you=="A" & me=="Y" ~ "X",
			you=="A" & me=="Z" ~ "Y",
			you=="B" & me=="X" ~ "X",
			you=="B" & me=="Y" ~ "Y",
			you=="B" & me=="Z" ~ "Z",
			you=="C" & me=="X" ~ "Y",
			you=="C" & me=="Y" ~ "Z",
			you=="C" & me=="Z" ~ "X",
		),
		points2=case_when(
			you=="A" & resp=="X" ~ 3,
			you=="A" & resp=="Y" ~ 6,
			you=="A" & resp=="Z" ~ 0,
			you=="B" & resp=="X" ~ 0,
			you=="B" & resp=="Y" ~ 3,
			you=="B" & resp=="Z" ~ 6,
			you=="C" & resp=="X" ~ 6,
			you=="C" & resp=="Y" ~ 0,
			you=="C" & resp=="Z" ~ 3,
		),
		points2=case_when(
			resp=="X" ~ points2 + 1,
			resp=="Y" ~ points2 + 2,
			resp=="Z" ~ points2 + 3,
		)
	)
sum(v$points2)	

