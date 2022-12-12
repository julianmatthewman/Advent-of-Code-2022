v <- readLines("Day 9/input.txt")
temp <- strsplit(v, " ")
tape <- lapply(temp, function(x) rep(x[[1]], x[[2]])) |> unlist()
# tape <- head(tape, 20)

head_x <- 0
head_y <- 0
tail_x <- 0
tail_y <- 0

head_coord <- list(c(0,0))
tail_coord <- list(c(0,0))

for (step in tape) {
	if (step=="R") head_x <- head_x + 1
	if (step=="L") head_x <- head_x - 1
	if (step=="U") head_y <- head_y + 1
	if (step=="D") head_y <- head_y - 1
	head_coord <- append(head_coord, list(c(head_x, head_y)))
	
	if (head_x-tail_x > 1 & head_y==tail_y) tail_x <- tail_x + 1
	if (head_x-tail_x < -1 & head_y==tail_y) tail_x <- tail_x - 1
	if (head_y-tail_y > 1 & head_x==tail_x) tail_y <- tail_y + 1
	if (head_y-tail_y < -1 & head_x==tail_x) tail_y <- tail_y - 1
	
	#Diagonals
	if (head_x-tail_x > 1 & head_y-tail_y > 0) {
		tail_x <- tail_x + 1
		tail_y <- tail_y + 1
	}
	if (head_x-tail_x > 1 & head_y-tail_y < 0) {
		tail_x <- tail_x + 1
		tail_y <- tail_y - 1
	}
	
	if (head_x-tail_x < -1 & head_y-tail_y > 0) {
		tail_x <- tail_x - 1
		tail_y <- tail_y + 1
	}
	if (head_x-tail_x < -1 & head_y-tail_y < 0) {
		tail_x <- tail_x - 1
		tail_y <- tail_y - 1
	}
	
	
	#Diagonals 2
	if (head_x-tail_x > 0 & head_y-tail_y > 1) {
		tail_x <- tail_x + 1
		tail_y <- tail_y + 1
	}
	if (head_x-tail_x > 0 & head_y-tail_y < -1) {
		tail_x <- tail_x + 1
		tail_y <- tail_y - 1
	}
	
	if (head_x-tail_x < 0 & head_y-tail_y > 1) {
		tail_x <- tail_x - 1
		tail_y <- tail_y + 1
	}
	if (head_x-tail_x < 0 & head_y-tail_y < -1) {
		tail_x <- tail_x - 1
		tail_y <- tail_y - 1
	}
	
	
	tail_coord <- append(tail_coord, list(c(tail_x, tail_y)))
}

cbind(
	as.data.frame(head_coord) |> t(),
	as.data.frame(tail_coord) |> t()
)
length(unique(tail_coord))



#Part 2
one_coord <- list(c(0,0))
two_coord <- list(c(0,0))
three_coord <- list(c(0,0))
four_coord <- list(c(0,0))
five_coord <- list(c(0,0))
six_coord <- list(c(0,0))
seven_coord <- list(c(0,0))
eight_coord <- list(c(0,0))
nine_coord <- list(c(0,0))

move <- function(coord1, coord2) {
	head_x <- coord1[[1]]
	head_y <- coord1[[2]]
	tail_x <- coord2[[1]]
	tail_y <- coord2[[2]]
	
	
	if (head_x-tail_x > 1 & head_y==tail_y) tail_x <- tail_x + 1
	if (head_x-tail_x < -1 & head_y==tail_y) tail_x <- tail_x - 1
	if (head_y-tail_y > 1 & head_x==tail_x) tail_y <- tail_y + 1
	if (head_y-tail_y < -1 & head_x==tail_x) tail_y <- tail_y - 1
	
	#Diagonals
	if (head_x-tail_x > 1 & head_y-tail_y > 0) {
		tail_x <- tail_x + 1
		tail_y <- tail_y + 1
	}
	if (head_x-tail_x > 1 & head_y-tail_y < 0) {
		tail_x <- tail_x + 1
		tail_y <- tail_y - 1
	}
	
	if (head_x-tail_x < -1 & head_y-tail_y > 0) {
		tail_x <- tail_x - 1
		tail_y <- tail_y + 1
	}
	if (head_x-tail_x < -1 & head_y-tail_y < 0) {
		tail_x <- tail_x - 1
		tail_y <- tail_y - 1
	}
	
	
	#Diagonals 2
	if (head_x-tail_x > 0 & head_y-tail_y > 1) {
		tail_x <- tail_x + 1
		tail_y <- tail_y + 1
	}
	if (head_x-tail_x > 0 & head_y-tail_y < -1) {
		tail_x <- tail_x + 1
		tail_y <- tail_y - 1
	}
	
	if (head_x-tail_x < 0 & head_y-tail_y > 1) {
		tail_x <- tail_x - 1
		tail_y <- tail_y + 1
	}
	if (head_x-tail_x < 0 & head_y-tail_y < -1) {
		tail_x <- tail_x - 1
		tail_y <- tail_y - 1
	}
	
	
	list(c(tail_x, tail_y))
}

for (step in head_coord[-1]) {
	one_coord <- append(one_coord, move(step, one_coord[[length(one_coord)]]))
	two_coord <- append(two_coord, move(one_coord[[length(one_coord)]], two_coord[[length(two_coord)]]))
	three_coord <- append(three_coord, move(two_coord[[length(two_coord)]], three_coord[[length(three_coord)]]))
	four_coord <- append(four_coord, move(three_coord[[length(three_coord)]], four_coord[[length(four_coord)]]))
	five_coord <- append(five_coord, move(four_coord[[length(four_coord)]], five_coord[[length(five_coord)]]))
	six_coord <- append(six_coord, move(five_coord[[length(five_coord)]], six_coord[[length(six_coord)]]))
	seven_coord <- append(seven_coord, move(six_coord[[length(six_coord)]], seven_coord[[length(seven_coord)]]))
	eight_coord <- append(eight_coord, move(seven_coord[[length(seven_coord)]], eight_coord[[length(eight_coord)]]))
	nine_coord <- append(nine_coord, move(eight_coord[[length(eight_coord)]], nine_coord[[length(nine_coord)]]))
}

cbind(
	as.data.frame(head_coord) |> t(),
	as.data.frame(one_coord) |> t()
)
length(unique(nine_coord))
