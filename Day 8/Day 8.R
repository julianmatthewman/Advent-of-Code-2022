v <- readLines("Day 8/input.txt")
m <- matrix(unlist(strsplit(v, "")), byrow = TRUE, ncol = nchar(v)[[1]])

# m[1:4,1:4]
# m[1,] #row
# m[,1] #col

visible <- 0
for (col in seq_len(ncol(m))) {
	for (row in seq_len(nrow(m))) {
		if (row==1 | row==99 | col==1 | col==99) {
			visible <- visible + 1
		} else if (all(m[row, col] > m[1:(row-1),col]) | 
				all(m[row, col] > m[(row+1):99,col]) | 
				all(m[row, col] > m[row,1:(col-1)]) | 
				all(m[row, col] > m[row,(col+1):99])) {
			visible <- visible + 1
		}
	}
}
visible

#Part 2
scenic <- NULL
for (col in seq_len(ncol(m))) {
	for (row in seq_len(nrow(m))) {
		n <- 0
		e <- 0
		s <- 0
		w <- 0
		
		if (row!=1) {
			n <- row-max(c(1, max(which((m[row, col] <= m[1:(row-1),col])))))
		}
		if (row!=nrow(m)) {
			s <- min(c(nrow(m)-row, min(row+which((m[row, col] <= m[(row+1):nrow(m),col])))-row))
		}
		if (col!=1) {
			w <- col-max(c(1, max(which((m[row, col] <= m[row,1:(col-1)])))))
		}
		if (col!=ncol(m)) {
			e <- min(c(ncol(m)-col, min(col+which((m[row, col] <= m[row,(col+1):ncol(m)])))-col))
		}
		scenic <- c(scenic, n*e*s*w)
		}
}
# m
# matrix(scenic, ncol = ncol(m))


max(scenic, na.rm = TRUE)

