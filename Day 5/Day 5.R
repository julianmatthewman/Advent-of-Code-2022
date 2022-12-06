v <- readLines("Day 5/input.txt")
v
library(tidyverse)

#Bad soluton for reading in matrix
m <- c("S,C,0,0,Z,0,0,0,0,
F,J,P,0,0,T,0,N,0,
G,H,G,Q,0,G,0,D,0,
V,V,D,G,F,D,0,V,0,
R,B,F,N,N,Q,L,S,0,
J,M,M,P,H,V,B,B,D,
L,P,H,D,L,F,D,J,L,
D,T,V,M,J,N,F,M,G") |> 
	str_remove_all("[\r\n]") |> 
	strsplit(",") |> 
	unlist() |> 
	matrix(nrow = 8, ncol = 9,byrow = TRUE)
m[m==0] <- NA
m
	
l <- v[11:length(v)] |> str_extract_all("\\d+") |> lapply(as.numeric)
l
for (i in seq_along(l)) {
	for (n in l[[i]][[1]]) {
		m[,l[[n]][[3]]][is.na(m[,l[[n]][[3]]])][1] <- #Set the first NA element in the colum specified inthe third list element
			m[,l[[n]][[2]]][!is.na(m[,l[[n]][[2]]])][1] #Take the first non-NA element from the column specified in the second list element
		m[,l[[n]][[2]]][!is.na(m[,l[[n]][[2]]])][1] <- NA #Set the taken element to NA
	}
}
m

l[[1]][[1]]

m[,l[[1]][[3]]][is.na(m[,l[[1]][[3]]])][1] <- #Set the first NA element in the colum specified inthe third list element
	m[,l[[1]][[2]]][!is.na(m[,l[[1]][[2]]])][1] #Take the first non-NA element from the column specified in the second list element
m[,l[[1]][[2]]][!is.na(m[,l[[1]][[2]]])][1] <- NA #Set the taken element to NA
