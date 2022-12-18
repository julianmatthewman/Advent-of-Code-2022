v <- readLines("Day 11/input.txt")
v

#Monkeys
m <- list(
	m1=c(84, 66, 62, 69, 88, 91, 91),
	m2=c(98, 50, 76, 99),
	m3=c(72, 56, 94),
	m4=c(55, 88, 90, 77, 60, 67),
	m5=c(69, 72, 63, 60, 72, 52, 63, 78),
	m6=c(89, 73),
	m7=c(78, 68, 98, 88, 66),
	m8=c(70)
)

#Operations
o <- list(
	o1=function(old) {old*11},
	o2=function(old) {old*old},
	o3=function(old) {old+1},
	o4=function(old) {old+2},
	o5=function(old) {old*13},
	o6=function(old) {old+5},
	o7=function(old) {old+6},
	o8=function(old) {old+7}
)

#Tests
t <- list(
	t1=function(x) {if(x%%2==0L) 5 else 8},
	t2=function(x) {if(x%%7==0L) 4 else 7},
	t3=function(x) {if(x%%13==0L) 5 else 1},
	t4=function(x) {if(x%%3==0L) 7 else 6},
	t5=function(x) {if(x%%19==0L) 2 else 8},
	t6=function(x) {if(x%%17==0L) 3 else 1},
	t7=function(x) {if(x%%11==0L) 3 else 6},
	t8=function(x) {if(x%%5==0L) 2 else 4}
)

#Numbers get too large for module operation so need to pre-divide by the product of all the divisors (are all prime numbers)
#Product of the divisors
p <- prod(2,7,13,3,19,17,11,5)

solved11 <- function(rounds, div_by) {
	counter <- rep(0,8)
	for (round in seq_len(rounds)) {
		for (monkey_no in seq_along(m)) {
			for (item in m[[monkey_no]]) {
				if (!is.null(item)) {
					new_item <- floor(o[[monkey_no]](item)/div_by)
					new_item <- new_item %% p
					pass_to <- t[[monkey_no]](new_item)
					#Pass to monkey
					m[[pass_to]] <- c(m[[pass_to]], new_item)
					#Delete first item
					m[[monkey_no]] <- m[[monkey_no]][-1]
					#Add to counter
					counter[[monkey_no]] <- counter[[monkey_no]]+1}
			}
		}
	}
	counter
}

#Part 1
counter <- solved11(rounds=20, div_by=3)
sort(counter) |> tail(2) |> prod()
#Part 2
counter <- solved11(rounds=10000, div_by=1)
sort(counter) |> tail(2) |> prod()
