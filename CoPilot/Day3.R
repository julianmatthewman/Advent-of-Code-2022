
# Day 3: Rucksack Reorganization ------------------------------------------

# Language: r

# Each rucksack has two large compartments

#Every item type is identified by a single lowercase or uppercase letter (that is, a and A refer to different types of items).

#The list of items for each rucksack is given as characters all on a single line.
# A given rucksack always has the same number of items in each of its two compartments, so the first half of the characters represent items in the first compartment, while the second half of the characters represent items in the second compartment.

#To help prioritize item rearrangement, every item type can be converted to a priority:
#Lowercase item types a through z have priorities 1 through 26.
#Uppercase item types A through Z have priorities 27 through 52.

#Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?

#read a txt file from my downloads folder as a vector
x <- readLines("~/Downloads/input-3.txt")
x

#get the first half of every element
y <- substr(x, 1, nchar(x)/2)
#get the second half of every element
z <- substr(x, nchar(x)/2 + 1, nchar(x))

#Get the one character that occurs in both halves of each element
a <- mapply(function(x, y) intersect(x, y), strsplit(y, ""), strsplit(z, ""))
a

#convert the character vectors to integer vectors according to the priority rules
b <- lapply(a, function(x) {
  if (x %in% letters) {
    match(x, letters)
  } else if (x %in% LETTERS) {
    match(x, LETTERS) + 26
  }
})
b
sum(unlist(b))
