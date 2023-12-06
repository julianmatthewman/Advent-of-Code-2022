#--- Day 1: Trebuchet?! ---
#calibration document consists of lines of text; each line originally contained a specific calibration value that needs to be recovered\
#On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.

#Read a text file from my downlaods folder
x <- readLines("~/Downloads/input-4.txt")
x

#a function to combine the first digit and the last digit (in that order) to form a single two-digit number
combine <- function(x){
  first <- substr(x, 1, 1)
  last <- substr(x, nchar(x), nchar(x))
  return(paste(first, last, sep = ""))
}
combine(x)

#get the first digit from a string
first <- substr(x, 1, 1)
first

#get the first number from a string
first <- as.numeric(substr(x, 1, 1))

#a function that gets the first number from a string
get_first <- function(x){
  first <- as.numeric(substr(x, 1, 1))
  return(first)
}

#a regular expression that gets the first digit from a string
first <- gsub("([0-9]).*", "\\1", x)
first

#a regular expression that gets the first digit from a string, the digit isn't necesarily at the start of the string
first <- gsub(".*([0-9]).*", "\\1", x)
first

library(stringr)
all <- str_extract_all(x, "[0-9]")

#paste together the first and last elements of each list element
first_last <- sapply(all, function(x) paste(x[1], x[length(x)], sep = ""))
first_last
firstlast <- as.numeric(first_last)
sum(firstlast)
