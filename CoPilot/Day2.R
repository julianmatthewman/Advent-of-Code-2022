# Puzzle 2 ----------------------------------------------------------------

#read a txt file from my downloads folder as a vector
x <- readLines("~/Downloads/input-2.txt")
x

#split each element of the vector in two, removing any whitespace
y <- strsplit(x, " ")
y


#Rock Paper Scissors is a game between two players. 
#Each game contains many rounds; in each round, the players each simultaneously choose one of Rock, Paper, or Scissors using a hand shape. 
#Then, a winner for that round is selected: Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock. 
#If both players choose the same shape, the round instead ends in a draw.

#The first element is what your opponent is going to play: A for Rock, B for Paper, and C for Scissors.
#The second element is what you should play in response: X for Rock, Y for Paper, and Z for Scissors.

#The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

#calculate the score for each round
z <- lapply(y, function(x) {
         if (x[1] == "A" & x[2] == "X") {
    1 + 3
  } else if (x[1] == "A" & x[2] == "Y") {
    2 + 6
  } else if (x[1] == "A" & x[2] == "Z") {
    3 + 0
  } else if (x[1] == "B" & x[2] == "X") {
    1 + 0
  } else if (x[1] == "B" & x[2] == "Y") {
    2 + 3
  } else if (x[1] == "B" & x[2] == "Z") {
    3 + 6
  } else if (x[1] == "C" & x[2] == "X") {
    1 + 6
  } else if (x[1] == "C" & x[2] == "Y") {
    2 + 0
  } else if (x[1] == "C" & x[2] == "Z") {
    3 + 3
  }
})
z
sum(unlist(z))

