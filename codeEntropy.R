#We create a function that computes the Shannon entropy
sh <- function(x){
  
  #If x is 0 we directly put it as 0, because R cannot infer that its 0
  if(x == 0 | x == 1)
    return(0)
  
  #Shannon entropy formula
  (- x * log(x) - (1 - x) * log(1 - x)) / log(2)
}

#Creating the gini function
gi <- function(x){
  if(x == 0 | x == 1)
    return(0)
  
  2 * x * (1 - x)
}

#Information gain fot the "M" tree
M <- sh(3/8) - (3/8 * sh(1/3) + 3/8 * sh(2/3) + 2/8 * sh(0))

#Computing for the other tree
A <- sh(3/8) - (1/8 * sh(1) + 4/8 * sh(2/4) + 3/8 * sh(0))
R <- sh(3/8) - (2/8 * sh(1/2) + 3/8 * sh(1/3) + 3/8 * sh(1/3))
E <- sh(2/8) - (5/8 * sh(3/5) + 3/8 * sh(0))

#We can put everything in a matrix
gainMatrix <- matrix(c(M,A,R,E), nrow = 1, ncol = 4, dimnames = list(c("Gain"),
                                                                     c("M", "A", "R", "E")))

#Let's check the results
gainMatrix
#The tree with the highest value of gain is the "A" tree

#With the gini entropy
#Information gain fot the "M" tree
M <- gi(3/8) - (3/8 * gi(1/3) + 3/8 * gi(2/3) + 2/8 * gi(0))
A <- gi(3/8) - (1/8 * gi(1) + 4/8 * gi(2/4) + 3/8 * gi(0))
R <- gi(3/8) - (2/8 * gi(1/2) + 3/8 * gi(1/3) + 3/8 * gi(1/3))
E <- gi(2/8) - (5/8 * gi(3/5) + 3/8 * gi(0))

#We can put everything in a matrix
gainMatrix <- matrix(c(M,A,R,E), nrow = 1, ncol = 4, dimnames = list(c("Gain"),
                                                                     c("M", "A", "R", "E")))

#Let's check the results
gainMatrix
#The tree with the highest value of gain is the "A" tree

