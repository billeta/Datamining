#We create a function that computes the Shannon entropy
sh <- function(x){
  
  #If x is 0 we directly put it as 0, because R cannot infer that its 0
  if(x == 0 | x == 1)
    return(0)
  
  #Shannon entropy formula
  (- x * log(x) - (1 - x) * log(1 - x)) / log(2)
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
