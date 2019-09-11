S = matrix(nrow = 4, ncol = 2, c(2,3,2.5,3.5,2,3.5,2.5,3), byrow = TRUE )
S2 = matrix(nrow = 4, ncol = 2, c(2,4,3,5,2,5,3,5), byrow = TRUE)

#DOES NOT WORK DON'T KNOW WHY???
#Creation of the inertia function
Inertia = function(x){
  #We need to compute k
  k = nrow(x)
  p = ncol(x)
  #Then we compute the average vector
  g = colMeans(x)
  
  #We can then compute the distance
  I <- 0
  for (i in 1:k)
    I <- I + sum((x[i,]-g)^2)
  
  #Finally we return the Inertia
  I/k
}

Inertia(S)

Inertia(S2)

#New version of inertia
Inertia2 = function(x){
  #Then we compute the average vector
  g = colMeans(x)
  
  #We create a new matrix G
  G = matrix(g,nrow(x), ncol(x), byrow = TRUE)
  
  #Then we sum for every rows (we are computing distances)
  mean(rowSums((x - G)^2))
}

Inertia2(S)
Inertia2(S2)


#Lets create a bigger matrix
k = 100000
p = 1000
  
S3 = matrix(runif(k*p), k, p)

#We will look at the computation time between the two function
system.time(Inertia(S))
system.time(Inertia2(S))
