S = matrix(nrow = 4, ncol = 2, c(2,3,2.5,3.5,2,3.5,2.5,3), byrow = TRUE )
S2 = matrix(nrow = 4, ncol = 2, c(2,4,3,5,2,5,3,5), byrow = TRUE)

#Creation of the inertia function
Inertia = function(x){
  #We need to compute k
  k = nrow(x)
  
  #Then we compute the average vector
  g = colMeans(x)
  
  #We can then compute the distance
  i = 0
  for (i in 1:k)
    i = i + sum((x[i,] - g)^2)
  
  #Finally we return the Inertia
  i/k
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
