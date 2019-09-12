##TP 2##

#####EXERCICE 1#####

#1#

#On charge les données
data("USArrests")

#2#

#We store the scaled data into the variable d
d <- scale(USArrests)

#3#

#On creer une matrice de distances
distances <- dist(d)^2/2
#dist() is computing the euclidian distance, on le mets au carré car on veut que des valeurs positives

#4#

#We perform a hierchical clustering
h <- hclust(distances, method = "ward.D")

#Then we plot the results
plot(h, hang = -1)

#5#

#We can see huge height values between 2 classes and 4 classes
#So it can be efficient to take 2 or 4 groups.
plot(rev(h$height)[1:10], type = "l", xlab="number of clusters", ylab = "distance")

#6#

#We'll use the cutree function to select the 2 cluster's partition
classe <- cutree(h,2)

#7#

#We add the classe variable to the distance matrix
classe1 <- subset(d, classe == 1)

#8#

#We'll compute the cardinal and the gravity center of the classe1
n1 <- nrow(classe1[,1:4])
cg1 <- colMeans(classe1[,1:4])

#then we compute his inertia
nvar <- ncol(USArrests)
inertie1 <- mean(rowSums(classe1[,1:4] - matrix(cg1, n1, nvar, byrow = T))^2)

#9#

#We are doing the same as the question 8 but with the second classe
#First we select the class
classe2 <- subset(d, classe == 2)

#We'll compute his cardinal and his gravity center
n2 <- nrow(classe2[,1:4])
cg2 <- colMeans(classe2[,1:4])

#then we compute his inertia
inertie2 <- mean(rowSums(classe2[,1:4] - matrix(cg2, n2, nvar, byrow = T))^2)

#10#

#Class 1:
n1
cg1
inertie1

#Class 2:
n2
cg2
inertie2

#The first class is state with a huge amount of arrests and the second class is state without a lot
#of arrests. Class 1 is less spread out. The two class doesn't look at the urban population
#to categorize the states. (Probably with 4 class we would have urban population affecting some of the class)

#####EXERCICE 2#####