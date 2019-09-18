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
#on veut passer la premiere matrice de distance et non d2^2

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

#1#

#We are looking a the pca of the dataset
library(FactoMineR)
pcaUSA <- PCA(d)

#Then we can have a look at the plot of the two category with the two first component
plot(pcaUSA$ind$coord, pch = "")
text(pcaUSA$ind$coord, labels = names(classe), col = as.vector(classe))

#2#

#We are now going to look at the same plot but with 4 classes
#Let's first compute the four class
classe4 <- cutree(h, 4)

#Then we create the new plot
plot(pcaUSA$ind$coord, pch = "")
text(pcaUSA$ind$coord, labels = names(classe4), col = as.vector(classe4))

#####EXERCICE 3#####

#We are first using the euclidean distance
distances2 <- dist(d)

#Let's check the difference between all the different distances agglomeration methods
simpleh <- hclust(distances2, method = "single")
completeh <- hclust(distances2, method = "complete")
averageh <- hclust(distances2, method = "average")

#Lets look at our graph
par(mfrow = c(1,3))
plot(simpleh, main = "single linkage")
plot(completeh, main = "complete linkage")
plot(averageh, main = "average linkage")
par(mfrow = c(1,1))

#We can see that the methods that it used can change the way we create the clusters with the CAH

#We can go along and plot the tree with the height graph
par(mfrow = c(2,3))
plot(simpleh, main = "single linkage")
plot(completeh, main = "complete linkage")
plot(averageh, main = "average linkage")
plot(rev(simpleh$height)[1:10], type = "b", xlab="number of clusters", ylab = "distance")
plot(rev(completeh$height)[1:10], type = "b", xlab="number of clusters", ylab = "distance")
plot(rev(averageh$height)[1:10], type = "b", xlab="number of clusters", ylab = "distance")
par(mfrow = c(1,1))
#Plotting every graph of linkage tree with the graph of distances can help figure out
#how many clusters to take

#Let's cut into groups using the graphs
simplegroup <- cutree(simpleh, 2)
completegroup <- cutree(completeh, 4)
averagegroup <- cutree(averageh, 5)

#And let's plot the cluster using the two first PCA

par(mfrow = c(2,3))

plot(simpleh, main = "single linkage")
plot(completeh, main = "complete linkage")
plot(averageh, main = "average linkage")

#Single linkage
plot(pcaUSA$ind$coord, pch = "", main = "Using 2 clusters")
text(pcaUSA$ind$coord, labels = names(simplegroup), col = as.vector(simplegroup))

#complete linkage
plot(pcaUSA$ind$coord, pch = "", main = "Using 4 clusters")
text(pcaUSA$ind$coord, labels = names(completegroup), col = as.vector(completegroup))

#average linkage
plot(pcaUSA$ind$coord, pch = "", main = "Using 5 clusters")
text(pcaUSA$ind$coord, labels = names(averagegroup), col = as.vector(averagegroup))

par(mfrow = c(1,1))


