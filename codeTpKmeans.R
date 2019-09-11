### TP KMEANS ###

###EXERCICE 1###

#1#

#First we generate all the data
n <- 200
x1 <- rnorm(n/2)
y1 <- rnorm(n/2)
x2 <- rnorm(n/2, 4, 2)
y2 <- rnorm(n/2, 4, 2)
d <- data.frame(c(x1, x2), c(y1, y2))
names(d) <- c("x", "y")

#On genere toutes ces données a partir d'un loi normale centrée
#réduite et d'une loi normal avec un moyenne egal a 4 et
# d'écart type 2. On utilise la fonction rnorm afin de généré toutes
#les données.
#Finalement on les mets dans un seul dataframes avec pour colonnes x et y
#Ce dataframe va donc contenir 200 points généré aleatoirement

#On va représenté ces données a l'aide de la fonction plot
plot(d)

#on peut discerner deux groupes, l'un au milieu du plot et l'autre en bas a droite

#2#

#On va utiliser la fonction kmeans pour realiser le clustering
km = kmeans(d,2)
km

#We are going to look if the clustering vector are similar to km$cluster
km$cluster

#3#

#On va maintenant regarder a quoi ressemble ces clusters
plot(d, pch = "")
text(d, label = km$cluster)

#La classification a l'air d'avoir du sens car tout les 1 et les 2
#(donc les deux differentes classes) ont l'air d etre bien séparé et
#assez homogènes
plot(d, col = km$cluster)

#4#

#On va regarder quelles est la classe avec la plus petite inertie
km$withinss / km$size

#on obtient une inertie de 8.07 pour la classe 1 et de 2.28 pour la classe 2
#Donc la classe 2 (celle en rouge) est plus homogène que la classe 1

#On va maintenant calculer l'inertie intraclasse
mean((km$withinss / km$size)[1]*(95/n) + (km$withinss / km$size)[2]*(105/n))

#On verifie que cette valeur est égale a celle calculer par kmeans
km$tot.withinss/n

#5#

#On va maintenant regarder a l'inertie interclasse
km$betweenss/n

#6#

#On va creer une boucle afin de voir quel est le plus judicieux choix de clustering
#pour cela on va calculer plusieurs cluster avec un k different
k = 5

#On initialise nos variables qui vont nous etre utile dans notre boucle
intra = vector()
inter = vector()
tot = vector()

#On creer une boucle pour calculer tout les kmeans
for (i in 1:k){
  km1 <- kmeans(d,i)
  intra[i] <- km1$tot.withinss/n
  inter[i] <- km1$betweenss/n
  tot[i] <- inter[i] + intra[i]
}

#On represente graphiquement ce que l'on a calculé
plot(intra, type="l")
points(inter, type = "l", lty = 2)
points(tot, type = "l", lty = 3)

#On voit bien qu'apres 2 la courbe ne reduit plus beaucoup (elle se coude) donc
#le choix de deux clusters semble etre le plus judicieux

#7#

#On va regarder a quoi ressemblerai nos clusters si on avait pris k egal a 3
#On calcule les clusters pour k = 3
km3 <- kmeans(d, 3)

plot(d, col = km3$cluster)

#Avec trois classes on voit bien que l'on a du mal a differencier le cluster
#de couleur verte et celui de couleur noire.

###EXERCICE 2###

#1#

#On va maintenant travailler sur le dataset USArrests qui repertorie
#celon l'etats differentes arrestation
head(USArrests)

#On va d'abord devoir centré réduire les données afin de pouvoir les analysé car
#les échelles ne sont pas les memes pour toutes les varaibles. Par exemple
#si on ne le fait pas, la variables Assault risque d'avoir plus d'importance que la variable
#Murder dans les calcul.
scaleUSArrests <- scale(USArrests)

head(scaleUSArrests)
#On peut donc voir que nos données sont désormais centrée réduites

#2#

#On va creer une boucle afin de voir quel est le plus judicieux choix de clustering
#pour cela on va calculer plusieurs cluster avec un k different
k = 6

#On initialise nos variables qui vont nous etre utile dans notre boucle
intra = vector()

#On creer une boucle pour calculer tout les kmeans jusqu'a k=6 et on regarde l'inertie inter et intra classe
for (i in 1:k){
  km1 <- kmeans(scaleUSArrests,i)
  intra[i] <- km1$tot.withinss/n
}

#On represente graphiquement ce que l'on a calculé afin de regarder combien de clusters seront le plus judicieux
plot(intra, type="l")

#On remarque que la courbe se coude au niveau de 2 clusters, ce qui signifie que 2 clusters est le nombre ideal
kmUSArrests <- kmeans(scaleUSArrests, 2)

#On va regarder visuellement ces deux clusters
plot(scaleUSArrests, pch = "")
text(scaleUSArrests, label = kmUSArrests$cluster, col = kmUSArrests$cluster)

#On peut voir que le cluster 1 est sans doute les etats sans trop d'arrestation et l'autre cluster le contraire

#3#

#On va regarder comment est calculé chaque centre de gravité
kmUSArrests$centers
#Nos intuitions etaient bonne car le cluster 1 sont les points avec le moins d'arrestation et vice-versa