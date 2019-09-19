#1#
#2#
#3#

#We start by the dependencies
library(rpart.plot)

#4#

#Then we load the data
data("ptitanic")

?ptitanic

#plot the head of the dataset
head(ptitanic)

#We are going to look at the survived variable
class(ptitanic$survived)
summary(ptitanic$survived)
#The type of the variable is factor, that means that it take only the value "died" or "survived"

#5#

#We will compute our decision tree and plot it
r <- rpart(survived~., data = ptitanic)
rpart.plot(r)
#In the box we have the label of the response variable
#Then, the proportion of the survived people, so a low value with a "died" is good that means that the prediction is good
#Finally the proportion of the entire data

#For the block in the bottom left:
#If there is a 27 years old man, the tree say to us that it will die but there is a proportion of 0.17 that it will have survived even if the tree predict DEATH

#Let's take a subset of the male persons in the third class of the titanic
ex <- subset(ptitanic, sex != "male" & pclass != "3rd") # exemples 
#We can see how many survived
summary(ex$survived) 

#Then we can check at proportion of survivant
sum(ex$survived == "survived") / nrow(ex) # proportion de survivants
nrow(ex) / nrow(ptitanic) # pourcentage relat. `a tous les exemple
#Our subset contains 19% of the survivant

#6#

#We create a new passengers
np <- data.frame("1st", "female", 19, 3, 0) 

#create names
names(np) <- c("pclass", "sex", "age", "sibsp", "parch")

#Will find the probability that the new passengers died with the decision tree we've created
predict(r, newdata = np)

#With the biggest class (higher probability)
predict(r, newdata = np, type = "class")
#Thus the new passengers had a higher chance to survived on the titanic

#We'll look at the error
pred.surv <- predict(r, newdata = ptitanic, type = "class")

#We compare our prediction with the reality
mat.conf <- table(ptitanic$survived, pred.surv)
mat.conf

#Erreur d'apprentissage
(mat.conf[1,2] + mat.conf[2,1]) / sum(mat.conf)
#It is low but the model is surely overfitted because we use it on the same set of data


#7#


#Getting the number of row of the dataset
n <- nrow(ptitanic)

set.seed(123)
#Creating the train set ...
param.app <- 0.7 # proportion of observation for the training set

permut.lignes <- sample(n) # we shuffle the row index

sel <- permut.lignes[1:(param.app * n)] # This the line we select for the train dataset

#Creating the test and train subset of the original data
train <- ptitanic[sel,]
test <- ptitanic[-sel,]

#Creating the new model
r2 <- rpart(survived~., data = train)

#Running prediction on the test dataset
pred.surv2 <- predict(r2, newdata = test, type = "class") 

#We compare our prediction with the reality
mat.conf2 <- table(test$survived, pred.surv2)
mat.conf2

#We can look at a relevant error measure
(mat.conf2[1,2] + mat.conf2[2,1]) / sum(mat.conf2)


#8#


?rpart.control
#minsplit: this is the minimum number of observations that must exist in a node on order for a split to be attempted
#minsplit == 0 will make the tree go really deeper
#cp: This is the complexity parameter. This mean how the overall R squared needs to increase for each nodes
#more cp is low more the tree is deep

#Let's try with those argument
r3 <- rpart(survived~., data = ptitanic, minsplit = 0, cp = 0) 
rpart.plot(r3)
#As we can see the tree is really deep

#the trick is to find the best value of minsplit and cp
#We can try to findl the best cp for a constant minsplit
#We'll set it to 20
minsplit <- 20

#Setting the cp values to try
mes.cp <- seq(0, 0.1, by = 0.001)

#Setting the corresponding errors for each values
erreur.test <- numeric(length(mes.cp))

#Will create the erreur function that will compute the error

erreur <- function(matconf){
  
  #Computing the error using the mat.conf value
  (matconf[1,2] + matconf[2,1]) / sum(matconf)
}

#creating a loop that will for every cp give back the error of the tree (with the confusion matrix)
for (i in 1:length(mes.cp)) {
  r4 <- rpart(survived~., data = train, cp = mes.cp[i], minsplit = minsplit) 
  pred.surv4 <- predict(r4, newdata = test, type = "class") 
  mat.conf4 <- table(test$survived, pred.surv4) 
  erreur.test[i] <- erreur(mat.conf4) 
} 

#Look at the results with a plot
plot(mes.cp, erreur.test, type = "l")
abline(v = 0.012, col = "red")
#cp equal to 0.012 seems good, let's run the model with it

#We create a new model
r5 <- rpart(survived~., data = ptitanic, cp = 0.012, minsplit = 20) 
rpart.plot(r5)
#The fact that is looks like the first one is just a coincidence


#9#


#Now we look at the accuracy of our prediction on a new dataset. Splitting into a train set and a test set
#Allows us to not have an overfitted model and to look if our model performs well in other new dataset
#Another way to go beyond train and test is to try a cross validation


#10#


#Dependencies
library(mlbench)

#Loading the dataset
data("Zoo")

#We are going to look at the new dataset
head(Zoo)
summary(Zoo)
?Zoo

#our response variable will be the type variable, we'll try to predic the type of the animal
#First let's create the train and test dataset
set.seed(42)

#We are going to compute a function that select the best value of cp and minsplit
best.cp <- function( dataset, nb.cp = 100, mes.minsplit = 20){
  
  #initializing cp seq
  mes.cp = seq(0, 0.1, by = 1/nb.cp)
  
  #Initializing the vectors errors
  erreur.test.cp = numeric(length(mes.cp))
  
  #Creating test and train
  param.app <- 0.7 # proportion of observation for the training set
  permut.lignes <- sample(n) # we shuffle the row index
  sel <- permut.lignes[1:(param.app * n)] # This the line we select for the train dataset
  traindata <- dataset[sel,]
  testdata <- dataset[-sel,]
  
  #starting the testing
  #Looping for each value of minsplit
 
  #Looping for each value of cp
  for (i in 1:length(mes.cp)){
    
    #Computing the model
    R <- rpart(type~., data = traindata, cp = mes.cp[i], minsplit = mes.minsplit)
    
    #Running prediction
    pred.surv <- predict(R, newdata = testdata, type = "class")
    
    #Computing the confusion matrices
    mat.conf <- table(testdata$type, pred.surv)
    
    #Computing the error
    erreur.test.cp[i] <- erreur(mat.conf)
    
    
  }
    

  
  #Return the results
  plot(mes.cp, erreur.test.cp, type = "l")
  
}



#Creating test and train
param.app <- 0.7 # proportion of observation for the training set
permut.lignes <- sample(n) # we shuffle the row index
sel <- permut.lignes[1:(param.app * n)] # This the line we select for the train dataset
traindata <- Zoo[sel,]
testdata <- Zoo[-sel,]
#lets model that for fun
R <- rpart(type~., data = traindata, cp = 0.015, minsplit = 10)







