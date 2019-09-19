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
#Then, the proportion of the other response variable
#Finally the proportion of the entire data

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
