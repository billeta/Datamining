#1#
#2#
#3#

#We start by the dependencies
library(rpart.plot)

#4#

#Then we load the data
data("ptitanic")

?ptitanic

#We are going to look at the survived variable
class(ptitanic$survived)
summary(ptitanic$survived)
#The type of the variable is factor, that means that it take only the value "died" or "survived"

#5#

#We will compute our decision tree
r <- rpart(survived~., data = ptitanic)
rpart.plot(r)

#Let's take a subset of the male persons in the third class of the titanic
ex <- subset(ptitanic, sex != "male" & pclass != "3rd") # exemples 
#We can see how many survived
summary(ex$survived) 

sum(ex$survived == "survived") / nrow(ex) # proportion de survivants
nrow(ex) / nrow(ptitanic) # pourcentage relat. `a tous les exemple
