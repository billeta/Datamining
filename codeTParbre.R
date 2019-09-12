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
