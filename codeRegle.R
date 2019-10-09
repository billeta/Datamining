#dependencies
library(arules)
library(arulesViz)

#Load the data
data(Groceries)

#Check the class of the data
class(Groceries)
#the data have a transaction format

#We can inspect the two first transactions
inspect(head(Groceries, 2))

#We can generate a rule with a support of 1% and the confidence 0.5
rules <- apriori(Groceries,
                 parameter = list(support = 0.01, confidence = 0.5))
#minval: is the minimum value of the support an itemset should be to be part of the rule
#smax: is the maximum support value for an itemset
#arem: in this case we use confidence and minval as rules but arem can help in creating other types of rules
#aval: wether or not return the the additional rule value of the arem
#original support: ways of calculating support (only LHS and RHS if true)
#maxtime: max time allowed to check for subset
#minlen: minimum items required in the rule
#maxlen: maximum items required in the rule

#Show the three rules with the highest confidence
inspect(head(sort(rules, by = "confidence"), 3))

#Show the 5 rules with the highest lift value
inspect(head(sort(rules, by = "lift"), 5))

#We can try to increase the support
rules_increased_support <- apriori(Groceries, parameter = list(support = 0.02, confidence = 0.5))

#We can try to remove shorter rules
rules_increased_minlen <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.2, minlen = 3))

#To look at the most frequent items
frequentItems <- eclat(Groceries,
                       parameter = list(supp = 0.07, maxlen = 15))

#Check the frequency table
inspect(frequentItems)

#Look at the same thing but without the useless subset
#Computing the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001,
                                             conf = 0.5, maxlen=3))
#Removing short rules
subsetRules <- which(colSums(is.subset(rules, rules)) > 1)
#get the number of subsets
length(subsetRules)
#Finally, get the rules of this subset
rules <- rules[-subsetRules]

#getting the rules to restrict the rules to a parcular items, in this cas whole milk
rules <- apriori (data = Groceries, parameter = list(supp = 0.001,conf = 0.08),
                  appearance = list(default = "lhs", rhs = "whole milk"), #only on the Right Hand Side
                  control = list(verbose = FALSE))
#Then we look at the rules by confidence
rules.conf <- sort (rules, by = "confidence", decreasing = TRUE)
inspect(head(rules.conf))

#Now we want to now when an item has been taken with other
rules <- apriori (data = Groceries, parameter = list(supp = 0.001, conf = 0.15,
                                                     minlen=2),
                  appearance = list(default = "rhs",lhs = "whole milk"), #We look at the lhs in this case
                  control = list(verbose = FALSE))
#Sorting again by confidence
rules.conf <- sort (rules, by = "confidence", decreasing = TRUE)
inspect(head(rules.conf))
#It means that 736 times a whole milk has been bought in addition with other vegetables

#We take back the rules for the whole milk, as we did few lines ago
rules <- apriori (data = Groceries, parameter = list(supp = 0.01, conf = 0.5),
                  appearance = list(default = "lhs" , rhs = "whole milk"),
                  control = list(verbose=FALSE))
#We can plot the different roles with their confidence and support
plot(rules)
#Then we look at which items is highly related with whole milk
plot(rules, method = "graph", control = list(type = "items"))
#it gives a parallel plot
plot(rules, method = "paracoord", control = list(reorder = TRUE))





