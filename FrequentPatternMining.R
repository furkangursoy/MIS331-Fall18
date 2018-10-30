#install.packages("arules")
library(arules)

#read transaction from file
tdata <- read.transactions("sampletransactions.txt", sep=",")
inspect(tdata)

#Groceries data from arules package
data(Groceries)
summary(Groceries)
inspect(head(Groceries))

#frequent items & frequent items data frame
frequentItems <- apriori(Groceries, parameter = list(supp = 0.01, maxlen = 15, minlen=2, target="frequent itemsets"))
frequentItemsDF <- inspect(frequentItems)
edit(frequentItemsDF)

frequentItems <- apriori(Groceries, parameter = list(supp = 0.1, maxlen = 15, minlen=1, target="frequent itemsets")) 
frequentItemsDF <- inspect(frequentItems)
edit(frequentItemsDF)


#rules
rules1 <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.3, minlen=2)) #identify strong rules
inspect(rules1)

rules2 <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.5, minlen=2))
inspect(rules2)

rules3 <- apriori(Groceries, parameter = list(supp = 0.005, conf = 0.5, minlen=4))
inspect(rules3)

#sort by lift
rules3_lift <- sort(rules3, by="lift", decreasing=TRUE)
inspect(rules3_lift)

#filter rules
rules_wholemilk_rhs <- apriori(data=Groceries, parameter=list(supp=0.05,conf = 0.1), appearance=list(rhs="whole milk")) #select rules where right hand side is 'whole milk' 
inspect(rules_wholemilk_rhs)

