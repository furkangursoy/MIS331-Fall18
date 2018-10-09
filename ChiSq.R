### Chi-Square Test of Independence ###

## First example ##
setwd("C:/Users/Furkan/Desktop/lab")
df.auto <- read.table("autompg1.csv", sep =",", dec= ".", header=TRUE)

df.auto$origin <- factor(df.auto$origin, levels=c("1","2","3"), labels = c("US", "Europe", "Japan")) #factorize
df.auto$acceleration <- cut(df.auto$acceleration, breaks=3, labels=c("fast", "medium", "small")) #discretize

table(df.auto$acceleration, df.auto$origin) #contingency table
chisq.test(df.auto$acceleration, df.auto$origin) # interpret the X-squared statistics

## Second example ##
T <- as.table(cbind(c(10,20), c(30, 15))) #create a table
T

chisq.test(T, correct = FALSE) #correction is TRUE by default for 2x2 tables

