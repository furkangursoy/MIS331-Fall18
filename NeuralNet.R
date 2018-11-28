#install.packages("neuralnet")
library(neuralnet)

setwd("C:/Users/admin2/Desktop/lab") #set the working directory

# read the dataset and split it into train and test sets #
auto.df <- read.table("autompg1.csv", header=TRUE, sep=",")

auto.df$isUSA <- ifelse(auto.df$origin == 1, 1, 0) #differently from the previous models, we only want to predict whether produced in US or not. so, a new variable is created both for training and test sets.
auto.df$isEUR <- ifelse(auto.df$origin == 2, 1, 0) #differently from the previous models, we only want to predict whether produced in US or not. so, a new variable is created both for training and test sets.
auto.df$isJAP <- ifelse(auto.df$origin == 3, 1, 0) #differently from the previous models, we only want to predict whether produced in US or not. so, a new variable is created both for training and test sets.

auto.df$isUSA

auto.formula <- isUSA + isEUR + isJAP ~ mpg + cylinders + displacement + horsepower + weight #we only want to use these predictors and not all

names(auto.df)

max <- NULL
min <- NULL
for(i in 1:5){
  max[i] <- max(auto.df[,i])
  min[i] <- min(auto.df[,i])
}

autoRT <- cbind(auto.df) #RT: range transformed. a name we give.
for(i in 1:5){
  autoRT[i] <- (auto.df[i] - min[i])/(max[i] - min[i])
}

summary(autoRT)

ind <- sample(1:2, nrow(autoRT), r = TRUE, pr = c(0.7, 0.3)) #approx 70% of all values will be 1 and the rest will be 0
auto.train <- autoRT[ind==1,] #select approx 70% of the rows and create the training set
auto.test <- autoRT[ind==2,] #select remaining approx 30% of the rows and create the test set


# NEURAL NETWORKS #
auto.nn22 <- neuralnet(auto.formula, auto.train, hidden =c(2,2), linear.output = FALSE, stepmax = 1e+06) #build a neural network with 2 hidden layers and 2 nodes in each hidden layer
plot(auto.nn22) #visualize the neural networks
compute(auto.nn22, auto.test[,1:5])
nn22.pred <- max.col(compute(auto.nn22, auto.test[,1:5])$net.result) #neural network predictions (continuous values)
nn22.pred #our class predictions
nn22.test.accuracy <- sum(nn22.pred == auto.test$origin) / nrow(auto.test) #overall accuracy for the test set
nn22.test.accuracy  #compare with the other neural network's accuracy

nn22.pred.train <- max.col(compute(auto.nn22, auto.train[,1:5])$net.result) #neural network predictions (continuous values)
nn22.train.accuracy <- sum(nn22.pred.train == auto.train$origin) / nrow(auto.train) #overall accuracy for the test set
nn22.train.accuracy


auto.nn232 <- neuralnet(auto.formula, auto.train, hidden =c(2,3,2), linear.output = FALSE, stepmax = 1e+08) #build a neural network with 2 hidden layers and 2 nodes in each hidden layer
plot(auto.nn232) #visualize the neural networks
compute(auto.nn232, auto.test[,1:5])
nn232.pred <- max.col(compute(auto.nn232, auto.test[,1:5])$net.result) #neural network predictions (continuous values)
nn232.pred #our class predictions
nn232.test.accuracy <- sum(nn232.pred == auto.test$origin) / nrow(auto.test) #overall accuracy for the test set
nn232.test.accuracy  #compare with the other neural network's accuracy

nn232.pred.train <- max.col(compute(auto.nn232, auto.train[,1:5])$net.result) #neural network predictions (continuous values)
nn232.train.accuracy <- sum(nn232.pred.train == auto.train$origin) / nrow(auto.train) #overall accuracy for the test set
nn232.train.accuracy

## neural network for numeric prediction ##
# this example does not make use of training and test sets. you can see how to use training and test sets in previous sections of this document.
auto.df2 <- read.table("autompg1.csv", header=TRUE, sep=",")
auto.data <- as.data.frame(scale(auto.df2[,1:4])) #choosing only the variables we want and standardizing the variables. we previously transformed inputs to a fixed range but standardization is usually considered an acceptable method as well.
names(auto.data)
auto.formula2 <- mpg ~ cylinders + displacement + horsepower #we only want to use these predictors and not all
mpg.nn <- neuralnet(auto.formula2, auto.data, hidden =c(3,3), stepmax = 1e+08) #build a neural network with 2 hidden layers and 2 nodes in each hidden layer
plot(mpg.nn)
mpg.nn$net.result #predictions
sum((auto.data$mpg - mpg.nn$net.result[[1]])^2) #sum of squared errors. note that it is for standardized values.
