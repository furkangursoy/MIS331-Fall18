#install.packages("e1071")
library(rpart) #decision tree

# read the dataset and split it into train and test sets #
auto.df <- read.table("autompg1.csv", header=TRUE, sep=",")
auto.df$origin <- factor(auto.df$origin)


ind <- sample(1:2, nrow(auto.df), r = TRUE, pr = c(0.7, 0.3)) #approx 70% of all values will be 1 and the rest will be 0
auto.train <- auto.df[ind==1,] #select approx 70% of the rows and create the training set
auto.test <- auto.df[ind==2,] #select remaining approx 30% of the rows and create the test set


# creating formulas #
names(auto.df)
auto.formula <- origin ~ mpg + cylinders + displacement + horsepower + weight + acceleration + modelyear #specify the predictor and target variables
auto.formula <- origin ~. #practically the same with above line. includes all variables


# create a tree #
auto.tree <- rpart(auto.formula, auto.train, method="class") #train a decision tree
print(auto.tree) #see the rules of the tree
plot(auto.tree, margin = 0.2) #visualize the tree
text(auto.tree,  use.n=TRUE, all=TRUE, cex=0.5) #add labels to the tree7


auto.train.pred <- predict(auto.tree, auto.train, type="class") #predicted values for the training set
table(auto.train.pred, auto.train$origin) #the confusion matrix
auto.tree.train.accuracy <- sum(auto.train.pred == auto.train$origin)/nrow(auto.train) #overall accuracy
auto.tree.train.accuracy

auto.test.pred <- predict(auto.tree, auto.test, type="class") #predicted values for the test set
table(auto.test.pred, auto.test$origin) #the confusion matrix
auto.tree.test.accuracy <- sum(auto.test.pred == auto.test$origin)/nrow(auto.test) #overall accuracy
auto.tree.test.accuracy

# create another tree #
auto.tree2 <- rpart(auto.formula, auto.train, method="class", minsplit= 3, minbucket = 1, cp = 0.0001) #train a decision tree (which is likely to overfit due to the parameter settings)
auto.train.pred <- predict(auto.tree2, auto.train, type="class") #predicted values for the training set
table(auto.train.pred, auto.train$origin) #the confusion matrix
auto.tree2.train.accuracy <- sum(auto.train.pred == auto.train$origin)/nrow(auto.train) #overall accuracy
auto.tree2.train.accuracy

auto.test.pred <- predict(auto.tree2, auto.test, type="class") #predicted values for the test set
table(auto.test.pred, auto.test$origin) #the confusion matrix
auto.tree2.test.accuracy <- sum(auto.test.pred == auto.test$origin)/nrow(auto.test) #overall accuracy
auto.tree2.test.accuracy


#create a new tree by pruning auto.tree2
auto.tree3<- prune(auto.tree2, cp=0.02)

auto.train.pred <- predict(auto.tree3, auto.train, type="class") #predicted values for the training set
auto.tree3.train.accuracy <- sum(auto.train.pred == auto.train$origin)/nrow(auto.train) #overall accuracy
auto.tree3.train.accuracy

auto.test.pred <- predict(auto.tree3, auto.test, type="class") #predicted values for the test set
auto.tree3.test.accuracy <- sum(auto.test.pred == auto.test$origin)/nrow(auto.test) #overall accuracy
auto.tree3.test.accuracy

