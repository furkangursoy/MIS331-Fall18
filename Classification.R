#install.packages("rpart") 
#install.packages("class")
#install.packages("e1071")
#install.packages("adabag")

library(rpart) #decision tree
library(class) #k-nn classifier
library(e1071) #naive bayes classifier
library(adabag) #bagging and boosting with rpart


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



# create a k-nn classifier model #
auto.knn <- knn(train=auto.train, test=auto.test, cl = auto.train$origin) #predicted values for the test set
table(auto.knn, auto.test$origin) 
knn.accuracy <- sum(auto.knn == auto.test$origin) / nrow(auto.test) #overall accuracy
knn.accuracy

auto.knn3 <- knn(train=auto.train, test=auto.test, cl = auto.train$origin, k=3) #predicted values for the test set
knn3.accuracy <- sum(auto.knn3 == auto.test$origin) / nrow(auto.test) #overall accuracy
knn3.accuracy

auto.knn10 <- knn(train=auto.train, test=auto.test, cl = auto.train$origin, k=10, l=6) #predicted values for the test set
knn10.accuracy <- sum(auto.knn10 == auto.test$origin, na.rm=TRUE) / nrow(auto.test) #overall accuracy. remove NAs
knn10.accuracy


# create a naive bayes classifier #
auto.bayes <- naiveBayes(auto.formula, auto.train) #train the model
auto.bayes
auto.bayes.pred <- predict(auto.bayes, auto.test) #predictions for the test set
table(auto.bayes.pred, auto.test$origin) #the confusion matrix
nb.accuracy <- sum(auto.bayes.pred == auto.test$origin) / nrow(auto.test) #overall accuracy for the test set
nb.accuracy


# simple tree for comparison with bagging, boosting #
auto.rpart <- rpart(auto.formula, auto.train) #see the relevant section above for details on rpart
auto.rpart.pred <- predict(auto.rpart, auto.test, type="class")
rpart.accuracy <- sum(auto.rpart.pred == auto.test$origin) / nrow(auto.test)
rpart.accuracy #compare this with accuracy scores of other methods

# bagging with rpart #
auto.bag <- bagging(auto.formula, auto.train, mfinal=10) #create 10 trees
names(auto.bag)
auto.bag$trees #list of trees
auto.bag$votes #vote counts for each class. each rows sums up to 10.
auto.bag$prob #normalized votes
auto.bag$class #predicted class based on majority vote
auto.bag.pred <- predict(auto.bag, auto.test) #predictions for test set
bag.accuracy <- sum(auto.bag.pred$class == auto.test$origin) / nrow(auto.test) #overall accuracy for the test set
bag.accuracy #compare this with accuracy scores of other methods

# boosting with rpart #
auto.boost <- boosting(auto.formula, auto.train, mfinal = 10) #see the comments for bagging. interpretations are very similar
names(auto.boost)
auto.boost$trees
auto.boost$votes
auto.boost$weights
auto.boost$prob
auto.boost$class
auto.boost.pred <- predict(auto.boost, auto.test)
boost.accuracy <- sum(auto.boost.pred$class == auto.test$origin) / nrow(auto.test) #overall accuracy for the test set
boost.accuracy #compare this with accuracy scores of other methods

