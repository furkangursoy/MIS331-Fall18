auto.df <- read.table("autompg1.csv", header=TRUE, sep=",") #read data from the file
names(auto.df) #see column headers (i.e., variables)
autox.df <- auto.df[,-c(1,4,7,8)] #create a new data frame storing only the desired variables 
names(autox.df)
nrow(autox.df)

f1 <- acceleration ~ cylinders + displacement + weight
lm1 <- lm(f1, autox.df)
summary(lm1)
mean(lm1$residuals^2)


f2 <- acceleration ~ cylinders + displacement + weight + cylinders*displacement
lm2 <- lm(f2, autox.df)
summary(lm2)
mean(lm2$residuals^2)


f3 <- acceleration ~ cylinders + displacement + weight + cylinders*displacement + I(displacement^2)
lm3 <- lm(f3, autox.df)
summary(lm3)
mean(lm3$residuals^2)

autox.df$isUSA <- ifelse(auto.df$origin == 1, 1, 0) 
autox.df$isEUR <- ifelse(auto.df$origin == 2, 1, 0)

f4 <- acceleration ~ cylinders + displacement + weight + cylinders*displacement + I(displacement^2) + isUSA + isEUR
lm4 <- lm(f4, autox.df)
summary(lm4)
mean(lm4$residuals^2)


f5 <- acceleration ~ cylinders + weight + isUSA
lm5 <- lm(f5, autox.df)
summary(lm5)
mean(lm5$residuals^2)