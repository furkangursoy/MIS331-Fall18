3+5

x <- 5
y <- 7

x + y #comment

z <- x + y

9.01 * 31.444

a <- c(1,3,5,7)
a  
b <- c(1,1,1,2)  

a + b
a * b
a/b

c <- a + b
c

d <- c(100, 200)
a
a + d

e <- c(100, 200, 300)
a + e

e[1]
e[3]

s <- "a string"
s

s2 <- c("aa", "bbbb", "kkookkok")
s2

sn <- c("aa", 2)
sn

a[1:3]
a[-4]
a[-c(1,3)]
a[3:1]
a[c(1,4,2,3)]


setwd("C:/Users/Furkan/Desktop/lab")
df.auto <- read.table("autompg1.csv", sep =",", dec= ".", header=TRUE)
df.auto
edit(df.auto)

head(df.auto)
tail(df.auto)

summary(df.auto)

df.auto[1,3]
head(df.auto)
df.auto[1,]
df.auto[,3]

df.auto$cylinders
summary(df.auto$cylinders)
mean(df.auto$mpg)
sd(df.auto$mpg)

hist(df.auto$mpg)
plot(df.auto$mpg)
plot(df.auto$mpg, df.auto$horsepower)
cor(df.auto$mpg, df.auto$horsepower)
boxplot(df.auto$mpg)

summary(df.auto$origin)
df.auto$origin <- factor(df.auto$origin, levels=c("1","2","3"), labels = c("US", "Europe", "Japan"))
summary(df.auto$origin)

df.auto$acceleration <- cut(df.auto$acceleration, breaks=3, labels=c("fast", "medium", "small"))

table(df.auto$acceleration, df.auto$origin)

chisq.test(df.auto$acceleration, df.auto$origin)

T <- as.table(cbind(c(10,20), c(30, 15)))
T
chisq.test(T, correct = FALSE)

U <- cbind(c(10,20), c(30, 15))
chisq.test(U, correct=FALSE)
