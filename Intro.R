### Intro, Variables, Vectors, Arithmetic Operations ###
3 + 5
9.01 * 31.444

x <- 5
y <- 6
x + y

z <- x + y
z

a <- c(1,2,3) #numeric vector
b <- c(10, 11, 12)

a + b
a - b

c <- c(100, 200)
a + c # c(1,2,3) + c(100, 200, 100) (Recycling)
a*c
c/b


v <- c(1,2,3,4,5,6,7,8)
v[1]
v[2]
v[c(1,2)]
v[1:3]
v[-1]
v[-c(1,3,8)]
v[-c(1:3)]
v[8:1]


s <- "asd"
p <- c("asd", 'klm', "xyz")

mode(p) #see help for mode, typeof, and class if you are interested in the differences between them
typeof(p)
class(p)

mode(v)
typeof(v)
class(v)


setwd("C:/Users/Furkan/Desktop/lab_") #set working directory to a desired folder (usually where your input files are stored)

### Loading and Manipulating Data Frames ###
df.auto <- read.table("autompg1.csv", header = TRUE, sep = ",", dec = ".")
df.auto <- read.table(url("https://raw.githubusercontent.com/furkangursoy/MIS542-Fall18/master/autompg1.csv"),
                      header = TRUE,
                      sep = ",", 
                      dec = "."
                      ) #dividing a single command into multiple lines are possible

names(df.auto) #variables (column headers)
head(df.auto) #first several rows
summary(df.auto) # 5 point summary for numeric variables. note that, initially R treated all columns as numeric.

mode(df.auto)
class(df.auto)
typeof(df.auto)


is.data.frame(df.auto)
is.numeric(df.auto) 
is.numeric(df.auto$mpg)

is.factor(df.auto$origin) #origin should actually be factor (i.e., a nominal variable)
is.factor(df.auto$modelyear)

df.auto$originNew <- factor(df.auto$origin, levels = c("1", "2", "3"), 
                         labels = c("US", "Europe", "Japan")) #factorize the origin

levels(df.auto$originNew) #levels of the factor
is.numeric(df.auto$originNew) #it's no more numeric
is.factor(df.auto$originNew) #now, it's a factor
is.ordered(df.auto$originNew) #it is not an ordered factor (e.g., not an ordinal variable)


#discretize acceleration and transform it into an ordinal variable with 4 levels
df.auto$accelerationNew <- cut(df.auto$acceleration, breaks=c(min(df.auto$acceleration)-1,10, 15, 20,max(df.auto$acceleration))
                      , labels = c("faster", "fast", "slow","slower"))

is.factor(df.auto$accelerationNew)
is.ordered(df.auto$accelerationNew)
df.auto$accelerationNew <- ordered(df.auto$accelerationNew, c("slower", "slow", "fast", "faster") ) #specify the order
is.ordered(df.auto$accelerationNew)
head(df.auto$accelerationNew)
levels(df.auto$accelerationNew)

#everything seems fine. remove the old variables and rename the new variables.
df.auto$origin <- NULL #remove a column
df.auto$acceleration <- NULL
names(df.auto)
names(df.auto)[c(7,8)] <- c("origin", "acceleration") 
names(df.auto)

summary(df.auto)

### Some Descriptive Analyses ###
#single continuous
mean(df.auto$mpg)
sd(df.auto$mpg)
var(df.auto$mpg)
hist(df.auto$mpg)
plot(density(df.auto$mpg))
boxplot(df.auto$mpg)

#single categorical
plot(df.auto$origin)
pie(summary(df.auto$origin))

#two continuous
cor(df.auto$mpg, df.auto$horsepower)
boxplot(df.auto$mpg, df.auto$horsepower, names=c("Mileage per Gallon", "Horse Power")) #this is actually a useless plot since units are different
pairs(df.auto[,c(1,3,4)])

#two categorical
table(df.auto$acceleration, df.auto$origin)

#cont vs cat
plot(df.auto$origin, df.auto$horsepower)
aggregate(df.auto$mpg, by=list(df.auto$acceleration), FUN="mean")



### Missing values ###
nrow(df.auto)
df.auto[c(1, 19, 30, 60, 80), c(1, 3, 5)] <- NA #manually introduce missing values
df.auto[c(66, 77), c(2)] <- NA
summary(df.auto)

is.na(df.auto[1,1])
is.na(df.auto[3,3])
is.na(df.auto$mpg)
is.na(df.auto[1,])

mean(df.auto$mpg)
mean(df.auto$mpg, na.rm=TRUE)

#replace NAs with values
df.auto[is.na(df.auto$mpg),]$mpg <- 33.33 #assume that we know that missing mpg values are actually 33.33
df.auto[c(1, 19, 30, 60, 80),]$mpg

#listwise deletion
df.auto <- na.omit(df.auto)
nrow(df.auto)
mean(df.auto$weight)
mean(df.auto$weight, na.rm=TRUE)


### Basic data transformations ###
#discretization is examplified in previous sections using cut() function

#z-transformation (standardization). manual calculation.
df.auto$horsepowerZ = (df.auto$horsepower - mean(df.auto$horsepower))/sd(df.auto$horsepower)
mean(df.auto$horsepowerZ)
sd(df.auto$horsepowerZ)

#z-transformation using the built-in function
df.auto$horsepowerZ2 <- scale(df.auto$horsepower)
mean(df.auto$horsepowerZ2)
sd(df.auto$horsepowerZ2)

#Range standardization using a user-defined a function
rangeStandardize01 <- function(x){(x-min(x))/(max(x)-min(x))}
df.auto$horsepowerR01 <- rangeStandardize01(df.auto$horsepower)

summary(df.auto)


