##
SampleMean <- NULL
for (i in 1:100000){
  X <- rnorm(n=10, mean=10, sd=3)
  SampleMean[i] <- mean(X)
}

hist(SampleMean)
plot(density(SampleMean))
round(mean(SampleMean),2)
round(sd(SampleMean),2)

##
SampleMean <- NULL
for (i in 1:100000){
  X <- rnorm(n=5, mean=10, sd=3)
  SampleMean[i] <- mean(X)
}

hist(SampleMean)
plot(density(SampleMean))
round(mean(SampleMean),2)
round(sd(SampleMean),2)

##
SampleMean <- NULL
for (i in 1:100000){
  X <- rnorm(n=20, mean=10, sd=3)
  SampleMean[i] <- mean(X)
}

hist(SampleMean)
plot(density(SampleMean))
round(mean(SampleMean),2)
round(sd(SampleMean),2)



#######

SampleVariance <- NULL
for (i in 1:100000){
  X <- rnorm(n=3, mean=10, sd=3)
  SampleVariance[i] <- var(X)
}

cs <- (SampleVariance*2)/9
plot(density(cs))
round(mean(cs),2)
round(var(cs), 2)


########
count <- 0
for (i in 1:100000)
  {
    X <- rnorm(n=10, mean=10, sd=3)
    UCL <- mean(X) + ( 3/sqrt(10) ) * qnorm(0.975)
    LCL <- mean(X) - ( 3/sqrt(10) ) * qnorm(0.975)
    
  
    if(LCL <= 10 &&  UCL >= 10)
      {
        count <- count + 1
      }
  }
round(count/100000,2)

