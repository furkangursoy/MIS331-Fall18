### Required libraries ###
#install.packages("psych")
#install.packages("factoextra")
#install.packages("corrplot")

library(psych) #this package is used for kaiser-meyer-olkin measure of sampling adequacy and bartlett's test of sphericity
library(factoextra) #this package is used for visualization 
library(corrplot) #this package is for for visualization


### Load the dataset ###
auto.df <- read.table("autompg1.csv", header=TRUE, sep=",") #read data from the file
names(auto.df) #see column headers (i.e., variables)
autox.df <- auto.df[,-c(1,7,8)] #create a new data frame storing only the desired variables 
names(autox.df) #check that we have all and only desired variables in our data frame


### Check suitability of dataset for PCA ###
KMO(autox.df) #kaiser-meyer-olkin measure of sampling adequacy. MSA values greater than 0.5 or 0.6 usually indicate suitability
KMO(cor(autox.df)) #see that we can obtain the same result by directly supplying correlation matrix

cortest.bartlett(autox.df) #bartlett's test of sphericity. p values smaller than 0.5 usually indicates suitability
cortest.bartlett(cor(autox.df), n = nrow(autox.df)) #see that we can obtain the same result by supplying correlation matrix and sample size 
#do not confuse with bartlett.test() which is a different test

### PCA without scaling ###
autox.pca <- prcomp(autox.df) #perform principal component analysis. see the help on this function
autox.pca
names(autox.pca)
autox.pca$sdev #the square roots of the eigenvalues 
autox.pca$rotation #loadings matrix
cor(autox.df, autox.pca$x) #correlation matrix gives similar results to loadings, but scales are different
autox.pca$center
autox.pca$scale
autox.pca$x #the rotated data (e.g., new variable values)
summary(autox.pca) #notice the very high variance explained by PC1. that's due to variation differences.

v <- apply(as.matrix(autox.df), 2, mean) #find means of each variable
meansM <- matrix(v,nrow=nrow(autox.df),ncol=length(v),byrow=TRUE) #means matrix for subtraction
head(autox.pca$x) #notice that output of this command and next command is equal
head((as.matrix(autox.df) - meansM) #mean correct and multiply with rotation matrix to obtain 'x'
     %*% autox.pca$rotation
     )



varOrig <- var(autox.df[,1]) + var(autox.df[,2]) + var(autox.df[,3]) + var(autox.df[,4]) + var(autox.df[,5]) #this an next line achieves the same result. total variance in the original data.
varOrig <- sum(apply(as.matrix(autox.df), 2, var))
varOrig
var <- autox.pca$sdev^2
sum(var) #see that this value equals to varOrig. total variance is preserved
var/sum(var)
round(var/sum(var), 4) #notice we manually achieved the proportion of variance explained.
cor(autox.pca$x) #new components are not correlated with each other. 0 correlation.


### PCA with scaling ###
autox.pca.scaled <- prcomp(autox.df, scale. = TRUE) #perform pca
autox.pca.scaled$sdev #the square roots of the eigenvalues 
autox.pca.scaled$rotation #loadings matrix
cor(autox.df, autox.pca.scaled$x) #correlation matrix gives similar results to loadings, but scales are different
autox.pca.scaled$center
autox.pca.scaled$scale
autox.pca.scaled$x #the rotated data (e.g., new variable values)
summary(autox.pca.scaled)

varOrigScaled <- sum(apply(scale(as.matrix(autox.df)), 2, var))
varOrigScaled #since we have 5 variables, the value is 5 as expected.
var <- autox.pca.scaled$sdev^2
sum(var) #equals to varOrigScaled
round(var/sum(var), 4)
cor(autox.pca.scaled$x)


### Visualizations ##
plot(autox.pca.scaled) 
fviz_eig(autox.pca.scaled)

fviz_pca_ind(autox.pca.scaled, col.ind = "cos2", # Color by the quality of representation gradient.
             cols = c("#00AFBB", "#E7B800", "#FC4E07"), label = "none" #to not show labels/ids 
             )

fviz_pca_var(autox.pca.scaled, col.var = "contrib", # Color by contributions to the PC gradient.
             cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE # Avoid text overlapping 
)

fviz_pca_biplot(autox.pca.scaled, repel = TRUE, col.var = "contrib", # Variables color gradient.
                cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                col.ind = "#696969", # Individuals color 
                label ="var" 
                )

corrplot(get_pca_var(autox.pca.scaled)$cos2, is.corr=FALSE)

### Deciding on number of components to keep ###
autox.newrep.df <- autox.pca.scaled$x[,1:2] #based on explained variances (also visually shown in scree plot, correlation plot)
head(autox.newrep.df)
cor(autox.newrep.df)

### Mapping new observations ###
autox.new <- data.frame(cbind(
  c(3,5,6),
  c(100,150,100),
  c(250,100,100),
  c(1500, 2500,3000),
  c(10,20,20)
  ))

colnames(autox.new) <- names(autox.df)
autox.new

data.new.coord <- predict(autox.pca.scaled, newdata = autox.new)
data.new.coord

p <- fviz_pca_ind(autox.pca.scaled, repel = TRUE, label="none") #Plot of existing observations
fviz_add(p, data.new.coord, color ="green") #Add new observations onto the existing plot


### Rotation ### (NOT INCLUDED IN THE LAB)
ncomp <- 2
rawLoadings <- autox.pca.scaled$rotation[,1:ncomp] %*% diag(autox.pca.scaled$sdev, ncomp, ncomp)
rawLoadings

scores <- scale(autox.pca.scaled$x[,1:ncomp]) %*% varimax(rawLoadings)$rotmat
head(scores)
ijoijl
