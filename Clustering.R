#install.packages("cluster")
#install.packages("fpc")

# required packages #
library(cluster)
library(fpc)

# data preparation #
auto.df <- read.table("autompg1.csv", header=TRUE, sep=",") #read data from the file
names(auto.df) #see column headers (i.e., variables)
autox.df <- auto.df[,-c(1,7,8)] #create a new data frame storing only the desired variables 

# k-means #
auto.k3m <- kmeans(autox.df, 3, i=50) #run k-means with number of clusters: 3, maximum iterations:50
auto.k3m
names(auto.k3m) 
auto.k3m$size #cluster sizes
auto.k3m$centers #cluster centers
auto.df$cls_k3m <- auto.k3m$cluster #cluster assignments
table(auto.df$origin, auto.df$cls_k3m) #compare found clusters with manufacturing locations of cars

autox.scaled.df <- scale(autox.df) #z-transform

auto.scaled.k3m <- kmeans(autox.scaled.df, 3, i=50) #the same procedure, this time with the scaled data
auto.scaled.k3m
auto.df$cls_k3m_scaled <- auto.scaled.k3m$cluster
table(auto.df$origin, auto.df$cls_k3m_scaled)

table(auto.df$cls_k3m, auto.df$cls_k3m_scaled) #compare scaled vs non-scaled k-means results

# calculate distance matrix #
autox.scaled.dist <- dist(autox.scaled.df, method = "euclidean") #calculate distance matrix

# k-medoids #
pam(autox.scaled.dist, 3) #partioning around medoids. accepts either a distance matrix or the data itself
pam(autox.scaled.df, 3)

clara(autox.scaled.df, 3) #clara. similar to pam but suitable for larger datasets. accepts the data itself

pamk(autox.scaled.df, 2:10) #find number of clusters and perform pam (or clara if usepam=FALSE)
pam(autox.scaled.df, 2) #see that result is the same as above line

# hierarchical clustering #
auto.hclust <- hclust(autox.scaled.dist, method = "complete") #Hierarchical cluster analysis with specified method
plot(auto.hclust) #the dendogram

auto.df$cls_hc3 <- cutree(auto.hclust, 3) #cut the h. clustering tree into 3 clusters
auto.df$cls_hc5 <- cutree(auto.hclust, 5) #cut the h. clustering tree into 5 clusters

table(auto.df$cls_hc3, auto.df$cls_hc5) #compare assignments of 3-cluster solution with 5-cluster solution

# density-based clustering #
auto.db <- dbscan(autox.scaled.df, eps=0.5,  MinPts = 3) #see help for details
auto.db
auto.db$cluster

