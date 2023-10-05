
# SED612, SIT@KMUTT
# Aug 2022
# By PM

# Clustering with k-means, using Iris dataset

# Use the set.seed() function to guarantee 
# that the results are reproducible, e.g., when doing k-means
set.seed(1234)

# use the provided Iris dataset, built-in herein
data("iris")

# check its structure and dimension
str(iris)
dim(iris)

# use some columns, remove "species" the 5th column because
# it will not be used in clustering; k-means is unsupervised learning
data_all <- iris[,-c(5)]

# call a k-means algorithm
km <- kmeans(data_all, centers = 3)

# show center of each cluster
km$centers
# show the obtained clusters
km$cluster
# summary of frequency of each cluster
table(km$cluster)
# or we could use
km$size


# nullify the set seed
set.seed(NULL)
# let's try to improve the clustering
# use some columns, supposedly good features: petal's length and width
df <- iris[, c(3,4)]

# create 3 clusters
# nstart is the number of initial configurations, set to 10-25
# The nstart parameter allows you to specify the number
# of random starts to try. For example, the following code 
# runs K-means to find 3 clusters using 10 different starting 
# cluster means. The function automatically returns the best solution 
# out of the 10 different starting points of means

km <- kmeans(df, centers = 3, nstart = 10) 
km$cluster
# summary of the frequency of each cluster
# and you will see a better accuracy
table(km$cluster)

# import the library for plotting
library(ggplot2)
# assign each cluster (obtained from k-means) to each data point
#Add Column in df
df$cluster <- factor(km$cluster)
# let put the Iris species back in there as well, for comparison/accuracy
#Add Column in df
df$species <- iris$Species
# also assign each cluster the mean/centroid
centers <- data.frame(cluster=factor(1:3), km$centers)
# let's try to plot the clusters
ggplot(data=df, aes(x=Petal.Length, y=Petal.Width, color=cluster, shape=cluster)) + 
  geom_point(alpha=0.35, size= 3) +
  geom_point(data=centers, aes(x=Petal.Length, y=Petal.Width), size=5, stroke=2) +
  ggtitle("Different clusters of iris flowers")


# try to find k using a simple method
# use some columns, supposedly good features: petal's length and width
df <- iris[, c(3,4)]
wss <- NULL
for (i in 1:10){
  km <- kmeans(df, centers = i)
  wss = c(wss, km$tot.withinss)
}
plot(1:10, wss, type = "o",main = "The elbow method",sub = "Dataset: iris flowers",
     xlab = "Number of clusters",col="red")
wss

# now try to use a more detailed one.
# also try to see which k is good enough
# k from 2 to 6
k_val <- 2:6
total_within_ss <- double(length = 0)
# let's try to run k-means for 100 times
times <- 100
cat(times)
# run k-means for each k
for(kv in k_val){
  within_ss_each_k <- double(length = times)
  # run many times of k-means for each k
  for(i in 1:times){
    km_temp <- kmeans(df,centers = kv, nstart = 10)
    within_ss_each_k[i] <- km_temp$tot.withinss
  }
  # find an average
  avg_total_within_ss <- mean(within_ss_each_k)
  # append the values
  total_within_ss <- append(total_within_ss, avg_total_within_ss) 
}


# more or less you will see an elbow plot which
# helps you pick your best k

plot(k_val,total_within_ss,type="b", main="Iris data: total Within SS by various k",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K",
     col= "magenta")




# Now, let's try to see some performance metrics.

# Based on the preceding code, modify it, find accuracy, sensitivity, 
# specificity, and precision of each Iris flower type.


# let's try to find accuracy, first set a seed for reproducing
# Note that there is not real usual confusion matrix in k-means
# as it is found in the supervised learning methods such as k-NN.
set.seed(1234)
# use some columns, supposedly good features: petal's length and width
df <- iris[, c(3,4)]
# let's now use k = 3 
km <- kmeans(df, centers = 3, nstart = 10) 
km$cluster
# summary of the frequency of each cluster
# and you will see a better accuracy
table(km$cluster)

# First, relabel the data with the cluster number
df$cluster <- km$cluster
# also put the known species back (be careful here, the labels are randomly
# given by the program, each time they may not be the same; however, we have
# used "set.seed function here, so it is just fine for this example)
df$species <- iris$Species
for (i in 1:length(iris$Species)){
  if (df$cluster[i] == 3){
    df$label[i] = "setosa"
  } else if (df$cluster[i] == 1){
    df$label[i] = "versicolor"
  } else {
    df$label[i] = "virginica"
  }
}

# compare
cmp <- (df$species == df$label)
cmp
# calculate the accuracy score
mean(cmp)

# or we could as follows
# using a cross table
table(df$species, df$label)
val <- table(df$species, df$label)
acc <- sum(diag(val))/sum(val)
acc
#accuracy
