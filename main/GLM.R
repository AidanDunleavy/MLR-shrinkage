# clear variables, close windows
rm(list=ls(all=TRUE))
graphics.off()

library(glmnet)
library(ACSWR)
#data("chemicaldata")

data("waterquality")
dataset <- waterquality

waterquality
## Normalize Data
normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
#write.csv(dataset,"C:\\Users\\Nrs11\\OneDrive\\Documents\\WQD.csv",row.names=FALSE)
dataset.norm <- as.data.frame(lapply(dataset[2:10], normalize))

#write.csv(dataset.norm,"C:\\Users\\Nrs11\\OneDrive\\Documents\\NormWQD.csv",row.names=FALSE)
## Splitting data into the training and test sets
#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset, SplitRatio = 0.70)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


## Test Clustering
# Using the elbow method to find the optimal number of clusters
set.seed(123)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset.norm, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(123)
kmeans = kmeans(x = dataset.norm, centers = 5, iter.max = 18, nstart = 5)
y_kmeans = kmeans$cluster

# Center Count
count.c1 = sum(kmeans$cluster==1)
count.c2 = sum(kmeans$cluster==2)
count.c3 = sum(kmeans$cluster==3)
count.c4 = sum(kmeans$cluster==4)
count.c5 = sum(kmeans$cluster==5)
center.count = cbind=c(count.c1, count.c2, count.c3, count.c4, count.c5)

# Visualizing the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Cluster'),
         xlab = '',
         ylab = '')

## C5?
#install.packages("C50")
library(C50)
labels = c("Very Good",
           "Good",
           "Neutral",
           "Poor",
           "Very Poor")
labeltest <- factor(labels, levels = c(1,2,3,4,5), labels = c("Very Good",
                                                              "Good",
                                                              "Neutral",
                                                              "Poor",
                                                              "Very Poor"))
labeltest

dataset.norm$Cluster <- labeltest
dataset.norm
names(dataset.norm$Cluster) <- "Cluster"
dataset.norm$Cluster = factor(dataset.norm$Cluster,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("Very Good",
                                         "Good",
                                         "Neutral",
                                         "Poor",
                                         "Very Poor"))

library(caTools)
set.seed(123)
split = sample.split(dataset.norm, SplitRatio = 0.70)
training_set = subset(dataset.norm, split == TRUE)
test_set = subset(dataset.norm, split == FALSE)

tree <- C5.0(x = training_set[,-8], y = training_set$Cluster)
summary(tree)
plot(tree)

# Prediction?
p1=predict(tree, test_set)
table(test_set[,8],p1)

library(caret)
cm = confusionMatrix(test_set[,8],p1); cm