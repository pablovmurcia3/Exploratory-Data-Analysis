################################################################################
                                # Week 3 #
################################################################################
                                # Lesson 1 # 
################################################################################

# Hirarchicall Clustering

# Hierarchical clustering, as is denoted by the name, involves organizing your
# data into a kind of hierarchy
# Clusterung organizes things are close into groups


## Basic Idea
# An agglomerative approach. (From small points, to small groups, to big clusters)
# 1. Find closest two things
# 2. Put them together
# 3. Find next closest


## Requires:
# A defined Distance
# A merging approach


## Distance -- pick one that makes sense for your problem
# Euclidean distance ---- Extends to high dimentional problems
# Correlation similarity
# Binary ---- absolute sum of all the different coordinates -- context of cities


## Example

set.seed(1234) 
x <- rnorm(12, mean = rep(1:3, each = 4), 0.2) 
y <- rnorm(12, mean =  rep(c(1, 2, 1), each = 4), 0.2) 
plot(x, y, col = "blue", pch = 19, cex = 2) 
text(x + 0.05, y + 0.05, labels = as.character(1:12))

## The first step is to calculate the distance

dataFrame <- data.frame(x=x, y=y) 
dist(dataFrame) # dist function... takes a matrix and calculates the distance matrix
# It defaults to the euclidean distance
# The default distance metric used by the dist() function is Euclidean distance.

## Find the closest to points

rdistxy <- as.matrix(dist(dataFrame)) 
# Remove the diagonal from consideration 
diag(rdistxy) <- diag(rdistxy) + 100000 
# Find the index of the points with minimum distance  
ind <- which(rdistxy == min(rdistxy), arr.ind = TRUE) 

plot(x, y, col = "blue", pch = 19, cex = 2) 
text(x + 0.05, y + 0.05, labels = as.character(1:12)) 
points(x[ind[1, ]], y[ind[1, ]], col = "orange", pch = 19, cex = 2)

ind 

par(mfrow = c(1, 2)) 
plot(x, y, col = "blue", pch = 19, cex = 2, main = "Data") 
text(x + 0.05, y + 0.05, labels = as.character(1:12)) 
points(x[ind[1, ]], y[ind[1, ]], col = "orange", pch = 19, cex = 2) 

# Make a cluster and cut it at the right height 
library(dplyr) 
hcluster <- dist(dataFrame) %>% hclust 
dendro <- as.dendrogram(hcluster) 
cutDendro <- cut(dendro, h = (hcluster$height[1] + 0.00001)) 
plot(cutDendro$lower[[11]], yaxt = "n", main = "Begin building tree")
        
## Find the next closest to points

nextmin <- rdistxy[order(rdistxy)][3] 
ind <- which(rdistxy == nextmin,arr.ind=TRUE) 
ind 

par(mfrow = c(1, 1)) 
plot(x, y, col = "blue", pch = 19, cex = 2, main = "Data") 
text(x + 0.05, y + 0.05, labels = as.character(1:12)) 
points(x[ind[1, ]], y[ind[1, ]], col = "orange", pch = 19, cex = 2) 

# and on and on...

hClustering <- data.frame(x=x,y=y) %>% dist %>% hclust 
plot(hClustering)

# depending on the height ypu will have more or less clusters

### Prettier Dendrogram

source("myplclust.R")

dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
myplclust(hClustering,lab=rep(1:3,each=4),lab.col=rep(1:3,each=4))

# Here’s a function that takes the output of hclust() and color codes each of 
# the cluster members by their cluster membership

#### Merging points 

### Average linking ---  which takes the average of the coordinate values in each 
# group and measures the distance between these two averages.

### Complete linking -- is to measure the distance between two groups of points
# by the maximun distance between the two groups. That is,take all points in 
# group 1 and all points in group 2 and find the two points that are furthest
# apart–that’s the distance between the groups

# Complete merging is the default method in the hclust() function.

# While there’s not necessarily a correct merging approach for any given 
# application, it’s important to note that the resulting tree/hierarchy that you
# get can be sensitive to the merging approach that you use.


#### Using the heatmap() function

dataMatrix <- data.frame(x=x,y=y) 
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)

# Conceptually, heatmap() first treats the rows of a matrix as observations and
# calls hclust() on them, then it treats the columns of a matrix as observations
# and calls hclust() on those values. The end result is that you get a dendrogram
# associated with both the rows and columns of a matrix, which can help you to 
# spot obvious patterns in the data.
