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

################################################################################
                                # Lesson 2 # 
################################################################################


#### K-Means Clustering

### Requires:
# A defined Distance
# A number of clusters
# An initial guess as to cluster centroids

## Distance
# Continous - euclidean, correlation
# Binary - manhattan

### A partioning approach

# fix a number of clusters
# Get "centroids"
# assign things to closest centroid
# Recalculate the centroids


## Produces 

# Final estimate of cluster centroids
# An assignment of each point to cluster

set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

# kmeans()

dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3, iter.max  = 100) # returns a list
# REALLy IMPORTANT: iter.max number of different starting points
names(kmeansObj)

kmeansObj$cluster # Tindicates the cluster of the points in a dataframe
kmeansObj$centers # the location of the clusters in the space

plot(x, y, col = kmeansObj$cluster , pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd =3)

### Heatmaps

set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj <- kmeans(dataMatrix, centers = 3, iter.max  = 100)

# Then we can make an image plot using the K-means clusters.

par(mfrow = c(1, 2))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n", main = "Original Data")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n", main = "Clustered Data")

dataMatrix[order(kmeansObj$cluster),] # up-side down :()


################################################################################
                                # Lesson 3 # 
################################################################################

### Principal components analysis and singular value decomposition 

# Matrix data have some special statistical methods that can be applied to them. One
# category of statistical dimension reduction techniques is commonly called principal
# components analysis (PCA) or the singular value decomposition (SVD)

set.seed(12345)
dataMatrix <- matrix(rnorm(400), nrow = 40)
par(mar = c(4,4,2,2))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
?image
image(t(dataMatrix)[, nrow(dataMatrix):1]) # you need to reverse the matrix

# small investigation to order the matrix in Hirarchicall Clustering ###########
heat <- heatmap(dataMatrix)
class(heat)

rev(heat$rowInd)
heat$colInd

dataclust <- dataMatrix[rev(heat$rowInd), heat$colInd]
################################################################################

# Add a pattern 

# In the code below, we cycle through all the rows of the matrix and randomly 
# add 3 to the last 5 columns of the matrix.

set.seed(678910)
for (i in 1:40) {
        coinFlip <- rbinom(1, size = 1, prob = 0.5)
        if (coinFlip) {
                dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
                }
}

image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
heatmap(dataMatrix) # two set of column are clearly splitted

# We can display this a bit more explicitly by
# looking at the row and column means of the data.



library(dplyr)
hh <- dist(dataMatrix) %>% hclust
hh$order

source("myplclust.R")
myplclust(hh,lab=rep(1:3,each=4),lab.col=rep(1:3,each=4))


dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1, 3))

## Complete data
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])

## Show the row means
plot(rowMeans(dataMatrixOrdered), 40:1, xlab = "Row Mean", ylab = "Row", pch = 19)

## Show the column means
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)


# However, there may be other patterns beyond a simple mean shift and so more
# sophisticated methods will be needed. 

# So.. you have a very large matrix (with many variables)

# 1. The goal is to find a new set of variables/features that are uncorrelated and 
# explain as much variance in the data as possible. 

# 2.if you were to put all these multivariate observations together in one matrix, 
# find the best matrix created with fewer variables (lower rank) that explains
# the original data.

# The first goal is statistical in nature and the second goal is perhaps better
# characterized as lossy data compression. 
# (FIRST: PCA, SECOND: SVD)


# SVD

# PCA (uses SVD)

################################################################################
                                # Lesson 3 # 
################################################################################

# Plotting and color in R

# Quite often, with plots made in R, you’ll see something like the following Christmas
# themed plot.

set.seed(19)
x <- rnorm(30)
y <- rnorm(30)
plot(x, y, col = rep(1:3, each = 10), pch = 19)
legend("bottomright", legend = paste("Group", 1:3),
       col = 1:3, pch = 19, cex = 0.6)

# With image function 

par(mfrow = c(1, 2))
image(volcano, col = heat.colors(10), main = "heat.colors()")
image(volcano, col = topo.colors(10), main = "topo.colors()")

# R has a number of utilities for dealing with colors and color palettes in your 
# plots. For starters, the grDevices package has two functions:

# • colorRamp: Take a palette of colors and return a function that takes valeus 
# between 0 and 1, indicating the extremes of the color palette (e.g. see the 
# gray() function)

# • colorRampPalette: Take a palette of colors and return a function that takes
# integer arguments and returns a vector of colors interpolating the palette 
# (like heat.color or topo.colors())

# colorRamp 

pal <- colorRamp(c("red", "blue"))
# red
pal(0)

# Notice that pal is in fact a function that was returned by colorRamp(). When 
# we call pal(0) we get a 1 by 3 matrix. The numbers in the matrix will range 
# from 0 to 255 and indicate the quantities of red, green, and blue (RGB) in 
# columns 1, 2, and 3 respectively

# blue
pal(1)

# purple-ish
pal(0.5)


# You can also pass a sequence of numbers to the pal() function.

pal(seq(0, 1, len = 10))


# colorRampPalette()

# The colorRampPalette() function in manner similar to colorRamp((), 
# however the function that it returns gives you a fixed number of colors that
# interpolate the palette.

pal <- colorRampPalette(c("red", "yellow"))
# the pal() function takes an integer argument specifing the number of 
# interpolated colors to return.

# Just return red and yellow
 pal(2)

# Note that the colors are represented as hexadecimal strings. After the # 
# symbol, the first two characters indicate the red amount, the second two 
# the green amount, and the last two the blue amount. Because each position 
# can have 16 possible values (0-9 and A-F),the two positions together allow for
 # 256 possibilities per color.
 
# Return 10 colors in between red and yellow
pal(10)

#Part of the art of creating good color schemes in data graphics is to start with
# an appropriate color palette that you can then interpolate with a function 
# like colorRamp() or colorRampPalette(). One package on CRAN that contains 
# interesting and useful color palettes is the RColorBrewer6 package.

# The RColorBrewer packge offers three types of palettes: 

# • Sequential: for numerical data that are ordered
# • Diverging: for numerical data that can be positive or negative, often 
# representing deviations from some norm or baseline
# • Qualitative: for qualitative unordered data

library(RColorBrewer)
par(mfrow = c(1,1), cex= 0.4)
display.brewer.all()


# Below we choose to use 3 colors from the “BuGn” palette, which is a sequential 
# palette.

cols <- brewer.pal(3, "BuGn")
cols

# Those three colors make up my initial palette. Then I can pass them to
# colorRampPalette() to create my interpolating function.

pal <- colorRampPalette(cols)

# Now plot..
image(volcano, col = pal(20))

# The smoothScatter() function

# A function that takes advantage of the color palettes in RColorBrewer is the
# smoothScatter() function, which is very useful for making scatterplots of very
# large datasets. The smoothScatter() function essentially gives you a 2-D 
# histogram of the data using a sequential palette (here “Blues”)


set.seed(1)
x <- rnorm(10000)
y <- rnorm(10000)
plot(x,y)
smoothScatter(x, y)

# Color transparency can be added via the alpha parameter to rgb() to produce color
# specifications with varying levels of transparency. When transparency is used you’ll
# notice an extra two characters added to the right side of the hexadecimal
# representation (there will be 8 positions instead of 6).

# For example, if I wanted the color red with a high level of transparency, 
# I could specify
rgb(1, 0, 0, 0.1)

set.seed(2)
x <- rnorm(2000)
y <- rnorm(2000)
plot(x, y, pch = 19)

plot(x, y, pch = 19, col = rgb(0,0,0,0.15))
