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
 # you need to reverse the matrix

################################################################################
# small investigation to order the matrix in Hirarchicall Clustering 
################################################################################
# two graphical representations -- heatmap and image
heat <- heatmap(dataMatrix)
class(heat)

rev(heat$rowInd)
heat$colInd

dataclust <- dataMatrix[rev(heat$rowInd), heat$colInd]

# dendogram 
library(dplyr)
hi <- dist(dataMatrix) %>% hclust
hi$order
order <- dataMatrix[hh$order, ]
par(mfrow = c(1,2))
image(t(dataMatrix)[, nrow(order):1])
image(t(order)[, nrow(order):1])
################################################################################
#  tehe end 
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

# Principal components analysis (PCA) is simply an application of the SVD. The principal
# components are equal to the right singular values if you first scale the data 
# by subtracting the column mean and dividing each column by its standard 
# deviation (that can be done with the scale() function).


# SVD --- to do PCA

svd1 <- svd(scale(dataMatrixOrdered))

# The svd() function returns a list containing three components named u, d, 
# and v. The u and v components correspond to the matrices of left and right 
# singular vectors, respectively, while the d component is a vector of singular
# values, corresponding to the diagonal of the matrix D described above.

# Below we plot the first left and right singular vectors along with the 
# original data.

par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1], main = "Original Data")
plot(svd1$u[, 1], 40:1, ylab = "Row", xlab = "First left singular vector", pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)

# If we believed that the first left and right singular vectors, call them u1
# and v1, captured all of the variation in the data, then we could approximate 
# the original data matrix with a compressed matrix

# Approximate original data with outer product of first singular vectors
approx <- with(svd1, outer(u[, 1], v[, 1]))
# Plot original data and approximated data
par(mfrow = c(1, 2))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1], main = "Original Matrix")
image(t(approx)[, nrow(approx):1], main = "Approximated Matrix")


# Obviously, the two matrices are not identical, but the approximation seems
# reasonable in this case. This is not surprising given that there was only one
# real feature in the original data.


### Components of the SVD - Variance explained

# The statistical interpretation of singular values is in the form of variance 
# in the data explained by the various components. The singular values produced
# by the svd() are in order from largest to smallest and when squared are
# proportional the amount of variance explained by a given singular vector.

par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained",
     pch = 19)

# We can see that the first component explains about 40% of all the variation in
# the data. In other words, even though there are 10 dimensions in the data, 40%
# of the variation in the data can be explained by a single dimension. That
# suggests that the data could be simplified quite a bit, a phenomenon we
# observed in the last section where it appeared the data could be reasonably 
# approximated by the first left and right singular vectors.

### Relationship to principal components

# As we mentioned above, the SVD has a close connection to principal components
# analysis (PCA). PCA can be applied to the data by calling the prcomp() function 
# in R. Here, we show that the first right singular vector from the SVD is equal 
# to the first principal component vector returned by PCA.

svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1",
        ylab = "Right Singular Vector 1")
abline(c(0, 1))


### Components of the SVD - Variance explained


# To show how this works, here’s a very simple example. First, we’ll simulate a 
# “dataset” that just takes two values, 0 and 1.

constantMatrix <- dataMatrixOrdered * 0

for (i in 1:dim(dataMatrixOrdered)[1]) {
        constantMatrix[i, ] <- rep(c(0, 1), each = 5)
        }

# Then we can take the SVD of this matrix and show the singular values as well 
# as the proportion of variance explained.

svd1 <- svd(constantMatrix)
par(mfrow = c(1, 3))
image(t(constantMatrix)[, nrow(constantMatrix):1], main = "Original Data")
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained",
        pch = 19)

# As we can see from the right-most plot, 100% of the variation in this “dataset”
# can be explained by the first singular value. Or, all of the variation in this
# dataset occurs in a single dimension. This is clear because all of the 
# variation in the data occurs as you go from left to right across the columns. 
# Otherwise, the values of the data are constant.

 
# Situation with two patterns

set.seed(678910)
for (i in 1:40) {
        coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
        coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
        if (coinFlip1) {
                ## Pattern 1
                        dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)
                        }
        if (coinFlip2) {
                ## Pattern 2
                        dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)
                        }
        }
library(dplyr)
hh <- dist(dataMatrix) %>% hclust        
dataMatrixOrdered <- dataMatrix[hh$order, ]

# and the we plot it 

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1], main = "Data")
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1", main = "Block pattern")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2", main = "Alternating pattern")

# this is the truth 
# Svd ---------- can pick up both "patterns"

# So, lets do it!

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
# plot the right singular vector = principal component vect or  
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[, 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")



plot(svd2$u[, 1], pch = 19, xlab = "Column", ylab = "First left singular vector")
plot(svd2$u[, 2], pch = 19, xlab = "Column", ylab = "Second left singular vector")

# When we look at the variance explained, we can see that the first singular 
# vector picks up a little more than 50% of the variation in the data.

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column",
     ylab = "Percent of variance explained",
     pch = 19)


## Dealing with missing values

dataMatrix2 <- dataMatrixOrdered
# Randomly insert some missing data
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA

svd1 <- svd(scale(dataMatrix2)) # error


# using package impute:  do a k-nearest-neighbors imputation of the missing data
BiocManager::install(c("impute"))
library("impute")

dataMatrix2 <- impute.knn(dataMatrix2)$data

#  compare how the SVD performs on the original dataset (no missing data)
# and the imputed dataset. 
svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow = c(1, 2))
plot(svd1$v[, 1], pch = 19, main = "Original dataset")
plot(svd2$v[, 1], pch = 19, main = "Imputed dataset")


# Example: Face data


fileUrl <- "https://github.com/rdpeng/courses/raw/master/04_ExploratoryAnalysis/clusteringExample/data/face.rda"
download.file(fileUrl, destfile = "./data/faces.rda", mode = "wb")

load("./data/face.rda")
par(mfrow =c(1,1), mar = c(4,4,2,2))
image(t(faceData)[, nrow(faceData):1])

svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "Singular vector", ylab = "Variance explained")

# Note that %*% is matrix multiplication Here svd1$d[1] is a constant
approx1 <- svd1$u[, 1] %*% t(svd1$v[, 1]) * svd1$d[1]
 
# In these examples we need to make the diagonal matrix out of d
approx5 <- svd1$u[, 1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[, 1:5])
approx10 <- svd1$u[, 1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[, 1:10])

par(mfrow = c(1, 4))
image(t(approx1)[, nrow(approx1):1], main = "1 vector")
image(t(approx5)[, nrow(approx5):1], main = "5 vectors")
image(t(approx10)[, nrow(approx10):1], main = "10 vectors")
image(t(faceData)[, nrow(faceData):1], main = "Original data")

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
