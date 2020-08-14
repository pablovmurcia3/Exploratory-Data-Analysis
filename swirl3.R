library(swirl)

swirl()

install.packages("fields")

dist(dataFrame) # By default dist uses Euclidean distance as its metric, but
# other metrics such as Manhattan, are available 

# R provides a simple function which you can call which
# creates a dendrogram for you. It's called hclust() and takes as an argument the
#  pairwise distance matrix

hc <- hclust(distxy)

plot(hc)

plot(as.dendrogram(hc))
#  Notice that the vertical heights of the lines and labeling of the scale on 
# the left edge give some indication of distance

abline(h = 1.5, col = "blue")

# We see that this blue line intersects 3 vertical lines and this tells us that
# using the distance 1.5 (unspecified units) gives us 3 clusters (1 through 4), 
# (9 through 12), and (5 through 8).
            
abline(h = 0.4, col = "red")

abline(h = 0.05, col = "green")

# So the number of clusters in your data depends on where you draw the line!

# how distances between clusters of points are measured? 

# The first is called complete linkage and it says that if you're trying to 
# measure a distance between two clusters, take the greatest distance between 
# the pairs of points in those two clusters. Obviously such pairs contain one
# point from each cluster

dFsm

dist(dFsm)

# We see that the smallest distance is between points 2 and 3 in this reduced set,
# (these are actually points 8 and 11 in the original set), indicating that the two
# clusters these points represent ((5 through 8) and (9 through 12) respectively)
# would be joined (at a distance of 1.869) before being connected with the third
# cluster (1 through 4).

#  The second way to measure a distance between two clusters that we'll just mention
# is called average linkage. First you compute an "average" point in each cluster
# (think of it as the cluster's center of gravity). You do this by computing the
# mean (average) x and y coordinates of the points in the cluster.

hc

# It is a good idea to experiment with different methods of linkage to see the 
# varying ways your data groups. This will help you determine the best way to 
# continue with your analysis.

# a heat map is "a graphical representation of data where the individual values 
# contained in a matrix are represented as colors. ... Heat maps originated in 
# 2D displays of the values in a data matrix. Larger values were represented by 
# small dark gray or black squares (pixels) and smaller values by lighter squares."

# http://sebastianraschka.com/Articles/heatmaps_in_r.html#clustering

heatmap(dataMatrix, col = cm.colors(25))

heatmap(mt)

mt

plot(denmt)

distmt

################################################################################

# k-means method "aims to partition the points into k groups such that the sum 
# of squares from points to the assigned cluster centres is minimized."

# Need a distance: Euclidean

# The process stops once you reach an iteration in which no adjustments are made 
# or when you've reached some predetermined maximum number of iterations.

cmat

points(cx, cy, col = c("red", "orange", "purple"), pch = 3, cex = 2, lwd = 2)

mdist(x, y, cx, cy)

apply(distTmp, 2, which.min) # remember Apply function!!

points(x, y, pch = 19, cex = 2, col = cols1[newClust]) # Interesting annotation 

# Recalculate the centroids (by the mean of the existing clusters)

tapply(x, newClust, mean)

tapply(y, newClust, mean)

points(newCx, newCy, col = cols1, pch = 8, cex = 2, lwd = 2)

# For the new centroids, group the nearest points

mdist(x, y, newCx, newCy)

apply(distTmp2, 2, which.min)

points(x, y, pch = 19, cex = 2, col = cols1[newClust2]) 

# and again get the mean 

tapply(x, newClust2, mean)

tapply(y, newClust2, mean)

points(finalCx, finalCy, col = cols1, pch = 9, cex = 2, lwd = 2)

# The coomand that do all in one step 

# Now that you've gone through an example step by step, you'll be relieved to hear
# that R provides a command to do all this work for you. Unsurprisingly it's called
# kmeans and, although it has several parameters, we'll just mention four. These are
# x, (the numeric matrix of data), centers, iter.max, and nstart. The second of
# these (centers) can be either a number of clusters or a set of initial centroids.
# the third, iter.max, specifies the maximum number of iterations to go through, and
# nstart is the number of random starts you want to try if you specify centers as a
# number.

# 1. interesting = difference iter.max and nstart 

kmeans(dataFrame, centers = 3)

kmObj$iter

points(x, y, pch = 19, cex = 2, col = cols1[newClust2]) 



plot(x,y,col=kmObj$cluster,pch=19,cex=2)
points( kmObj$centers, col = c("black","red","green"), pch = 3, cex = 3, lwd = 3)


kmeans(dataFrame, centers = 6)

plot(x,y,col= kmeans(dataFrame,6)$cluster, pch=19,cex=2)

plot(x,y,col= kmeans(dataFrame,6)$cluster, pch=19,cex=2)

plot(x,y,col= kmeans(dataFrame,6, nstart = 10)$cluster, pch=19,cex=2) 

###############################################################################

sample(colors(),10)

pal <- colorRamp(c("red","blue")) 

pal(0)
        
pal(1)

pal(seq(0,1,len=6))


p1 <- colorRampPalette(c("red","blue"))

p1(2) # hexadecimal characters

p1(6)

0xcc

p2 <-colorRampPalette(c("red","yellow"))

p2(2)

p2(10)

showMe(p1(20))

showMe(p2(20))

showMe(p2(2))

p1

?rgb

p3 <- colorRampPalette(c("blue", "green"), alpha =.5)

p3(5)

plot(x, y, pch = 19, col = rgb(0,0.5,0.5))

plot(x, y, pch = 19, col = rgb(0,0.5,0.5, 0.3))


cols <- brewer.pal(3, "BuGn") 

# The string "BuGn" is the second last palette in the sequential display. The 3
# tells the function how many different colors we want.

showMe(cols)

pal <- colorRampPalette(cols)

showMe(pal(20))

image(volcano, col = pal(20))

image(volcano, col = p1(20))

################################################################################

# SDV: entails processes which finding subsets of variables in datasets that
# contain their essences

head(dataMatrix)

heatmap(dataMatrix)

myedit("addPatt.R")

source("addPatt.R", local = TRUE)

heatmap(dataMatrix)

# SVD

# As data scientists, we'd like to find a smaller set of multivariate variables that
# are uncorrelated AND explain as much variance (or variability) of the data as
# possible. This is a statistical approach.

mat

svd(mat)

# REMEMBER ---- the number of single values is equal to the number of lineal 
# independent vectors!!!!! (NO NO NO :( )

matu %*% diag  %*%  t(matv) 

# PCA

# Basically, PCA is a method to reduce a high-dimensional data set to its essential
# elements (not lose information) and explain the variability in the data.

# but you should know that SVD and PCA are closely related.

# scale mat, our simple example data matrix.  This means that we subtract the
# column mean from every element and divide the result by the column standard 
# deviation. 

svd(scale(mat))

# Now run the R program prcomp on scale(mat). This will give you the principal
# components of mat. 

prcomp(scale(mat)) # Notice that the principal components of the scaled matrix, 
# shown in the Rotation component of the prcomp output, ARE the columns of V

#  Thus, PCA of a scaled matrix yields the V matrix (right singular vectors) of 
# the same scaled matrix.

svd1$v[,1] 

# the first LEFT singular vector it is associated with the ROW means of the
# clustered data

# the first RIGHT singular vector it's associated with the COLUMN means of the
# clustered data

# Why were the first columns of both the U and V matrices so special? 
# Well as it happens, the D matrix of the SVD explains this phenomenon. It is
# an aspect of SVD called variance explained. 

# The diagonal entries of D are like weights for the U and V columns accounting 
# for the variation in the data. 

svd1$d

head(constantMatrix)

svd2$d

# The question is, "Can our analysis detect these patterns just from the data?"
# Let's see what SVD shows. 
# Since we're interested in patterns on columns we'll look at the first two 
# right singular vectors (columns of V) to see if they show any evidence of the
# patterns.

svd2$v[,1:2]

svd2$d

# So the first element which showed the difference between the left and right halves
# of the matrix accounts for roughly 50% of the variation in the matrix, and the
# second element which picked up the alternating pattern accounts for 18% of the
# variance.

dim(faceData)

a1 <- (svd1$u[,1] * svd1$d[1]) %*% t(svd1$v[,1])

myImage(a1)

a2 <- svd1$u[,1:2] %*% diag(svd1$d[1:2]) %*% t(svd1$v[,1:2])

myImage(a2)

myImage(svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5]))

myImage(svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10]))
