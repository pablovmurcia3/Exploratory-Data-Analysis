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
