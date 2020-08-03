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
