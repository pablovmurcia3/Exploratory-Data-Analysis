library(swirl)

swirl()

dim(ssd)

names(ssd[,c(562,563)])

table(ssd$subject)

sum(table(ssd$subject))
#  We can infer that this data is supposed to train machines to recognize 
# activity collected from the accelerometers and gyroscopes built into the
# smartphones that the subjects had strapped to their waists.

table(ssd$activity)

# Because it's training data, each row is labeled with the correct activity (from the 6
# possible) and associated with the column measurements (from the accelerometer and
# gyroscope). We're interested in questions such as, "Is the correlation between the
# measurements and activities good enough to train a machine?" so that "Given a set of 561
# measurements, would a trained machine be able to determine which of the 6 activities the
# person was doing?"

sub1 <- subset(ssd, subject == 1  )

dim(sub1)

names(sub1[,1:12])

myedit("showXY.R")

showMe(1:6)

# Clustering 

mdist <- dist(sub1[,1:3])

hclustering <- hclust(mdist)

myplclust(hclustering, lab.col = unclass(sub1$activity))

#  Well that dendrogram doesn't look too helpful, does it? There's no clear 
# grouping of

#  So average acceleration doesn't tell us much. How about  maximum acceleration?

#  Finally we're seeing something vaguely interesting! Let's focus then on the 3
# dimensions of maximum acceleration

mdist <- dist(sub1[,10:12])

hclustering <- hclust(mdist)

myplclust(hclustering, lab.col = unclass(sub1$activity))

#  Now we see clearly that the data splits into 2 clusters, active and passive 
# activities.

# SVD

svd1 <- svd(scale(sub1[,-c(562,563)]))

dim(svd1$u)

# Here we're looking at the 2 left singular vectors of svd1 (the first 2 columns of
# svd1$u). Each entry of the columns belongs to a particular row with one of the 6
# activities assigned to it. We see the activities distinguished by color. Moving from
# left to right, the first section of rows are green (standing), the second red (sitting),
# the third black (laying), etc.  The first column of u shows separation of the nonmoving
# (black, red, and green) from the walking activities. The second column is harder to
# interpret. However, the magenta cluster, which represents walking up, seems separate
# from the others.

# (IMPORTANT the answer of the class)

# We'll try to figure out why that is. To do that we'll have to find which of the 500+
# measurements (represented by the columns of sub1) contributes to the variation of that
# component. Since we're interested in sub1 columns, we'll look at the RIGHT singular
# vectors (the columns of svd1$v), and in particular, the second one since the separation
# of the magenta cluster stood out in the second column of svd1$u.

maxCon <- which.max(svd1$v[, 2])

mdist <- dist(sub1[, c(10:12, maxCon)])

hclustering <- hclust(mdist)

myplclust(hclustering, lab.col = unclass(sub1$activity))

# Now we see some real separation. Magenta (walking up) is on the far left, and
# the two other walking activities, the two blues, are on the far right, but in 
# separate clusters from one another. 

names(sub1)[maxCon]

# K-means clustering

kClust <- kmeans(sub1[,-c(562,563)], centers = 6)

table(kClust$cluster, sub1$activity)


kClust <- kmeans(sub1[,-c(562,563)], centers = 6,  nstart = 100)

table(kClust$cluster, sub1$activity)

dim(kClust$centers)

# So the centers are a 6 by 561 array. Sometimes it's a good idea to look at 
# the features (columns) of these centers to see if any dominate.

laying <- which(kClust$size==29)

plot(kClust$centers[laying, 1:12],pch=19,ylab="Laying Cluster")

names(sub1[,1:3])

# So the 3 directions of mean body acceleration seem to have the biggest 
# effect on laying.

walkdown <- which(kClust$size==49)

plot(kClust$centers[walkdown, 1:12],pch=19,ylab="Walkdown Cluster")

#  We see an interesting pattern here. From left to right, looking at the 12 
# acceleration measurements in groups of 3, the points decrease in value. The X 
# direction dominates, followed by Y then Z. This might tell us something more 
# about the walking down activity.