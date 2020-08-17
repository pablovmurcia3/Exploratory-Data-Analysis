################################################################################
                                # Week 4 #
################################################################################
                                # Lesson 1 # 
################################################################################


# Clustering case study 

fileUrl <- "https://github.com/thatgeeman/courses/raw/master/04_ExploratoryAnalysis/clusteringExample/data/samsungData.rda"
download.file(fileUrl, destfile = "./data/samsungData.Rda", mode = "wb")

samsungData1 <- load("./data/samsungData.Rda")

# Exploring data
names(samsungData)[1:12]
table(samsungData$activity)

samsungData[, names(samsungData) == "activity"]
grep("activity", colnames(samsungData))
grep("subject", colnames(samsungData))



# Plotting average acceleration for first subject

par(mfrow=c(1, 2), mar = c(5, 4, 1, 1))

samsungData <- transform(samsungData, activity = factor(activity))
class(samsungData$activity)


sub1 <- subset(samsungData, subject == 1)
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[1])
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[2])
legend("bottomright",legend=unique(sub1$activity),col=unique(sub1$activity), 
       pch = 1, cex = 0.5)


###  Clustering based just on average acceleration

source("myplclust.R")

distanceMatrix <- dist(sub1[,1:3])
hclustering <- hclust(distanceMatrix)
par(mfrow = c(1,1))
myplclust(hclustering, lab.col = unclass(sub1$activity))
plot(hclustering)


# Plotting max acceleration for the first subject


par(mfrow=c(1,2))
plot(sub1[,10],pch=19,col=sub1$activity,ylab=names(sub1)[10])
plot(sub1[,11],pch=19,col = sub1$activity,ylab=names(sub1)[11])

# it kind to appear a pattern between the moving activities and the non-moving ones

# Clustering based on maximum acceleration

source("myplclust.R")
distanceMatrix <- dist(sub1[,10:12])
hclustering <- hclust(distanceMatrix)

par(mfrow=c(1,1))
myplclust(hclustering,lab.col=unclass(sub1$activity))

###  Singular Value Decomposition (on entire matrix)

svd1 = svd(scale(sub1[,-c(562,563)])) # without the activity and subject column
par(mfrow=c(1,2))
plot(svd1$u[,1],col=sub1$activity,pch=19)
plot(svd1$u[,2],col=sub1$activity,pch=19)

# Find maximum contributor
par(mfrow=c(1,1))
plot(svd1$v[,2],pch=19) # second right singular vector (why?)
maxContrib <- which.max(svd1$v[,2])
colnames(sub1)[296]
svd1$v[,2][296]

plot(svd1$v[,1],pch=19) # first right singular vector (
maxContrib <- which.max(svd1$v[,1])
colnames(sub1)[281]

#  New clustering with maximum contributer

distanceMatrix <- dist(sub1[, c(10:12,maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=unclass(sub1$activity))  


###  K-means clustering (nstart=1, first try)

kClust <- kmeans(sub1[,-c(562,563)],centers=6, nstart = 100)
kClust$cluster
table(kClust$cluster,sub1$activity)

# K-means have troble separating the data by activity 
# but the problem decrease if we increase the number of nstart


#  Cluster 1 Variable Centers (Laying)

plot(kClust$center[1,1:10],pch=19,ylab="Cluster Center",xlab="") 
kClust$center[1,c(1,10)]
# first cluster -- group the walkdown activity 
# We can detected iit by positive values of tBodyAcc.mean...X and tBodyAcc.max...X


plot(kClust$center[2,1:10],pch=19,ylab="Cluster Center",xlab="")
kClust$center[2,1:3]
# second cluster -- group the laying activity 
# We can detected iit by positive values of tBodyAcc.mean...X, tBodyAcc.mean...Y
# and tBodyAcc.mean...Z 


plot(kClust$center[3,1:10],pch=19,ylab="Cluster Center",xlab="")
plot(kClust$center[4,1:10],pch=19,ylab="Cluster Center",xlab="")
plot(kClust$center[5,1:10],pch=19,ylab="Cluster Center",xlab="")
plot(kClust$center[6,1:10],pch=19,ylab="Cluster Center",xlab="")

# so.. it is usefull to predict!

