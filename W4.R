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


################################################################################
                                # Lesson 1 # 
################################################################################

# Download
fileUrl <- "https://github.com/bcaffo/courses/raw/master/04_ExploratoryAnalysis/CaseStudy/pm25_data.zip"
download.file(fileUrl, destfile = "./data/EPA.zip", mode="wb")
unzip(zipfile = "./data/EPA.zip")

# Read
pm0 <- read.table("./pm25_data/RD_501_88101_1999-0.txt", comment.char = "#", 
                  header = FALSE,  sep = "|", na.strings = "")
pm1 <- read.table("./pm25_data/RD_501_88101_2012-0.txt", comment.char = "#", 
                  header = FALSE,  sep = "|", na.strings = "")

cnames <- readLines("./pm25_data/RD_501_88101_1999-0.txt",1)
cnames

cnames <- strsplit(cnames, "|", fixed = TRUE)
cnames
names(pm0) <- cnames[[1]]
names(pm1) <- cnames[[1]]

names(pm0) <- make.names(cnames[[1]])
names(pm1) <- make.names(cnames[[1]])

str(pm0)
str(pm1)

# Missing values 

mean(is.na(pm0$Sample.Value)) # 11% of the data is Na
mean(is.na(pm1$Sample.Value)) # 5% of the data is Na

# Some summary 

summary(pm1$Sample.Value)
summary(pm0$Sample.Value)

# Some basic graphics

boxplot(pm0$Sample.Value, pm1$Sample.Value)
boxplot(log10(pm0$Sample.Value), log10(pm1$Sample.Value))

# Negative vales

which(pm1$Sample.Value<0)
n <- pm1$Sample.Value[which(pm1$Sample.Value<0)]

# or

negative <- pm1$Sample.Value<0
sum(negative, na.rm = TRUE)
mean(negative, na.rm = TRUE)

# Dates

dates <- pm1$Date

str(dates)

dates <- as.character(dates)

dates <- as.Date(dates, "Y%m%d")
str(dates) # it doesnt work  .....................


# or 

str(pm1$Date)

library(lubridate)

dates <- ymd(pm1$Date)

str(dates)

hist(dates, "month")

hist(dates[negative], "month")


## We will use only one monitor 


site0 <- unique(subset(pm0, State.Code ==36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code ==36, c(County.Code, Site.ID)))

site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")


# intersect function 

both <- intersect(site0, site1)

# pick the monitor with a large number of observations

pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))

cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

tapply(cnt0$POC, cnt0$county.site, length)
tapply(cnt1$POC, cnt1$coun, length)

# we should select the county-monitor 63.2008

pm1sub <- subset(pm1, State.Code == 36 & County.Code ==  63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code ==  63 & Site.ID == 2008)

dim(pm0sub)
dim(pm1sub)

## Time series

library(lubridate)
pm1sub$Date <- ymd(pm1sub$Date)
pm0sub$Date <- ymd(pm0sub$Date)

par (mfrow =c(1,2), mar = c(4,4,2,1))
plot(pm0sub$Date, pm0sub$Sample.Value)
abline(h = median(pm0sub$Sample.Value, na.rm = T))
plot(pm1sub$Date, pm1sub$Sample.Value, pch = 20)
abline(h = median(pm1sub$Sample.Value, na.rm = T))

# we need to standardize the range of the y axis

rng <- range(pm0sub$Sample.Value, pm1sub$Sample.Value, na.rm = T)

par (mfrow =c(1,2), mar = c(4,4,2,1))
plot(pm0sub$Date, pm0sub$Sample.Value, pch = 20, ylim = rng)
abline(h = median(pm0sub$Sample.Value, na.rm = T))
plot(pm1sub$Date, pm1sub$Sample.Value, pch = 20, ylim = rng)
abline(h = median(pm1sub$Sample.Value, na.rm = T))

      
## Now we are gonna look individual states 

# My try 

library(dplyr)

pm1mbs <- pm1  %>% group_by(State.Code)  %>%
        summarise(meansv = mean (Sample.Value, na.rm = TRUE))

pm0mbs <- pm0  %>% group_by(State.Code)  %>%
        summarise(meansv = mean (Sample.Value, na.rm = TRUE))


pm1mbs$year = rep(2012)
pm0mbs$year = rep(1999)


final <- rbind(pm1mbs, pm0mbs)

library(lattice)

xyplot(meansv ~ year | factor(State.Code), 
       panel = function(x , y, ...) {
        panel.xyplot(x,y, ...) # first call the default panel function for xyplot
        panel.lmline(x,y, col = 1) # Add horizontal line at the median
        },
       pch = 20,
       lty = 4,
       data = final)


xyplot(meansv ~ year | factor(State.Code), 
       pch = 20,
       type = "b",
       lty = 1,
       data = final)


# In video

mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))

d0 = data.frame(state = names(mn0), mean = mn0)
d1 = data.frame(state = names(mn1), mean = mn1)

mrg <- merge(d0, d1, by = "state")


par(mfrow = c(1,1))
with(mrg, plot(rep(1999,52), mrg[,2], xlim = c(1998,2013),
               ylim = range (mrg$mean.x, mrg$mean.y)))
with(mrg, points(rep(2012,52), mrg[,3]))
segments(rep(1999,52), mrg[,2], rep(2012,52), mrg[,3])
     
         