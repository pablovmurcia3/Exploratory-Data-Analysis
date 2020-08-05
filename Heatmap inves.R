data <- read.csv("./data/toheatmap.txt")
names(data)[1] <- "id"
rnames <- data[,1]

# One technique
mat_data <- data.matrix(data[,2:ncol(data)])
rownames(mat_data) <- rnames

install.packages("gplots")
library(gplots)

heatmap.2(mat_data)


# Other technique (coursera)

data <- data.matrix(data[,2:ncol(data)])
heatmap(data)


# exploration -- Dendogram column-wise


dist(rbind(x, y))

x <- data[,3]

y <- data[,4]


dist(rbind(x, z))

z <- data[,2]

c <- data[,1]

dist(rbind(x, c))

### dendogram

distxy <- dist(rbind(y, x , z, c))

hClustering <- hclust(distxy)

plot(hClustering)
