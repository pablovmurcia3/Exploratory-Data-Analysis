################################################################################
                                # Week 4 #
################################################################################
                                # Lesson 1 # 
################################################################################


# Clustering case study 

fileUrl <- "https://github.com/thatgeeman/courses/raw/master/04_ExploratoryAnalysis/clusteringExample/data/samsungData.rda"
download.file(fileUrl, destfile = "./data/samsungData.Rda", mode = "wb")

samsungData <- load("./data/samsungData.Rda")


# exploring data
names(samsungData)[1:12]
table(samsungData$activity)

# Plotting average acceleration for first subject