################################################################################
                                # Week 1 #
################################################################################

# Principles of Analytic Graphics

# 1. Show comparisons

# 2. Show causality, mechanism, explanation,systematic structure

# 3. Show multivariate data

# 4. Integrate evidence

# 5. Describe and document the evidence

# 6. Content,Content,Content (Content is king)

################################################################################

# Exploratory Graphs

# 1. To understand data properties 

# 2. To find patterns in data

# 3. To suggest modeling strategies 

# 4. To "debug" analyses 

# Are there any counties in the U.S. that exceed the national standard for fine 
# particle pollution?

if(!file.exists("./data")){dir.create("./data")}

fileUrl <- "https://github.com/rdpeng/courses/raw/master/04_ExploratoryAnalysis/exploratoryGraphs/PM25data.zip" 
download.file(fileUrl,destfile="./data/PM25data.zip") # Without method = curl
unzip("./data/PM25data.zip")

class <- c("numeric", "character", "factor", "numeric", "numeric") 
pollution <- read.csv("./data/avgpm25.csv", colClasses = class )

# Simple Summaries: One Dimension

#vFor one dimensional summarize,there are number of optionsi n R.

# • Five-number summary
# • Boxplots
# • Barplot
# • Histogram
# • Density plot

library(dplyr)

pollution <- arrange(pollution, pm25)

# 1
fivenum(pollution$pm25)
summary(pollution$pm25)

# quantile #####################################################################
(576*25)/100

pollution$pm25[144] #25

pollution$pm25[(576*50)/100] # 50

pollution$pm25[(576*75)/100] # 75

################################################################################

# 2

boxplot(pollution$pm25, col = "blue")
#Note that in a boxplot, the “whiskers” that stick out above and below the box 
# have a length of 1.5 times the inter-quartile range, or IQR, which is simply 
# the distance from the bottom of the box to the top of the box. Anything beyond
# the whiskers is marked as an “outlier” and is plotted separately as an individual
# point.

filter(pollution, pm25 > 15)
## maps!########################################################################
install.packages("maps")

library(maps)
map("county", "california") 
with(filter(pollution, pm25 > 15), points(longitude, latitude))
################################################################################

# 3

hist(pollution$pm25, col = "green")
rug(pollution$pm25) # show us the actual data points.

# The hist() function has a default algorithm for determining the number of bars
# to use in the histogram based on the density of the data. However, you can override
# the default option by setting the breaks argument to something else

hist(pollution$pm25, col = "green", breaks = 100) 
rug(pollution$pm25)

# Overlaying Features

boxplot(pollution$pm25, col = "blue")
abline(h = 12)

hist(pollution$pm25, col = "green")
abline(v = 12, lwd = 2) 
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)

# 3- Barplot
# The barplot is useful for summarizing categorical data

table(pollution$region) %>% barplot(col = "wheat")

barplot(table(pollution$region), col = "wheat")

# Simple Summaries: Two Dimensions and Beyond

# Multiple Boxplots
par(mfrow = c(1,1), mar = c(4, 4, 2, 1))
boxplot(pm25 ~ region, data = pollution, col = "red")
# The boxplot() function can take a formula, with the left hand side indicating 
# the variable for which we want to create the boxplot(continuous)  and the 
# right hand side indicating the variable that stratifies the left hand side 
# into categories

# Multiple histograms
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

# Scatterplots

# For continuous variables

par(mfrow = c(1,1), mar = c(4, 4, 2, 1))

with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)

# Using Colors

with(pollution, plot(latitude, pm25, col = region)) 
abline(h = 12, lwd = 2, lty = 2)


# Multiple Scatterplots

par(mfrow = c(1, 2), mar = c(5, 4, 2, 1)) 
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West")) 
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))
