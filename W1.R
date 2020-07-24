################################################################################
                                # Week 1 #
################################################################################
                               # Lesson 1 # 
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

#For one dimensional summarize, there are number of optionsi n R.

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

par(mfrow = c(1,1), mar = c(3, 3, 2, 2))
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

with(pollution, plot(latitude, pm25, pch = 20))

abline(h = 12, lwd = 2, lty = 2)

# Using Colors

with(pollution, plot(latitude, pm25, col = region)) 
abline(h = 12, lwd = 2, lty = 2)


# Multiple Scatterplots

par(mfrow = c(1, 2), mar = c(5, 4, 2, 1)) 
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West")) 
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))

################################################################################
                                # Lesson 2 # 
################################################################################

# There are three different plotting systems in R and they each have different 
# characteristics and modes of operation. They three systems are the base plotting 
# system, the lattice system, and the ggplot2 system


# The base plotting system 

# The basic model is sometimes referred to as the “artist’s palette” model. The
# idea is you start with blank canvas and build up from there. 

# In more R-specific terms, you typically start with plot function (or similar 
# plot creating function) to initiate a plot and then annotate the plot with 
# various annotation functions (text, lines, points, axis) 


data(airquality) 

with(airquality, {
        plot(Temp, Ozone)  # plot function creates the initial plot and draws the points
        lines(loess.smooth(Temp, Ozone)) #  The lines function is used to annotate or add to the plot
        }) 


data(cars) 
## Create the plot / draw canvas 
with(cars, plot(speed, dist)) 
## Add annotation 
title("Speed vs. Stopping distance")

# Drawbacks: Cant go back, difficult to translate the plot

# The lattice system

library(lattice)

# With the lattice system, plots are created with a single function call, such 
# as xyplot or bwplot. There is no real distinction between functions that 
# create or initiate plots and functions that annotate plots because it all 
# happens at once. 

# Lattice plots tend to be most useful for conditioning types of plots, i.e.
# looking at how y changes with x across levels of z.


state <- data.frame(state.x77, region = state.region) 
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

# important: notion of panels

# The ggplot2 system

# The ggplot2 plotting ssystem attempts to split the difference between base and 
# lattice in a number of ways. Taking cues from lattice, the ggplot2 system 
# automatically deals with spacings, text,t itles but also allows you to annotate 
# by “adding” to a plot. 

library(ggplot2)

data(mpg)
qplot(displ, hwy, data = mpg) # The qplot function in ggplot2 is what you use to
# “quickly get some data on the screen”. 



# THE BASE PLOTTING SYSTEM IN R

# The core plotting and graphics engine in R is encapsulated in the following
# packages:

# • graphics: contains plotting functions for the “base” graphing systems, 
#   including plot, hist, boxplot and many others. 

# • grDevices: contains all the code implementing the various graphics devices,
#   including X11,PDF,PostScript,PNG,etc.

# There are two phases to creating a base plot:
# 1. Initializing a new plot 
# 2. Annotating (adding to) an existing plot

# Histogram

library(datasets) 
# Draw a new plot on the screen device 
hist(airquality$Ozone)
        

# base graphics system has many global parameters that can set and tweaked

# Box Plots

# Box plots can be made in R using the boxplot() function, which takes as its 
# first argument a formula. The formula has form of y-axis ∼ x-axis. Anytime you
# see a ∼ in R, it’s a formula. Here, we are plotting ozone levels in New York 
# by month, and the right hand side of the ∼ indicate the month variable. 
# However, we first have to transform the month variable into a factor before 
# we can pass it to boxplot(), or else boxplot() will treat the month variable 
# as continuous.

data("airquality")
airquality <- mutate(airquality, Month = factor(Month)) 
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")


# Scatterplot

with(airquality, plot(Wind, Ozone))

# Many base plotting functions share a set of global parameters. Here area 
# few key ones:

# • pch: the plotting symbol (default is open circle)
# • lty: the line type (default is solid line), can be dashed, dotted, etc.
# • lwd: the line width, specified as an integer multiple
# • col: the plotting color, specified as a number, string, or hex code; the
#   colors() function gives you a vector of colors by name
# • xlab: character string for the x-axis label
# • ylab: character string for the y-axislabel

# The par() function is used to specify the global graphics parameters that 
# affect all plots in an R session

# • las: the orientation of the axis labels on the plot
# • bg: the background color
# • mar: the margin size
# • oma: the outer margin size (default is 0 for all sides)
# • mfrow: number of plots per row, column (plots are filled row-wise)
# • mfcol: number of plots per row, column (plots are filled column-wise)

# Plot ()

# The most basic base plotting function is plot().
# The plot() function makes a scatterplot, or other type of plot depending on 
# the class of the object being plotted.

# Key annotations

# lines : Just connect the dots
# abline
# points
# text: add labels
# title
# mtext
# axis

# examples
# 1 
library(datasets) 
# Make the initial plot 
with(airquality, plot(Wind, Ozone))  
# Add a title 
title(main = "Ozone and Wind in New York City")

# or
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City")) 

# 2
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City")) 
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

# 3
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n")) 
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue")) 
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red")) 
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

# Notice that when constructing the initial plot, we use the option type = "n" 
# in the call to plot(). This is a common paradigm as plot() will draw everything 
# in the plot except for the data points inside the plot window. Then you can use 
# annotation functions like points() to add data points 

# Base Plot with Regression Line

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", 
                      pch = 20))  

## Fit a simple linear regression model 
model <- lm(Ozone ~ Wind, airquality) 

## Draw regression line on plot 
abline(model, lwd = 2)

# Multiple base plots

par(mfrow = c(1, 2), mar = c(3, 2, 2, 2)) 
with(airquality, {
        plot(Wind, Ozone, main = "Ozone and Wind")
        plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
        })


par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0)) # 
with(airquality, {
        plot(Wind, Ozone, main = "Ozone and Wind")
        plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
        plot(Temp, Ozone, main = "Ozone and Temperature")
        mtext("Ozone and Weather in New York City", outer = TRUE)
        })
