################################################################################
                                # Week 2 #
################################################################################
                              # Lesson 1 # 
################################################################################

# The lattice Plotting System

library(lattice)

# Remember All plotting/annotation is done at once with a single function call

# Important functions

# Xyplot: Scatter plots
# bwplot: boxplots
# ......
# Stripplot: boxplot but with points

### xyplot ###

xyplot(y ~ x | f * g, data)
# Look for scatterplots of y and x in the levels of f and g
# f and g are conditional variables


library(datasets)
xyplot(Ozone ~ Wind, data = airquality)
airquality <- transform(airquality, Month = factor(Month))
class(airquality$Month)
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))
# the powerof xyplot! -- in a single call a complex function 

### difference from base 

# Base functions plot directly to graphics device
# Lattice functions return an object of class trellis (and auto-print it)

p <- xyplot(Ozone ~ Wind, data = airquality)
class(p)
print(p)

### Lattice panel functions

set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x |f, layout = c(2,1))

# Custom panel function 
xyplot(y ~ x |f, panel = function(x , y, ...) {
        panel.xyplot(x,y, ...) # first call the default panel function for xyplot
        panel.abline(h = median(y), lty = 2) # Add horizontal line at the median
})

xyplot(Ozone ~ Wind | Month,
       panel = function(x, y) {
               panel.xyplot(x, y)
               panel.abline(h = median(y), lty = 2) # Add horizontal line at the median
       },
       data = airquality)

# regression line
xyplot(y ~ x |f, panel = function(x , y, ...) {
        panel.xyplot(x,y, ...) # first call the default panel function for xyplot
        panel.lmline(x,y, col = 2) # Add horizontal line at the median
})

# example ######################################################################
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://github.com/DataScienceSpecialization/courses/raw/master/04_ExploratoryAnalysis/PlottingLattice/maacs_env.rds"
download.file(fileUrl,destfile="./data/maacs_env.rds")

maacs <- readRDS("./data/maacs_env.rds")
library(dplyr)
condenced <- maacs %>% select(MxNum, VisitNum, airmus) %>% mutate(allergen = log(airmus, 2))
xyplot(allergen ~ VisitNum | MxNum,
       data = condenced,
       xlab = "Visit Number", 
       ylab = expression('Log'[2]*' Airborne Mouse Allergen'),
       strip = FALSE,
       pch = 20,
       layout = c(17, 9),
       main = "Mouse Allergen and Asthma Cohort Study (Baltimore City)"
)
################################################################################
                                # Lesson 2 # 
################################################################################

# The ggplot2 package in R is an implementation of The Grammar of Graphics as 
# described by Leland Wilkins on in his book

# The grammar of graphics represents an abstraction of graphics ideas and objects.
# You can think of this as developing the verbs,nouns, and adjectives for 
# data graphics.

# Plots are made up of aesthetics (size,shape,color) and geoms(points,lines).


### The basic function: qplot --- analgous to plot
# Important: Factors need to be labeled 
f <- factor(f, labels = c("Group 1", "Group 2"))

library(ggplot2)
str(mpg)

qplot(displ, hwy, data = mpg)

# Modifyng aesthetics

qplot(displ, hwy, data = mpg, col = drv) 

# Color is an aesthetic and the color of each point can be mapped to a variable.
# Note that the x-coordinates and y-coordinates are aesthetics too, and they got 
# mapped to the displ and hwy variables,respectively. In this case we will map 
# the color to the drv variable.


# Adding a geom 

qplot(displ, hwy, data = mpg, geom = c("point", "smooth")) 
qplot(displ, hwy, data = mpg) + geom_smooth(method = 'loess')
# Only specify one variable -- Histogram

qplot(hwy, data = mpg, fill = drv) 

qplot(drv, hwy, data = mpg, geom = "boxplot")

# Facets --- like panels 

# The facets argument expects a formula type of input, with a ∼ separating the
# left hand side variable and the right hand side variable. The left hand side
# variable indicates how the rows of the panels should be divided and the right
# hand side variable indicates how the columns of the panels should be divided

qplot(displ, hwy, data = mpg, facets =  .~ drv) # we will divided the columns of
# the panesl by drv (nothing in rows (.))
qplot(hwy, data = mpg, facets =  drv ~ ., binwidth =  2)

qplot(displ, hwy, data = mpg, facets = . ~ drv) + geom_smooth()
# or geom as and argument 
qplot(displ, hwy, data = mpg, facets = . ~ drv, geom = c("point", "smooth"))

# case study

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://github.com/lupok2001/datasciencecoursera/raw/master/maacs.Rda"
download.file(fileUrl,destfile="./data/maacs.Rda", mode = "wb")
load("./data/maacs.Rda")

str(maacs)
as.numeric(maacs$mopos)
levels(maacs$mopos)

# one dimensional 

qplot(log(eno), data = maacs, binwidth = 0.07)

qplot(log(eno), data = maacs, binwidth = 0.1, fill = mopos)

qplot(log(eno), data = maacs, geom = "density")

qplot(log(eno), data = maacs, geom = "density", color = mopos)

# some scatterplots

qplot(log(pm25), log(eno), data = maacs)

qplot(log(pm25), log(eno), data = maacs, shape = mopos)

qplot(log(pm25), log(eno), data = maacs, color = mopos)

qplot(log(pm25), log(eno), data = maacs, color = mopos) +
        geom_smooth(method = "lm")

# with facets

qplot(log(pm25), log(eno), data = maacs, facets = .~ mopos) +
        geom_smooth(method = "lm")

# The qplot() function in ggplot2 is the analog of plot() in base graphics but 
# with many built-in features that the traditionaly plot() does not provide. The
# syntax is somewhere in between the base and lattice graphics system