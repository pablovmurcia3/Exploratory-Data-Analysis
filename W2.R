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
y <- x + f - f * x +rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x |f, layout = c(2,1))

# Custom panel function 
xyplot(y ~ x |f, panel = function(x , y, ...) {
        panel.xyplot(x,y, ...)
        panel.abline(h = median(y), lty = 2) # Add horizontal line at the median
})

