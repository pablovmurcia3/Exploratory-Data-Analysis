library(swirl)

swirl()

# The lattice system, as the base does, provides several different plotting functions.
# These include xyplot for creating scatterplots, bwplot for box-and-whiskers plots or
# boxplots, and histogram for histograms. There are several others (stripplot, dotplot, 
# splom and levelplot), which we won't cover here.

xyplot(y ~ x | f * g, data) # he * represents interaction between them

head(airquality)

xyplot(Ozone ~ Wind, data = airquality)

xyplot(Ozone ~ Wind, data = airquality, pch=8, col="red", main="Big Apple Data")

xyplot(Ozone ~ Wind | as.factor(Month) , data = airquality, layout = c(5,1))

p <- xyplot(Ozone~Wind,data=airquality)

print(p) 

names(p)

mynames[myfull]

p[["formula"]]

p[["x.limits"]]

table(f)

xyplot(y ~ x |f, layout = c(2,1))

v1

v2

myedit("plot1.R")

p <- xyplot(y ~ x | f, panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)  ## First call the default panel function for 'xyplot'
        panel.abline(h = median(y), lty = 2)  ## Add a horizontal line at the median
})
print(p)
invisible()


source(pathtofile("plot1.R"), local = TRUE)

myedit("plot2.R")

source(pathtofile("plot2.R"), local = TRUE)

str(diamonds)

table(diamonds$color)

table(diamonds$color, diamonds$cut)

myedit("myLabels.R")

source(pathtofile("myLabels.R"), local = TRUE)

xyplot(price ~ carat | color*cut, data = diamonds, strip = FALSE, pch = 20, 
       xlab = myxlab,
       ylab = myylab,
       main = mymain)

xyplot(price ~ carat | color*cut, data = diamonds, pch = 20, 
     )

################################################################################

# Ggplot 

#  Ggplot components include aesthetics which are attributes such as colour, shape,
# and size, and geometric objects or geoms such as points, lines, and bars.

str(mpg)

qplot(displ, hwy, data = mpg)

qplot(displ, hwy, data = mpg, color = drv)

qplot(displ, hwy, data = mpg, color = drv, geom = c("point","smooth"))

qplot(y = hwy, data = mpg, color =  drv)

# So, specifying the y parameter only, without an x
# argument, plots the values of the y argument in the order in which they occur in
# the data.

myhigh

# Boxplot
qplot(drv, hwy, data = mpg, geom = "boxplot")

qplot(drv, hwy, data = mpg, geom = "boxplot", color = manufacturer)
# Wow!!!!

# Histograms
qplot(hwy, data = mpg, fill = drv)

# facets 
qplot(displ, hwy, data = mpg, facets = . ~ drv)

qplot(hwy, data = mpg, facets = drv~. , binwidth = 2)

################################################################################

qplot(displ, hwy, data=mpg, geom = c("point", "smooth"), facets = .~drv)

g <- ggplot(mpg, aes(displ, hwy))

summary(g)

g + geom_point()

g + geom_point() + geom_smooth()

g + geom_point() + geom_smooth(method = "lm")

g + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ drv)

g + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ drv) +
        ggtitle("Swirl Rules!")

g + geom_point(color = "pink", size = 4, alpha = 1/2)

# Notice the different shades of pink? That's the result of the alpha aesthetic
# which you set to 1/2. This aesthetic tells ggplot how transparent the points
# should be. Darker circles indicate values hit by multiple data points.

g + geom_point(size = 4, alpha = 1/2, aes(color = drv))

g + geom_point(aes(color = drv)) + 
        labs( title =  "Swirl Rules!")+
        labs(x = "Displacement", y = "Hwy Mileage")

g + geom_point(aes(color = drv), size = 2, alpha = 1/2) +
        geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE)

g + geom_point(aes(color = drv)) + theme_bw(base_family = "Times")

plot( myx, myy, type = "l", ylim = c(-3,3))

g <- ggplot(testdat, aes(myx, myy)) 

g + geom_line()

g + geom_line() + ylim(-3,3)

# Now recall that at the beginning of the lesson we mentioned 7 components of a 
# ggplot plot, one of which was a coordinate system. This is a situation where 
# using a coordinate system would be helpful.

g + geom_line() + coord_cartesian(ylim = c(-3,3))

g <- ggplot(mpg, aes(x = displ, y = hwy, color = factor(year))) 
g + geom_point() 

g <- ggplot(mpg, aes(x = displ, y = hwy))
g + geom_point(aes(color = factor(year)))

g + geom_point() + facet_grid(drv~cyl, margins = TRUE)

g + geom_point() +
        facet_grid(drv~cyl, margins = TRUE) +
        geom_smooth(method = "lm", se = FALSE, size = 2, color = "black")

g + geom_point() +
        facet_grid(drv~cyl, margins = TRUE) +
        geom_smooth(method = "lm", se = FALSE, size = 2, color = "black") +
        labs(x = "Displacement", y = "Highway Mileage", title = "Swirl Rules!")

################################################################################

library(swirl)

swirl()

str(diamonds)

# Histograms and density

qplot(price, data = diamonds)

range(diamonds$price)

qplot(price, data = diamonds, binwidth = 18497/30)

brk

counts

qplot(price, data = diamonds, binwidth = 18497/30, fill = cut)

qplot(price, data = diamonds, geom = "density")

qplot(price, data = diamonds, geom = "density", color = cut)

# scatterplots

qplot(carat, price, data = diamonds)

qplot(carat, price, data = diamonds, shape = cut)

qplot(carat, price, data = diamonds, color = cut)


# This is different ....
qplot(carat,price,data=diamonds, color=cut) + geom_smooth(method="lm")

qplot(carat,price,data=diamonds, color=cut, facets = .~cut) +
        geom_smooth(method="lm")

### Gplots

g <- ggplot(diamonds, aes(depth, price))

summary(g) # We see that g holds the entire dataset

g + geom_point(alpha= 1/3)

cutpoints <- quantile(diamonds$carat, seq (0, 1,length = 4), na.rm = TRUE )

cutpoints

diamonds$car2 <- cut(diamonds$carat, cutpoints)
#  This command takes 2 arguments,diamonds$carat, which is what we want to cut, 
# and cutpoints, the places where we'll cut.

g <- ggplot(diamonds, aes(depth, price))

g + geom_point(alpha = 1/3) + facet_grid(cut ~ car2)

myd

diamonds[myd,]

g + geom_point(alpha = 1/3) + facet_grid(cut ~ car2) +
        geom_smooth(method = "lm", size = 3, color = "pink")



# boxplots 

ggplot(diamonds, aes(car2, price)) + geom_boxplot() + facet_grid(.~cut)

ggplot(diamonds, aes(price)) + geom_boxplot() + facet_grid(.~cut)


ggplot(diamonds, aes(y = price)) + geom_boxplot() # ????? 
qplot(y = price, data = diamonds, geom = "boxplot")


