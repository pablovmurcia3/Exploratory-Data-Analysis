library(swirl)

swirl()

head(pollution)

dim(pollution)

summary(pollution$pm25)

quantile(ppm)

boxplot(ppm, col = "blue")
abline(h =12) #  drew a horizontal line at 12
2

hist(ppm, col = "green")
rug(ppm)

low

high

hist(ppm, col = "green", breaks = 100)
rug(ppm)

hist(ppm, col = "green")
abline(v = 12, lwd = 2)

abline(v = median(ppm), col = "magenta", lwd = 4)

names(pollution)

reg <-table(pollution$region)

reg

barplot(reg, col = "wheat", main = "Number of Counties in Each Region")

boxplot( pm25~region, data = pollution, col = "red")

par(mfrow=c(2,1),mar=c(4,4,2,1))

east <- subset(pollution, region == "east")

head(east)

hist(east$pm25, col = "green")

hist(subset(pollution, region == "west")$pm25, col = "green")

with(pollution, plot(latitude, pm25))
abline(h =12, lwd = 2, lty = 2)


plot(pollution$latitude, ppm, col = pollution$region)
abline(h =12, lwd = 2, lty = 2)

par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))

west <- subset(pollution, region == "west")

plot(west$latitude, west$pm25, main = "West")

plot(east$latitude, east$pm25, main = "East")
