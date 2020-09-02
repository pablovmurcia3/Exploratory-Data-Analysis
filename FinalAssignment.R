################################################################################
                         #  assignment #
################################################################################

# Download

fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileUrl, destfile = "./data/NEI.zip", method = "curl")
unzip(zipfile = "./data/NEI.zip", exdir ="data" )
?unzip

# read files 
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# quick file investigation 

str(NEI)
head(NEI)
str(SCC)
head(SCC)

# 1. 

library(dplyr)
library(RColorBrewer)


pmyear <- NEI %>% group_by(year)  %>% summarise(meanpm = mean(Emissions))

cols <- brewer.pal(8, "Dark2")
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))


with(pmyear, {plot(year, meanpm, 
                   type = "l",
                   lwd = 2,
                   main = "Total emissions from PM2.5",
                   ylab ="Tons of fine Particulate matter (PM25)",
                   xlab = "Year",
                   col = pal(1),
                   )})

# 2.

pmyearbaltimore <- NEI %>% filter(fips == "24510") %>%
        group_by(year) %>%
        summarise(meanpm = mean(Emissions))


with(pmyearbaltimore, {plot(year, meanpm, 
                   type = "l",
                   lwd = 2,
                   main = "Total emissions from PM2.5 in Baltimore City",
                   ylab ="Tons of particulate matter (PM2.5)",
                   xlab = "Year",
                   col = pal(1),
)})     


balti = subset(NEI, fips == "24510")
polutant <- tapply(balti$Emissions, balti$year, mean)
plot(polutant, type = "l")

# 3

library(ggplot2)

pmbaltimore <- NEI %>% filter(fips == "24510") %>%
        group_by(type, year) %>%
        summarise(meanpm = mean(Emissions))

g <- ggplot(pmbaltimore, aes(year, meanpm))

p <- g + geom_line(color = pal(1), size= 1.3) +
        facet_grid( .~ factor(type)) +
        labs(title = "Total emissions from PM2.5 in Baltimore City according to 
             the type of the source", y = "Tons of particulate matter (PM2.5)",
             x = "Year") +
        theme(plot.title = element_text(hjust = 0.5, vjust = 2))

p

# 4

coalssc <- SCC$SCC[grep("Coal",SCC$EI.Sector)] 

pmcoal <- NEI %>% filter(NEI$SCC %in% coalssc)  %>% 
        group_by(year) %>%
        summarise(meanpm = mean(Emissions))
        

g <- ggplot(pmcoal, aes(year, meanpm))

p <- g + geom_line(color = pal(1), size= 1.3) 
p

# 5

Vehiclessc <- SCC$SCC[grep("Motor Vehicles",SCC$SCC.Level.Three)] 

pmcoal <- NEI %>% filter(SCC %in% Vehiclessc)  %>% 
        group_by(year) %>%
        summarise(meanpm = mean(Emissions))



