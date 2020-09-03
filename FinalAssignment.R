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


pmyear <- NEI %>% group_by(year)  %>% summarise(sumpm = sum(Emissions))


with(pmyear, {plot(year, sumpm, 
                   type = "l",
                   lwd = 2,
                   main = expression("Total PM"[2.5]*" emissions in the United States from 1999 to 2008"),
                   ylab = expression("Tons of PM"[2.5]*" emissions"),
                   xlab = "Year",
                   col = "#1B9E77",
                   )})

# 2.

pmyearbaltimore <- NEI %>% filter(fips == "24510") %>%
        group_by(year) %>%
        summarise(sumpm = sum(Emissions))


with(pmyearbaltimore, {plot(year, sumpm, 
                   type = "l",
                   lwd = 2,
                   main =  expression("Total PM"[2.5]*" emissions in Baltimore City from 1999 to 2008"),
                   ylab =expression("Tons of PM"[2.5]*" emissions"),
                   xlab = "Year",
                   col = "#1B9E77",
)})     


# 3

library(ggplot2)

pmbaltimore <- NEI %>% filter(fips == "24510") %>%
        group_by(type, year) %>%
        summarise(sumpm = sum(Emissions))

g <- ggplot(pmbaltimore, aes(year, sumpm))

p <- g + geom_line(aes(color = type), size= 1.3) +
        facet_grid( .~ type) +
        labs(title = expression("Total PM"[2.5]*" emissions by type of source in Baltimore City from 1999 to 2008"),
             x = "Year", y = expression("Tons of PM"[2.5]*" emissions")) +
        theme(plot.title = element_text(hjust = 0.5, vjust = 2)) +
        scale_colour_brewer(palette="Dark2")

p

# 4

coalssc <- SCC$SCC[grep("Coal",SCC$EI.Sector)] 

pmcoal <- NEI %>% filter(NEI$SCC %in% coalssc)  %>% 
        group_by(year) %>%
        summarise(sumpm = sum(Emissions))


g <- ggplot(pmcoal, aes(year, sumpm))
p <- g +geom_line(color = "#1B9E77", size= 1.3) + 
        labs(title = expression("Coal combustion PM"[2.5]*" emissions in the United States from 1999 to 2008"),
             y = expression("Tons of PM"[2.5]*" emissions"),
             x = "Year") + 
        theme(plot.title = element_text(hjust = 0.5, vjust = 2)) 
p

# 5

Vehiclessc <- SCC$SCC[grep("Vehicles",SCC$SCC.Level.Two)] 

baltimoreVehicle <- NEI %>% filter(fips == "24510") %>% 
        filter(SCC %in% Vehiclessc) %>% 
        group_by(year) %>%
        summarise(sumpm = sum(Emissions))

g <- ggplot(baltimoreVehicle, aes(year, sumpm))
p <- g +geom_line(color = "#1B9E77", size= 1.3) +
        labs(title = expression("Motor Vehicle PM"[2.5]*" emissions in Baltimore City from 1999 to 2008"),
             y = expression("Tons of PM"[2.5]*" emissions"),
             x = "Year") + 
        theme(plot.title = element_text(hjust = 0.5, vjust = 2)) 
p

# 6


Vehiclessc <- SCC$SCC[grep("Vehicles",SCC$SCC.Level.Two)] 

baltimoreVehicle <- NEI %>% filter(fips == "24510") %>% 
        filter(SCC %in% Vehiclessc) %>% 
        group_by(year) %>%
        summarise(sumpm = sum(Emissions))

losangelesVehicle <- NEI %>% filter(fips == "06037") %>% 
        filter(SCC %in% Vehiclessc) %>% 
        group_by(year) %>%
        summarise(sumpm = sum(Emissions))


baltimoreAngeles <- data.frame(rbind(baltimoreVehicle,losangelesVehicle),
                              city = c(rep("Baltimore City",4),
                                       rep("los Angeles County", 4)))

g <- ggplot(baltimoreAngeles, aes(year, sumpm))
p <- g +geom_line(aes(color = city),size= 1.3) +
        facet_wrap(.~factor(city)) +
        labs(title = expression("Motor Vehicle PM"[2.5]*" emissions from 1999 to 2008"),
             x = "Year", y = expression("Tons of PM"[2.5]*" emissions")) +
        theme(plot.title = element_text(hjust = 0.5, vjust = 2)) + 
        scale_colour_brewer(palette="Dark2")

p
        
p

