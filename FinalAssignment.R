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



# 1. 

# Load Packages
library(dplyr)
library(RColorBrewer)

# Plot

pmyear <- NEI %>% group_by(year)  %>% summarise(sumpm = sum(Emissions))

png(filename="./plots/plot1.png", width=600, height=480,  type="cairo")
with(pmyear, {plot(year, sumpm, 
                   type = "l",
                   lwd = 2,
                   main = expression("Total PM"[2.5]*" emissions in the United States from 1999 to 2008"),
                   ylab = expression("Tons of PM"[2.5]*" emissions"),
                   ylim = c(0,max(pmyear$sumpm)),
                   xlab = "Year",
                   col = "#1B9E77",
                   )})
dev.off()


# 2.

# Load packages
library(dplyr)
library(RColorBrewer)

# Plot
pmyearbaltimore <- NEI %>% filter(fips == "24510") %>%
        group_by(year) %>%
        summarise(sumpm = sum(Emissions))

png(filename="./plots/plot2.png", width=600, height=480,  type="cairo")
with(pmyearbaltimore, {plot(year, sumpm, 
                   type = "l",
                   lwd = 2,
                   main =  expression("Total PM"[2.5]*" emissions in Baltimore City from 1999 to 2008"),
                   ylab =expression("Tons of PM"[2.5]*" emissions"),
                   ylim = c(0, max(pmyearbaltimore$sumpm)),
                   xlab = "Year",
                   col = "#1B9E77",
)})     
dev.off()



# 3

# Load packages
library(ggplot2)
library(dplyr)
library(RColorBrewer)

#Plot

pmbaltimore <- NEI %>% filter(fips == "24510") %>%
        group_by(type, year) %>%
        summarise(sumpm = sum(Emissions))

png(filename="./plots/plot3.png", width= 900, height=480,  type="cairo")
g <- ggplot(pmbaltimore, aes(year, sumpm))
p <- g + geom_line(aes(color = type), size= 1.3) +
        facet_grid( .~ type) +
        labs(title = expression("Total PM"[2.5]*" emissions by type of source in Baltimore City from 1999 to 2008"),
             x = "Year", y = expression("Tons of PM"[2.5]*" emissions")) +
        theme(plot.title = element_text(hjust = 0.5, vjust = 2)) +
        scale_colour_brewer(palette="Dark2")

p
dev.off()


# 4

# Load packages
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Plot
coalssc <- SCC$SCC[grep("Coal",SCC$EI.Sector)] 

pmcoal <- NEI %>% filter(NEI$SCC %in% coalssc)  %>% 
        group_by(year) %>%
        summarise(sumpm = sum(Emissions))

png(filename="./plots/plot4.png", width= 600, height=480,  type="cairo")
g <- ggplot(pmcoal, aes(year, sumpm))
p <- g +geom_line(color = "#1B9E77", size= 1.3) + 
        labs(title = expression("Coal combustion PM"[2.5]*" emissions in the United States from 1999 to 2008"),
             y = expression("Tons of PM"[2.5]*" emissions"),
             x = "Year") + 
        ylim(0,max(pmcoal$sumpm)) +
        theme(plot.title = element_text(hjust = 0.5, vjust = 2)) 
p
dev.off()

# 5

# Load packages
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Plot
Vehiclessc <- SCC$SCC[grep("Vehicles",SCC$SCC.Level.Two)] 

baltimoreVehicle <- NEI %>% filter(fips == "24510") %>% 
        filter(SCC %in% Vehiclessc) %>% 
        group_by(year) %>%
        summarise(sumpm = sum(Emissions))

png(filename="./plots/plot5.png", width= 600, height=480,  type="cairo")
g <- ggplot(baltimoreVehicle, aes(year, sumpm))
p <- g +geom_line(color = "#1B9E77", size= 1.3) +
        labs(title = expression("Motor Vehicle PM"[2.5]*" emissions in Baltimore City from 1999 to 2008"),
             y = expression("Tons of PM"[2.5]*" emissions"),
             x = "Year") + 
        ylim(0,max(baltimoreVehicle$)) +
        theme(plot.title = element_text(hjust = 0.5, vjust = 2)) 
p
dev.off()

# 6

# Load packages
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Plot

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

png(filename="./plots/plot6.png", width= 800, height= 480,  type="cairo")
g <- ggplot(baltimoreAngeles, aes(year, sumpm))
p <- g +geom_line(aes(color = city),size= 1.3) +
        facet_wrap(.~factor(city)) +
        labs(title = expression("Motor Vehicle PM"[2.5]*" emissions from 1999 to 2008"),
             x = "Year", y = expression("Tons of PM"[2.5]*" emissions")) +
        theme(plot.title = element_text(hjust = 0.5, vjust = 2)) + 
        scale_colour_brewer(palette="Dark2")

p
dev.off()
        

