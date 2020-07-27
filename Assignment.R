# Assignment

# Download
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl,destfile="./data/UCexdata_data_household_power_consumption.zip",
              method="curl")  
unzip(zipfile="./data/UCexdata_data_household_power_consumption.zip",
      exdir = "./data")

# Read subset
library(data.table)
household <- fread("./data/household_power_consumption.txt",
                   skip = grep("1/2/2007",
                               readLines("./data/household_power_consumption.txt"))[1]-1,
                   nrows = 2880
)
header <- fread("./data/household_power_consumption.txt", nrows = 1, header = FALSE)
colnames(household) <- unlist(header)
remove(header)

# Date manipulation
dt <- paste(household$Date, household$Time)
datetime <- strptime(dt, format="%d/%m/%Y %H:%M:%S")
household <- cbind(datetime, household)
class(household$datetime)

table(weekdays(household$datetime))

# Plots

# 1.
par(mfrow = c(1,1),mar = c(3,6,2,6))
hist(household$Global_active_power, xlab = "Global active power (kilowatt)",
     main ="Global active power", 
     col = "red",
     cex.lab = 0.8,
     cex.axis =0.8
     )
# 2
dev_null <- Sys.setlocale("LC_TIME", "english")
par(mar = c(3,6,2,6))
with(household, plot(datetime,Global_active_power, 
                     type = "l",
                     ylab = "Global active power (kilowatt)",
                     cex.lab = 0.8,
                     cex.axis =0.8))
# 3 

with(household, plot(datetime,Sub_metering_1, 
                     type = "l",
                     ylab = "Energy sub metering",
                     cex.lab = 0.8,
                     cex.axis =0.8))
with(household, lines(datetime, Sub_metering_2,
                      col ="red"))
with(household, lines(datetime, Sub_metering_3,
                      col ="blue"))
legend("topright", lty = 1, col = c("black","blue", "red"), 
       legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_2"),
       cex = 0.75)

# 4
par(mar = c(3,5,2,2), mfrow = c(2,2), oma = c(0, 0, 0,0))
with(household, plot(datetime,Global_active_power, 
                     type = "l",
                     ylab = "Global active power",
                     cex.lab = 0.8,
                     cex.axis =0.8))

with(household, plot(datetime,Voltage, 
                     type = "l",
                     ylab = "Voltage",
                     xlab = "datetime",
                     cex.lab = 0.8,
                     cex.axis =0.8))

with(household, plot(datetime,Sub_metering_1, 
                     type = "l",
                     ylab = "Energy sub metering",
                     cex.lab = 0.8,
                     cex.axis =0.8))
with(household, lines(datetime, Sub_metering_2,
                      col ="red"))
with(household, lines(datetime, Sub_metering_3,
                      col ="blue"))
legend(x=c(household$datetime[1700],household$datetime[1750]),y=c(40,42),
       lty = 1, col = c("black","blue", "red"), 
       legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_2"),
       cex = 0.9,  box.lty=0)

with(household, plot(datetime,Global_reactive_power, 
                     type = "l",
                     ylab = "Global_reactive_power",
                     xlab = "datetime",
                     cex.lab = 0.8,
                     cex.axis =0.8))

        household$datetime[2800]
