# Assignment

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl,destfile="./data/UCexdata_data_household_power_consumption.zip",
              method="curl")  
unzip(zipfile="./data/UCexdata_data_household_power_consumption.zip",
      exdir = "./data")


library(data.table)
household <- fread("./data/household_power_consumption.txt",
                   skip = grep("1/2/2007",
                               readLines("./data/household_power_consumption.txt"))[1]-1,
                   nrows = 2880
)
header <- fread("./data/household_power_consumption.txt", nrows = 1, header = FALSE)
colnames(household) <- unlist(header)
remove(header)

dt <- paste(household$Date, household$Time)
datetime <- strptime(dt, format="%d/%m/%Y %H:%M:%S")
household <- cbind(datetime, household)

time <- strptime(household$Time, format="%H:%M:%S")


library(lubridate)
table(weekdays(household$datetime))

# Plots

# 1.

hist(household$Global_active_power, xlab = "Global active power (kilowatt)",
     main ="Global active power", col = "red")
# 2
dev_null <- Sys.setlocale("LC_TIME", "english")
with(household, plot(datetime,Global_active_power, type = "l"))

