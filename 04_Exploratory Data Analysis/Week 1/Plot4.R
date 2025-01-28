# load packages and libraries

if (!require("pacman")) {
  install.packages("pacman") }
library(pacman)

# display all available packages included in pacman
pacman::p_library()

# load relevant packages for this project
pacman::p_load(utils, readr, data.table, reshape2, stringr, dplyr, tidyr, lubridate, gsubfn)


# set working directory
setwd("C:\\Users\\kelly.zhao\\Downloads")

# create a new folder for the course project
if (!dir.exists("04_Exploratory Data Analysis")) {
  dir.create(paste(getwd(), "\\04_Exploratory Data Analysis", sep=""))
}

setwd(paste(getwd(), "\\04_Exploratory Data Analysis", sep=""))
path<- getwd()

# download data file
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(url, destfile = "data.zip", method="curl")
unzip(zipfile = "data.zip")
list.files(path)

# load data
data <- read_delim("household_power_consumption.txt",
                     delim = ";",
                     na    = c("?"),
                     col_types = list(col_date(format = "%d/%m/%Y"),
                     col_time(format = ""),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number(),
                     col_number())) %>% filter(between(Date, as.Date("2007-02-01"), as.Date("2007-02-02")))

head(data)

# plot 1
with(data, hist(Global_active_power, xlab = "Global Active Power (kilowatts)", main="Global Active Power", col="red"))
dev.copy(png, "plot1.png", width  = 480, height = 480)
dev.off()

# plot 2
data <- mutate(data, datetime = ymd_hms(paste(Date, Time)))
png("plot2.png", width=480, height=480)
plot(Global_active_power ~ datetime, data, type="l", xlab="", ylab="Global Active Power (kilowatts)")
dev.off()

# Plot 3
png("plot3.png", width  = 480, height = 480)

plot(Sub_metering_1 ~ datetime, data, type = "l", ylab = "Energy sub metering", xlab = NA)
lines(Sub_metering_2 ~ datetime, data, type = "l", col = "red")
lines(Sub_metering_3 ~ datetime, data, type = "l", col = "blue")
legend("topright", lty = 1, col    = c("black",         "red",            "blue"), 
                            legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"))
dev.off()

# Plot 4
png("plot4.png", width  = 480, height = 480)
par(mfrow = c(2, 2))
## top left
plot(Global_active_power ~ datetime, data, type = "l", ylab = "Global Active Power (kilowatts)", xlab = NA)

## top right
plot(Voltage ~ datetime, data, type = "l")

## bottom left
plot(Sub_metering_1 ~ datetime, data, type = "l", ylab = "Energy sub metering", xlab = NA)
lines(Sub_metering_2 ~ datetime, data, type = "l", col = "red")
lines(Sub_metering_3 ~ datetime, data, type = "l", col = "blue")
legend("topright", col = c("black", "red", "blue"), 
                legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
                lty = 1, bty = "n")

## bottom right
plot(Global_reactive_power ~ datetime, data, type = "l")
dev.off()