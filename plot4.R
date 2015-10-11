# Initial Download, and unzipping of data files.
# Calls this function only when the data file is missing.
# Downloads and unzips
initialdownloadunzip <- function(){
  #local git
  # mywd <- "C:/Users/Benj Arriola/Dropbox/Personal/Data Science/ExData_Plotting1"
  # my external drive
  # mywd <- "F:/Coursera/Data Science Specialization/04 Exploratory Data Analysis/Project 1"
  # setwd(mywd)
  
  dataDownloadURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  download.file(dataDownloadURL,"household_power_consumption.zip")
  unzip("household_power_consumption.zip")
}

# Load the data, then get subset for the dates 2-1-2007 to 2-2-2007
# Calls this function only when the dataframe has no data.
loadData <- function(dataFile = "household_power_consumption.txt") {
  powerConsumption <- read.table(dataFile, header=TRUE, sep=";", colClasses=c("character", "character", rep("numeric",7)), na.strings="?")
  powerConsumption$Time <- strptime(paste(powerConsumption$Date, powerConsumption$Time), "%d/%m/%Y %H:%M:%S")
  powerConsumption$Date <- as.Date(powerConsumption$Date, "%d/%m/%Y")
  dateRange <- as.Date(c("2007-02-01", "2007-02-02"), "%Y-%m-%d")
  powerConsumption <- subset(powerConsumption, Date %in% dateRange)
  return(powerConsumption)
}

# All my plot1.R, plot2.r, plot3.R and plot4.R have the same content
# All my functions are together and only the last function call changes.
# I just found it easier to work this way. Work with 1 file until I was done.

# plot 1 histogram
plot1 <- function(powerConsumptionData=NULL) {
  if(file.exists("household_power_consumption.txt")==FALSE) initialdownloadunzip()
  if(is.null(powerConsumptionData)) powerConsumptionData <- loadData()
  png("plot1.png", width=400, height=400)
  hist(powerConsumptionData$Global_active_power, main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency", col="red")
  dev.off()
}

# plot 2 line plot
plot2 <- function(powerConsumptionData=NULL) {
  if(file.exists("household_power_consumption.txt")==FALSE) initialdownloadunzip()
  if(is.null(powerConsumptionData)) powerConsumptionData <- loadData()
  png("plot2.png", width=400, height=400)
  plot(powerConsumptionData$Time, powerConsumptionData$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
  dev.off()
}

# plot 3 line plot
plot3 <- function(powerConsumptionData=NULL) {
  if(file.exists("household_power_consumption.txt")==FALSE) initialdownloadunzip()
  if(is.null(powerConsumptionData)) powerConsumptionData <- loadData()
  png("plot3.png", width=400, height=400)
  plot(powerConsumptionData$Time, powerConsumptionData$Sub_metering_1, type="l", col="black", xlab="", ylab="Energy sub metering")
  lines(powerConsumptionData$Time, powerConsumptionData$Sub_metering_2, col="red")
  lines(powerConsumptionData$Time, powerConsumptionData$Sub_metering_3, col="blue")
  legend("topright", col=c("black", "red", "blue"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1)
  dev.off()
}

# plot 4 combo
plot4 <- function(powerConsumptionData=NULL) {
  if(file.exists("household_power_consumption.txt")==FALSE) initialdownloadunzip()
  if(is.null(powerConsumptionData)) powerConsumptionData <- loadData()
  png("plot4.png", width=400, height=400)
  par(mfrow=c(2,2))
  
  # 1st quadrant
  plot(powerConsumptionData$Time, powerConsumptionData$Global_active_power, type="l", xlab="", ylab="Global Active Power")
  
  # 2nd quadrant
  plot(powerConsumptionData$Time, powerConsumptionData$Voltage, type="l", xlab="datetime", ylab="Voltage")
  
  # 3rd quadrant
  plot(powerConsumptionData$Time, powerConsumptionData$Sub_metering_1, type="l", col="black", xlab="", ylab="Energy sub metering")
  lines(powerConsumptionData$Time, powerConsumptionData$Sub_metering_2, col="red")
  lines(powerConsumptionData$Time, powerConsumptionData$Sub_metering_3, col="blue")
  legend("topright", col=c("black", "red", "blue"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, box.lwd=0)
  
  # 4th quadrant
  plot(powerConsumptionData$Time, powerConsumptionData$Global_reactive_power, type="n", xlab="datetime", ylab="Global_reactive_power")
  lines(powerConsumptionData$Time, powerConsumptionData$Global_reactive_power)
  dev.off()
}

# Plot all, 1 to 4 for faster testing
plotall <- function(powerConsumptionData=NULL){
  plot1()
  plot2()
  plot3()
  plot4()
}

plot4()