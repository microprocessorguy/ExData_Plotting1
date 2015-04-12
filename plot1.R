## Function to plot household Electric pwr consumption data
## Assumes by default that the data set file is in the same directory 
##
library(dplyr)
library(data.table)
plot1 <- function(dsetfile="./household_power_consumption.txt") {
  
    # we use only data from the date range 2007-02-01 and 2007-02-02
    # so filter it out    
    data <- fread(dsetfile, sep=";",na.strings = "?", header = T)
    data[,DateTime:=as.Date(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")]
    mydata <- filter(data, DateTime >= as.Date("2007-02-01 00:00:00"), 
                     DateTime <= as.Date("2007-02-02 00:00:00"))
    
    # now plot power as a histogram
    pwr <- as.numeric(mydata$Global_active_power)
    png(filename="./plot1.png",height = 480,width=480)
    hist(pwr,col="red",main="Global Active Power",
         xlab="Global Active Power (kilowatts)")
    dev.off()
    
    
}