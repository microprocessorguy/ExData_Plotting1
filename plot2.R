## Function to plot household Electric pwr consumption data
## Assumes by default that the data set file is in the same directory 
##
library(dplyr)
library(data.table)
plot2 <- function(dsetfile="./household_power_consumption.txt") {
    
    # we use only data from the date range 2007-02-01 and 2007-02-02
    # so filter it out    
    data <- fread(dsetfile, sep=";",na.strings = "?", header = T)
    data[,DateTime:=as.Date(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")]
    mydata <- filter(data, DateTime >= as.Date("2007-02-01 00:00:00"), 
                     DateTime <= as.Date("2007-02-02 00:00:00"))
    
    # now plot power as a series
    pwr <- as.numeric(mydata$Global_active_power)
    dt <- weekdays(as.Date(mydata$Date))
    png(filename="./plot2.png",height = 480,width = 480)
    plot(pwr,type="l",axes=F,ann=F)
    box()
    axis(1,at=1,lab=c("Thu"))
    axis(1,at=(length(pwr)/2),lab=c("Fri"))
    axis(1,at=length(pwr),lab=c("Sat"))
    axis(2, at=2*0:max(pwr))
    title(ylab= "Global Active Power (kilowatts)")
    dev.off()
       
}