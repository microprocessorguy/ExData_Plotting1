## Function to plot household Electric pwr consumption data
## Assumes by default that the data set file is in the same directory 
##
library(dplyr)
library(data.table)
plot3 <- function(dsetfile="./household_power_consumption.txt") {
    
    # we use only data from the date range 2007-02-01 and 2007-02-02
    # so filter it out    
    data <- fread(dsetfile, sep=";",na.strings = "?", header = T)
    data[,DateTime:=as.Date(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")]
    mydata <- filter(data, DateTime >= as.Date("2007-02-01 00:00:00"), 
                     DateTime <= as.Date("2007-02-02 00:00:00"))
    
    # now plot sub_metering data from mydata
    sub1 <- as.numeric(mydata$Sub_metering_1)
    sub2 <- as.numeric(mydata$Sub_metering_2)
    sub3 <- as.numeric(mydata$Sub_metering_3)
    dt <- weekdays(as.Date(mydata$Date))
    plotcolors <- c("black","red","blue")
    png(filename="./plot3.png",height = 480,width = 480)
    # plot the first one
    plot(sub1,type="l",axes=F,ann=F,col=plotcolors[1])
    box()
    axis(1,at=1,lab=c("Thu"))
    axis(1,at=(length(pwr)/2),lab=c("Fri"))
    axis(1,at=length(pwr),lab=c("Sat"))
    maxy <- max(cbind(sub1,sub2,sub3))
    maxx <- length(sub1)
    axis(2, at=10*0:maxy)
    # plot the rest
    lines(sub2,type="l",col=plotcolors[2])
    lines(sub3,type="l",col=plotcolors[3])
    legend("topright", 
           c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           col=plotcolors, lty=1)
    title(ylab= "Energy sub metering")
    dev.off()
    
}