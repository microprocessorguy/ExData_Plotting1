## Function to plot household Electric pwr consumption data
## Assumes by default that the data set file is in the same directory 
##

library(dplyr)
library(data.table)

plot4 <- function(dsetfile="./household_power_consumption.txt") {
    
    # we use only data from the date range 2007-02-01 and 2007-02-02
    # so filter it out    
    data <- fread(dsetfile, sep=";",na.strings = "?", header = T)
    data[,DateTime:=as.Date(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")]
    mydata <- filter(data, DateTime >= as.Date("2007-02-01 00:00:00"), 
                     DateTime <= as.Date("2007-02-02 00:00:00"))
    
    # create the plot canvas
    png(filename="./plot4.png",height = 480,width = 480)
    op <- par(mfrow = c(2, 2), # 2 x 2 pictures on one plot
              pty = "s",       # square plotting region
              mar = c(5,4,2,1))
            
    # first subplot
    pwr <- as.numeric(mydata$Global_active_power)
    plot(pwr,type="l",axes=F,ann=F)
    box()
    axis(1,at=1,lab=c("Thu"))
    axis(1,at=(length(pwr)/2),lab=c("Fri"))
    axis(1,at=length(pwr),lab=c("Sat"))
    axis(2, at=2*0:max(pwr))
    title(ylab= "Global Active Power")
    
    # second subplot
    voltage <- as.numeric(mydata$Voltage)
    plot(voltage,type="l",axes=F,ann=F)
    box()
    axis(1,at=1,lab=c("Thu"))
    axis(1,at=(length(pwr)/2),lab=c("Fri"))
    axis(1,at=length(pwr),lab=c("Sat"))
    srt_lbl <- ceiling(min(voltage))
    end_lbl <- floor(max(voltage))
    axis(2,at=seq(srt_lbl,end_lbl,2))
    #axis(2,at=seq(srt_lbl,end_lbl,2),labels=seq(srt_lbl,end_lbl,4))
    title(ylab= "Voltage",xlab="datetime")
    
    # third subplot
    sub1 <- as.numeric(mydata$Sub_metering_1)
    sub2 <- as.numeric(mydata$Sub_metering_2)
    sub3 <- as.numeric(mydata$Sub_metering_3)
    plotcolors <- c("black","red","blue")
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
    legend("topright", bty="n", cex=0.8,
           c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           col=plotcolors, lty=1)
    title(ylab= "Energy sub metering")
    
    # fourth subplot
    rpwr <- as.numeric(mydata$Global_reactive_power)
    plot(rpwr,type="l",axes=F,ann=F)
    box()
    axis(1,at=1,lab=c("Thu"))
    axis(1,at=(length(pwr)/2),lab=c("Fri"))
    axis(1,at=length(pwr),lab=c("Sat"))
    axis(2, at=seq(from=0.0,to=max(rpwr),by=0.1))
    title(ylab= "Global_reactive_power",xlab="datetime")
    
    dev.off()
    par(op)
    
}