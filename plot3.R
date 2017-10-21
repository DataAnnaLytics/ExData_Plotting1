
# Code for third plot - load data, convert date and time, filter for relevant dates,
# open graphic device (this part is the same for all my plot-functions),
# start plotting via base plotting system, close graphic device
plot3 <- function(){
        
        #----------------------PREPARE DATA----------------------            
        
        #load packages
        library(dplyr)
        
        
        #load data - make sure NAs are recognized; we don't need all data
        powerdata<-read.table("household_power_consumption.txt", sep= ";" ,header = TRUE, na.strings = "?", nrows = 100000)
        
        #combine date and time and convert to date and time
        powerdata<-mutate(powerdata, DateAndTime = paste(Date , Time, sep=","))
        powerdata$DateAndTime<-as.POSIXct(powerdata$DateAndTime,  format= "%d/%m/%Y,%H:%M:%S")
        
        
        #subset for feb 1st 2007 and feb 2nd 2007 
        powerdata<-filter(powerdata, powerdata$DateAndTime >= "2007-02-01 00:00:00" & powerdata$DateAndTime < "2007-02-03 00:00:00")
        
        #----------------------PLOTTING----------------------
        
        #open png, 480x480 pixels
        png(filename="plot3.png", width = 480, height = 480, units = "px")
        
        #Plot third graph - line chart (plot):
        # type = "l" for lines, no x-label --> xlab = "", 
        # I decided that German abbreviations "Do", "Fr" and "Sa" for 
        # "Thu", "Fri" and "Sat" are fine:) 
        plot(powerdata$Sub_metering_1, type="l", ylab= "Energy sub metering", xlab="", 
             x=powerdata$DateAndTime)
        #add lines for sub metering 2 and 3
        lines(powerdata$Sub_metering_2, type="l", lty=1, lwd=1, col="red", x=powerdata$DateAndTime)
        lines(powerdata$Sub_metering_3, type="l", lty=1, lwd=1, col="blue", x=powerdata$DateAndTime)
        #add legend
        legend("topright", col=c("black", "red", "blue"), lty=1,
               legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        
        
        #close graphic device
        dev.off()
        
}