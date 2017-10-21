
# Code for first plot - load data, convert date and time, filter for relevant dates,
# open graphic device (this part is the same for all my plot-functions),
# start plotting via base plotting system, close graphic device
plot1 <- function(){
    
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
        png(filename="plot1.png", width = 480, height = 480, units = "px")
        
        #Plot first graph - an easy one (histogram):
        hist(powerdata$Global_active_power, col="red", xlab= "Global Active Power (kilowatts)", 
             main= "Global Active Power")
        
        #close graphic device
        dev.off()
        
}