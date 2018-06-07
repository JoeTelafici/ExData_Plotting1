###############################################################################
##
##    Joe Telafici
##    joe@telafici.com
##    Johns Hopkins Data Science - Exploratory Data Analysis
##    Week 1 Assignment - Plotting Exercise
##    Plot 3 Creation Script
##
###############################################################################



## This script requires the data in the University of  
## California Irvine's Machine Learning Repository's Individual household electric power consumption Data Set
## Available at https://archive.ics.uci.edu/ml/datasets/individual+household+electric+power+consumption
## to be located in the parent of the current working directory for this script (../)



readdata <- function (input)
{
    message (paste0("input file is ",input,"."))
    if (file.exists(input)){
        datatbl <- read.table(input, sep=";", na.strings = "?", header=TRUE, colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
        datatbl[,1]<-as.Date(datatbl[,1], format="%d/%m/%Y")
        datatbl <- datatbl[datatbl$Date < "2007-02-03" & datatbl$Date >= "2007-02-01",]
        datetimes <- strptime(paste(datatbl[,1],datatbl[,2]),"%Y-%m-%d %H:%M:%S")
        datatbl$Date <- datetimes
    }
    else {message(paste("Power data not located at", input))}
    
    return (datatbl)
}

createplot <-function ()
{
  library (ggplot2)
  outfile <- "plot3.png"
  infile <- "../household_power_consumption.txt"
  plotwidth <- 480
  plotheight <- 480
  powerdata <- readdata (infile)
  png(file=outfile)
  
  print(ggplot(powerdata, aes(x=Date)) + geom_line(aes(y=Sub_metering_1, colour="Sub_Metering_1")) + geom_line(aes(y=Sub_metering_2, colour="Sub_Metering_2")) + geom_line(aes(y=Sub_metering_3, colour="Sub_Metering_3")) + scale_x_datetime(date_labels = "%a", date_breaks ="1 day") + xlab("Date") + ylab("Energy sub metering") + scale_colour_manual(values=c("black", "red", "blue")) + theme(legend.title=element_blank(), legend.justification=c(1,1), legend.position=c(1,1), panel.background = element_rect(fill = NA, colour = "black"), legend.background = element_rect(fill = NA), legend.key = element_rect(fill = "white"), legend.box.background = element_rect(colour = "black"), legend.key.width=unit(2,"line")))
  dev.off ()
  
}
