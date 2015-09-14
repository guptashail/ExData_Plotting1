plot4 <- function()
{
  #Load the required libraries
  library(data.table)
  library(scales)
  library(grid)
  library(gridExtra)
  
  #read data file in a local variable
  data <- fread("household_power_consumption.txt")
  
  #get the subset of the data for required dates
  dataFeb <- subset(data[data$Date == "1/2/2007" | data$Date == "2/2/2007"])
  
  #transform the numeric and date time data
  dataFeb <- transform(dataFeb, Sub_metering_1 = as.numeric(Sub_metering_1),Sub_metering_2 = as.numeric(Sub_metering_2), Sub_metering_3 = as.numeric(Sub_metering_3), Global_active_power = as.numeric(Global_active_power), Global_reactive_power = as.numeric(Global_reactive_power), Voltage = as.numeric(Voltage), Date = as.POSIXct(strptime(paste(Date,Time), format = "%d/%m/%Y %H:%M:%S", tz= "EST")))
  
  #plot the graph with requied title, format and lables
  #store the graphs in vectors
  p1 <- ggplot(aes(x=Date, y = Global_active_power), data = dataFeb) + geom_line() + labs(x="", y= "Global Active Power") + scale_x_datetime(breaks=date_breaks('days'),labels=date_format('%a')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white', colour = 'black'))
  
  p2 <- ggplot(aes(x=Date), data = dataFeb) + geom_line(aes(y = Sub_metering_1,col="Sub_metering_1"))  + geom_line(aes(y = Sub_metering_2, col="Sub_metering_2")) + geom_line(aes(y = Sub_metering_3, col = "Sub_metering_3")) + labs(x="", y= "Energy Sub Metering") + scale_x_datetime(breaks=date_breaks('days'),labels=date_format('%a')) + scale_color_manual(values=c("Black", "Red", "Blue")) + theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.background = element_rect(colour = "white", fill = 'white'), panel.background = element_rect(fill = 'white', colour = 'black'), legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.margin=unit(-0.5,"cm"), panel.margin=unit(0,"cm"), legend.key = element_blank())
  
  p3 <- ggplot(aes(x=Date, y = Voltage), data = dataFeb) + geom_line() + labs(x="datetime") + scale_x_datetime(breaks=date_breaks('days'),labels=date_format('%a')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white', colour = 'black'))
  
  p4 <- ggplot(aes(x=Date, y = Global_reactive_power), data = dataFeb) + geom_line() + labs(x="datetime") + scale_x_datetime(breaks=date_breaks('days'),labels=date_format('%a')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white', colour = 'black'))
  
  #print the graphs across two columns
  grid.arrange(p1,p3,p2,p4, ncol = 2)
  
  #copy the screen output to a png file
  dev.copy(png, file = "plot4.png")
  
  #properly close the graphics device
  dev.off()
}