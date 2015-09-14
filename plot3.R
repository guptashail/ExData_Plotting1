plot3 <- function()
{
  #Load the required libraries
  library(data.table)
  library(scales)
  library(grid)
  
  #read data file in a local variable
  data <- fread("household_power_consumption.txt")
  
  #get the subset of the data for required dates
  dataFeb <- subset(data[data$Date == "1/2/2007" | data$Date == "2/2/2007"])
  
  #transform the numeric and date time data
  dataFeb <- transform(dataFeb, Sub_metering_1 = as.numeric(Sub_metering_1),Sub_metering_2 = as.numeric(Sub_metering_2), Sub_metering_3 = as.numeric(Sub_metering_3), Date = as.POSIXct(strptime(paste(Date,Time), format = "%d/%m/%Y %H:%M:%S", tz= "EST")))
  
  #plot the graph with requied title, format and lables
  #store the graph in a vector
  p <- ggplot(aes(x=Date), data = dataFeb) + geom_line(aes(y = Sub_metering_1,col="Sub_metering_1"))  + geom_line(aes(y = Sub_metering_2, col="Sub_metering_2")) + geom_line(aes(y = Sub_metering_3, col = "Sub_metering_3")) + labs(x="", y= "Energy Sub Metering") + scale_x_datetime(breaks=date_breaks('days'),labels=date_format('%a')) + scale_color_manual(values=c("Black", "Red", "Blue")) + theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.background = element_rect(colour = "black", fill = 'white'), panel.background = element_rect(fill = 'white', colour = 'black'), legend.margin=unit(-0.65,"cm"),panel.margin=unit(0,"cm"), legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.key = element_blank())
  
  #print the graph
  print(p)
  
  #copy the screen output to a png file
  dev.copy(png, file = "plot3.png")
  
  #properly close the graphics device
  dev.off()
}