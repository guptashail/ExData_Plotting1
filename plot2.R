plot2 <- function()
{
  #Load the required libraries
  library(data.table)
  
  #read data file in a local variable
  data <- fread("household_power_consumption.txt")
  
  #get the subset of the data for required dates
  dataFeb <- subset(data[data$Date == "1/2/2007" | data$Date == "2/2/2007"])
  
  #transform the numeric and date time data
  dataFeb <- transform(dataFeb, Global_active_power = as.numeric(Global_active_power), Date = as.POSIXct(strptime(paste(Date,Time), format = "%d/%m/%Y %H:%M:%S", tz= "EST")))
  
  #plot the graph with requied title, format and lables
  #store the graph in a vector
  p <- ggplot(aes(x=Date, y = Global_active_power), data = dataFeb) + geom_line() + labs(x="", y= "Global Active Power (kilowatts)") + scale_x_datetime(breaks=date_breaks('days'),labels=date_format('%a')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white', colour = 'black'))
  
  #print the graph
  print(p)
  
  #copy the screen output to a png file
  dev.copy(png, file = "plot2.png")
  
  #properly close the graphics device
  dev.off()
}