plot1 <- function()
{
  #Load the required libraries
  library(data.table)
  
  #read data file in a local variable
  data <- fread("household_power_consumption.txt")
  
  #get the subset of the data for required dates
  dataFeb <- subset(data[data$Date == "1/2/2007" | data$Date == "2/2/2007"])
  
  #transform the numeric data
  dataFeb <- transform(dataFeb, Global_active_power = as.numeric(Global_active_power))
  
  #plot the histogram with requied title and lables
  hist(dataFeb$Global_active_power, col = "Red", xlab = "Global Active Power (Kilowatt)", main = "Global Active Power")
  
  #copy the screen output to a png file
  dev.copy(png, file = "plot1.png")
  
  #properly close the graphics device
  dev.off()
}