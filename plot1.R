#read data from .rds files stored in working directory
# assign NEI and SCC variables to the outputs of the read call

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#aggregate NEI file by year for comparisons

Em2 <- aggregate(Emissions ~ year, NEI, sum)


#plot total emissions from PM2.5 to analyze if decrease has occurred in US from 1999 to 2008
#plot should show all sources for each year - 1999, 2002, 2005, 2008
#store final plot in a .png file for submission
# turn off dev after complete

png('plot1.png')

barplot(height = Em2$Emissions/10^3, names.arg = Em2$year, 
  xlab = "Year",ylab = "PM2.5 Emissions Level (Tons, Scale = x10000)", 
  main = "PM2.5 Emissions Levels (Tons) by Year")

dev.off()