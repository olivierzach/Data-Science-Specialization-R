#read data from .rds files stored in working directory
# assign NEI and SCC variables to the outputs of the read call

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#subset NEI data for Baltimore, fips == "24510"

NEIBalt <- NEI[NEI$fips == "24510",]

#aggregate Baltimore emissions by year

EmBalt <- aggregate(Emissions ~ year, NEIBalt,sum)


#plot total emissions from PM2.5 to analyze if decrease has occurred in US from 1999 to 2008 in Baltimore
#plot should show all sources for each year - 1999, 2002, 2005, 2008
#store final plot in a .png file for submission
# turn off dev after complete

png('plot1.png')

barplot(height = EmBalt$Emissions, names.arg = EmBalt$year, 
        xlab = "Year",ylab = "PM2.5 Emissions Level (Tons)", 
        main = "Baltimore PM2.5 Emissions Levels (Tons) by Year")

dev.off()