#install and load ggplot

install.packages("pacman")

pacman::p_load(ggplot2)

#read data from .rds files stored in working directory
# assign NEI and SCC variables to the outputs of the read call

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# find all on-road emission records for Balt City (fips == "24510") and Los Angeles (fips == "06037")


motorcombNEI <- NEI[(NEI$fips == "24510" | NEI$fips == "06037") & NEI$type == "ON-ROAD",]

# aggregate on-road emission records for both Balt and LA by year and fips #

motoragg2 <- aggregate(Emissions ~ year + fips, motorcombNEI, sum)

#re-name comparison headers

motoragg2$fips[motoragg2$fips=="24510"] <- "Baltimore"
motoragg2$fips[motoragg2$fips=="06037"] <- "Los Angeles"


#plot motor emissions comparing Balt to LA
#plot should show courses for each year - 1999, 2002, 2005, 2008
#store final plot in a .png file for submission
# turn off dev after complete

png('plot6.png')

g <- ggplot(motoragg2, aes(factor(year),Emissions))
g1 <- g + facet_grid(.~fips)
g2 <- g1 +geom_bar(stat = "Identity")+xlab("Year")+ylab("Motor Emissions") +
  ggtitle('Motor Emissions Baltimore vs. Los Angeles by Year')


print(g2)

dev.off()