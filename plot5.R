#install and load ggplot

install.packages("pacman")

pacman::p_load(ggplot2)

#read data from .rds files stored in working directory
# assign NEI and SCC variables to the outputs of the read call

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# find all on-road emission records for Balt City (fips == 24510)


motordataNEI <- NEI[NEI$fips == "24510" & NEI$type == "ON-ROAD",]

# aggregate on-road emission records for Balt City by year

motoragg <- aggregate(Emissions ~ year, motordataNEI, sum)


#plot coal emissions across US
#plot should show courses for each year - 1999, 2002, 2005, 2008
#store final plot in a .png file for submission
# turn off dev after complete

png('plot5.png')

g <- ggplot(motoragg, aes(factor(year),Emissions))
g1 <- g + geom_bar(stat = "Identity")+xlab("Year")+ylab("Motor Emissions Balt. City") +
  ggtitle('Motor Emissions Balt. City by Year')


print(g1)

dev.off()