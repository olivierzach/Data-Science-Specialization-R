#install and load ggplot

install.packages("pacman")

pacman::p_load(ggplot2)

#read data from .rds files stored in working directory
# assign NEI and SCC variables to the outputs of the read call

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# find all emission records for Balt City (fips == "24510") by type

typeNEI <- NEI[NEI$fips == "24510",]

# aggregate on-road emission records for both Balt and LA by year and fips #

typeagg <- aggregate(Emissions ~ year + type, typeNEI, sum)


#plot emissions by type for Baltimore
#plot should show courses for each year - 1999, 2002, 2005, 2008
#store final plot in a .png file for submission
# turn off dev after complete

png('plot3.png')

g <- ggplot(typeagg, aes(year,Emissions, color = type))
g1 <- g +geom_line() + xlab("Year")+ylab("Emissions Level") +
  ggtitle("Baltimore Emissions by Type and Year")


print(g1)

dev.off()