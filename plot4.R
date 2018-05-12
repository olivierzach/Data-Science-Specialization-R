#install and load ggplot

install.packages("pacman")

pacman::p_load(ggplot2, tm)

install.packages("ggplot2")
library(ggplot2)

#read data from .rds files stored in working directory
# assign NEI and SCC variables to the outputs of the read call

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Merge NEI and SCC datasets together

EmMerge <- merge(NEI,SCC, by = "SCC")

# find all combined records related to Coal and subset those records for analysis

coaldata <- grepl("coal",EmMerge$Short.Name,ignore.case = TRUE)
subsetcoal <- EmMerge[coaldata,]

# aggregate coal emissions by year

EmAgg <- aggregate(Emissions ~ year,subsetcoal,sum)


#plot coal emissions across US
#plot should show courses for each year - 1999, 2002, 2005, 2008
#store final plot in a .png file for submission
# turn off dev after complete

png('plot4.png')

g <- ggplot(EmAgg, aes(factor(year),Emissions/10^3))
g1 <- g + geom_bar(stat = "Identity")+xlab("Year")+ylab("Coal Emissions (Scale x10000)") +
  ggtitle('Coal Emissions by Year')


print(g1)

dev.off()