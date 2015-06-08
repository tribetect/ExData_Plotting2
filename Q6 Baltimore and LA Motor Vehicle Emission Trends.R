#EDA Project 2 Question 6
#--------------------------
#Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources 
#in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?
#----------------------------------------------------------------------------

#Pre-flight check: if both data files are available in the working directory:
foundData <- (("summarySCC_PM25.rds" %in% dir()) && #AND
              ("Source_Classification_Code.rds" %in% dir())) #End of file checks

#Terminate script, if dataset not found in working directory
error_message = paste0("Terminating Script: Data Files Not Found in Current Working Directory: ", getwd())    
if(!foundData) { stop(error_message) }

require(ggplot2)

#readRDS() the two files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI_Baltimore <- subset(x = NEI, fips == 24510)

#Use TApply to calculate Emission means by year in NEI
annualPM25 <- tapply(NEI_Baltimore$Emission, NEI_Baltimore$year, mean, simplify = TRUE)

years <- rownames(annualPM25)
png(file = "plotQ3.png", bg = "transparent", width = 480, height = 480, units = "px")

#CALL qPLOT with linear model regression lines
#
# setup the base plot
answer3 <- qplot(year, Emissions, data = NEI_Baltimore, facets = .~type, 
                 geom = "smooth", method = "lm", 
                 xlab = "Years 1999-2008", ylab = "PM2.5 Emissions (Tons)", 
                 title(main = "Trends in Emission Types in Baltimore City from 1999 - 2008"))

# Print to device
print(answer3)
 
#
#Add a gray watermark with timestamp to the right border

#Finishup
dev.off()

#Pending items:
## Why is main title not working?
## add a watermark:
#mtext(text = paste("BY github/tribetect", Sys.time()), 
#     side = 4, line = 0.5, col = "green")
