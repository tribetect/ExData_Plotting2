#EDA Project 2 Question 4

#----------------------------------------------------------------------------
#
# Across the United States, how have emissions 
# from coal combustion-related sources changed from 1999-2008?
#
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

#Subset coal sources
coalIndex <- grep("coal", SCC$Short.Name, ignore.case = TRUE) #Wherever "coal" is mentioned
SCC_coal <- SCC[coalIndex,]$SCC #SCC codes for coal sources

#for filtering NEI by SCC_coal later:
SCC_coal <- trimws(as.character(SCC_coal)) 
NEI$SCC <- trimws(as.character(NEI$SCC))

#Filter NEI by SCC_coal
NEI_coal <- NEI[0,] # Initialize empty data frame with NEI structure
for(i in SCC_coal) {
  NEI_coal_subset_i <- subset(NEI, NEI$SCC == i)
  rbind(NEI_coal, NEI_coal_subset_i)
}

#Use TApply to calculate Emission sums by year in NEI_coal
annualPM25 <- tapply(NEI_coal$Emission, NEI_coal$year, sum, simplify = TRUE)
years <- rownames(annualPM25)

#create a plot
png(file = "plotQ7.png", bg = "transparent", width = 480, height = 480, units = "px")
#CALL qPLOT with linear model regression lines
#
# setup the base plot
answer4 <- qplot(year, Emissions, data = NEI_coal, facets = .~type, 
                 geom = "smooth", method = "lm", 
                 xlab = "Years 1999-2008", ylab = "Coal Source PM2.5 Emissions (Tons)", 
                 title(main = "Trends Coal Emissions in USA (1999 - 2008)"))

# Print to device
print(answer4)
 
#
#Add a gray watermark with timestamp to the right border

#Finishup
dev.off()

#Pending items:
## Why is main title not working?
## add a watermark:
#mtext(text = paste("BY github/tribetect", Sys.time()), 
#     side = 4, line = 0.5, col = "green")
