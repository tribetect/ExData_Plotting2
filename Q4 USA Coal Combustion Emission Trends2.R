#EDA Project 2 Question 4

#----------------------------------------------------------------------------
#
#      Across the United States, how have emissions 
#       from coal combustion-related sources changed from 1999-2008?
#
#----------------------------------------------------------------------------

#Pre-flight check: if both data files are available in the working directory:
foundData <- (("summarySCC_PM25.rds" %in% dir()) && #AND
              ("Source_Classification_Code.rds" %in% dir())) #End of file checks

#Terminate script, if dataset not found in working directory
error_message = paste0("Terminating Script: Data Files Not Found in Current Working Directory: ", getwd())    
if(!foundData) { stop(error_message) }

require(ggplot2)
require(plyr) #fast subsetting

#readRDS() the two files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Subset coal sources
coalIndex <- grep("coal", SCC$Short.Name, ignore.case = TRUE) #Wherever "coal" is mentioned
SCC_coal <- SCC[coalIndex,]$SCC #SCC codes for coal sources

#to match:
SCC_coal <- trimws(as.character(SCC_coal)) 

#subset NEI with plyr/mutate
NEI_coal <- mutate(NEI, coalYN = (NEI$SCC %in% SCC_coal)) # original 6.5 million observations
NEI_coal <- subset(NEI_coal, coalYN == TRUE) #down to 53,400 observations

#Use TApply to calculate Emission sums by year in NEI_coal
annualPM25 <- tapply(NEI_coal$Emission, NEI_coal$year, sum, simplify = TRUE)
years <- rownames(annualPM25)

#create a plot
png(file = "plotQ4.png", bg = "transparent", width = 480, height = 480, units = "px")

#CALL qPLOT with linear model regression lines
#
# setup the base plot
answer4 <- qplot(year, Emissions, data = NEI_coal, #facets = .~type, 
                 geom = "smooth", method = "lm", 
                 xlab = "Years 1999-2008", ylab = "Coal Source PM2.5 Emissions (Tons)", 
                 main = "Coal Source PM2.5 Emissions in USA (1999 - 2008)")

# Print to device
print(answer4)
 
#
#Add a gray watermark with timestamp to the right border

#Finishup
dev.off()

#Pending items:
## Why is sub title not working?
## add a watermark:
#mtext(text = paste("BY github/tribetect", Sys.time()), 
#     side = 4, line = 0.5, col = "green")
