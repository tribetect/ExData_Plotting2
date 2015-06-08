#EDA Project 2 Question 4

#----------------------------------------------------------------------------
#
#  How have emissions from motor vehicle sources changed 
#  from 1999-2008 in Baltimore City? 
#    fips == "24510" 
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
motorIndex <- grep("Onroad", 
                   SCC$Data.Category, 
                   ignore.case = TRUE) 
                  #Motor vehicle source search index, 1709 of 11,717 observations in SCC

SCC_motor <- SCC[motorIndex,]$SCC #SCC codes (factor vector) for motor vehicles
SCC_motor <- trimws(as.character(SCC_motor)) #cleanup for match-and-subset NEI next 

#subset NEI with plyr/mutate for Baltimore motor vehicles

NEI_motor <- mutate(NEI, motorYN = (NEI$SCC %in% SCC_motor)) #tag motor vehicles obervations TRUE/FALSE: 
#FALSE    TRUE 
#1020590 5477061
NEI_motor <- subset(NEI_motor, motorYN == TRUE)  #drop 1.02 million FALSE, keep 5.5mil TRUEs
NEI_motor_Baltimore <- subset(NEI_motor, fips == 24510) #down to 1515 observations

#Use TApply to calculate Emission sums by year
annualPM25 <- tapply(NEI_motor_Baltimore$Emission, 
                     NEI_motor_Baltimore$year, sum, simplify = TRUE)
#REMOVE?? annualPM25 <- as.data.frame(annualPM25) 



#CALL qPLOT with linear model regression lines
#
# setup the base plot
answer5 <- qplot(years, annualPM25, geom = "auto",
                           xlab = "Years 1999-2008", ylab = "PM2.5 Emissions (Tons)", 
                           main = "Motor Vehicle Emissions in Baltimore (1999 - 2008)")

# Print to device
print(answer5)

#
#Add a gray watermark with timestamp to the right border

#Finishup
#create a plot
png(file = "plotQ5.png", bg = "transparent", width = 480, height = 480, units = "px")
dev.off()

#Pending items:

## add a watermark:
#mtext(text = paste("BY github/tribetect", Sys.time()), 
#     side = 4, line = 0.5, col = "green")
