#EDA Project 2 Question 56

#----------------------------------------------------------------------------
#
#   Compare emissions from motor vehicle sources in Baltimore City 
#   with emissions from motor vehicle sources in Los Angeles County, California 
#   (fips == "06037"). 
#
#   Which city has seen greater changes over time in motor vehicle emissions?
#    Baltimore: fips == "24510" 
#----------------------------------------------------------------------------

#Pre-flight check: if both data files are available in the working directory:
foundData <- (("summarySCC_PM25.rds" %in% dir()) && #AND
                ("Source_Classification_Code.rds" %in% dir())) #End of file checks

#Terminate script, if datafiles not found in working directory
error_message = paste0("Terminating Script: Data Files Not Found in Current Working Directory: ", getwd())    
if(!foundData) { stop(error_message) }

require(ggplot2)
require(plyr) #fast subsetting
#create a PNG device
png(file = "plotQ5.png", bg = "transparent", width = 480, height = 480, units = "px")

#readRDS() the two files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Subset coal sources
motorIndex <- grep("Onroad", 
                   SCC$Data.Category, 
                   ignore.case = TRUE) 
#Motor vehicle source search index, 1709 of 11,717 records in SCC

SCC_motor <- SCC[motorIndex,]$SCC #SCC codes (factor vector) for motor vehicles
SCC_motor <- trimws(as.character(SCC_motor)) #cleanup for match-and-subset NEI next 

#subset NEI with plyr/mutate for Baltimore motor vehicles

NEI_motor <- mutate(NEI, motorYN = (NEI$SCC %in% SCC_motor)) #tag motor vehicles obervations TRUE/FALSE: 
#FALSE    TRUE 
#1020590 5477061

NEI_motor_Baltimore <- subset(NEI_motor, fips == 24510 & motorYN == TRUE) 
#down to 1515 observations; drop 1.02 million FALSE, keep 5.5mil TRUEs, then subset Baltimore
NEI_motor_LA <- subset(NEI_motor, fips == 06037 & motorYN == TRUE) 

#Use TApply to calculate Emission sums by year
annualPM25 <- tapply(NEI_motor_Baltimore$Emission, 
                     NEI_motor_Baltimore$year, sum, simplify = TRUE) 
annualPM25 <- as.data.frame(annualPM25)#Y for plot
years <- as.integer(dimnames(annualPM25)[[1]]) #X for plot

#GGPlot object prep
result <- ggplot(data = annualPM25, mapping = aes(factor(years), annualPM25, group = 1))

#visualize quantities through point sizes
sizes <- as.integer(annualPM25[,1]/20)

#"Paint" the visualization
result <- result + geom_point(color = sizes, size = sizes)+geom_line()
result <- result + labs(x = "Year", y = "Emissions (Tons of PM25)", title = "Baltimore Motor Vehicle Emissions (1999 - 2008)")
result <- result + geom_text(aes(label=as.integer(annualPM25)),hjust=-1, vjust=1)

print(result)

#Finishup

dev.off()

#Pending items:

## add a watermark:
#mtext(text = paste("BY github/tribetect", Sys.time()), 
#     side = 4, line = 0.5, col = "green")