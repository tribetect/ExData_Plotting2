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
png(file = "plotQ6-1.png", bg = "transparent", width = 480, height = 480, units = "px")

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

NEI_motor_Baltimore <- subset(NEI_motor, fips == "24510" & motorYN == TRUE) 
#down to 1515 observations for Baltimore
NEI_motor_LA <- subset(NEI_motor, fips == "06037" & motorYN == TRUE) 
#down to 1515 observations for LA 

NEI_motor <- rbind(NEI_motor_Baltimore, NEI_motor_LA)

#Use TApply to calculate Emission sums by year
annualPM25_Baltimore <- tapply(NEI_motor_Baltimore$Emission, 
                     NEI_motor_Baltimore$year, sum, simplify = TRUE) 
annualPM25_Baltimore <- as.data.frame(annualPM25_Baltimore)#Y for plot
annualPM25_LA <- tapply(NEI_motor_LA$Emission, 
                               NEI_motor_LA$year, sum, simplify = TRUE) 
annualPM25_LA <- as.data.frame(annualPM25_LA)#Y for plot

annualPM25 <- cbind(annualPM25_LA, annualPM25_Baltimore)
years <- as.integer(dimnames(annualPM25)[[1]]) #X for plot
annualPM25 <- cbind(annualPM25, years)
rownames(annualPM25) <- NULL
colnames(annualPM25) <- c("LA", "Baltimore", "year")


#GGPlot object prep #"Paint" the visualization
resultBaltimore <- ggplot(data = annualPM25, mapping = aes(x = years, y = annualPM25$Baltimore))
resultBaltimore <- resultBaltimore + geom_point(size = 7) + geom_smooth(method = "lm", se = FALSE, size = 2, col = "blue")
result <- resultBaltimore + geom_point(size = 7, mapping = aes(x = years, y = annualPM25$LA))
result <- result + geom_smooth(method = "lm", se = FALSE, size = 2, col = "red",mapping = aes(x = years, y = annualPM25$LA) )
result <- result + labs(x = "Year", y = "Emissions log(Tons of PM25)", title = "Q6. Motor Vehicle Emissions LA (Red) vs. Baltimore (Blue)")
#result <- result + geom_text(aes(label=as.integer(annualPM25)),hjust=-1, vjust=1)

print(result)

#Finishup

dev.off()

#Pending items:

## add a watermark:
#mtext(text = paste("BY github/tribetect", Sys.time()), 
#     side = 4, line = 0.5, col = "green")