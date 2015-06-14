#Pre-flight check: if both data files are available in the working directory:
foundData <- (
              ("summarySCC_PM25.rds" %in% dir()) && #AND
              ("Source_Classification_Code.rds" %in% dir())) #End of file checks

#Terminate script, if dataset not found in working directory
error_message = paste0("Terminating Script: Data Files Not Found in Current Working Directory: ", getwd())    
if(!foundData) { stop(error_message) }


#readRDS() the two files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#OBJECTIVE: Explore the National Emissions Inventory database 
#and see what it say about fine particulate matter pollution 
#in the United states over the 10-year period 1999–2008. 
#You may use any R package you want to support your analysis.
#
#
#QUESTION 1: Have total emissions from PM2.5 decreased 
#in the United States from 1999 to 2008?
#
#Plotting system: BASE
#
#OUTPUT: plot showing the total PM2.5 emission from all sources
#for each of the years 1999, 2002, 2005, and 2008
png(file = "plotQ1.png", bg = "transparent", width = 480, height = 480, units = "px")

#
#FINAL STEP: CALL BASE PLOTTING COMMANDS
#
#Use TApply to calculate Emission means by year in NEI
annualPM25 <- tapply(NEI$Emission, NEI$year, mean, simplify = TRUE)
years <- rownames(annualPM25)
#Call plot functions layer by layer
plot(years, annualPM25,type = "o", 
    pch = 15, col = "red", 
    xlab = "Years", ylab = "Annual PM2.5 Emissions (Tons)")

title(main = "Q1. PM2.5 in USA over the years", 
      sub =  "A decrease in annual emissions of particulate matter")

#Add a gray watermark with timestamp to the right border
mtext(text = paste("BY github/tribetect", Sys.time()), 
      side = 4, line = 0.5, col = "gray")

#Finishup
dev.off()