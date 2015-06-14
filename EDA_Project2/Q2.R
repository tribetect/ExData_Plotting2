#EDA Project 2 Question 2
#--------------------------
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#(fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.
#----------------------------------------------------------------------------

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
NEI_Baltimore <- subset(x = NEI, fips == 24510)



#Plotting system: BASE
png(file = "plotQ2.png", bg = "transparent", width = 480, height = 480, units = "px")

#OUTPUT: plot showing the total PM2.5 emission from all sources
#for Baltimore, MD for 1999 - 2008
#
NEI_Baltimore <- subset(x = NEI, fips == 24510)
#FINAL STEP: CALL BASE PLOTTING COMMANDS
#
#Use TApply to calculate Emission means by year in NEI
annualPM25 <- tapply(NEI_Baltimore$Emission, NEI_Baltimore$year, mean, simplify = TRUE)
years <- rownames(annualPM25)

#Call plot functions layer by layer
plot(years, annualPM25,type = "o", 
    pch = 15, col = "red", 
    xlab = "Years", ylab = "Annual PM2.5 Emissions (Tons)")
title(main = "Q2. PM2.5 in Baltimore, MD over the years", 
      sub =  "A decrease in annual emissions of particulate matter")

#Add a gray watermark with timestamp to the right border
mtext(text = paste("BY github/tribetect", Sys.time()), 
      side = 4, line = 0.5, col = "green")

#Finishup
dev.off()