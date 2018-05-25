# Plots histograms and beanplots to visually descibe the input dataset

library(beanplot)
library(readxl)
library(tidyr)
library(dplyr)

source("functions/graph_plotting/generateRootDir.R")

plotDescriptiveGraphs <- function(inputData, directory, exclusions = NULL, height = 6000, width = 3000, titled = TRUE, legend = TRUE, ptSize = 14) {
  
  ### Load the list of variables to be plotted
  continuousVariableDF <- filter(allVariablesDF, variableType %in% c("continuous", "integer"))
  
  if (!is.null(exclusions)) {
    continuousVariableDF <- continuousVariableDF[!continuousVariableDF$variableSubtitle %in% exclusions,]
  }
  
  
  ### Create Graph directory
  rootDir <- generateRootDir(directory)
  
  ###  histogram of age
  fileName <- paste(rootDir,"/age_Hist_",
                    Sys.Date(),
                    ".png",
                    sep = "")
  png(file = fileName,
      width = width,
      height = height,
      res = 300,
      pointsize = ptSize)
  par(mfrow = c(1,1))
  
  hist(inputData$age,
       col = c("darkblue"),
       xlab = "Age",
       main = "Histogram of Age",
       xlim = c(0,110),
       xaxt = 'n')
  
  
  axis(1, at = seq(0, 120, by = 5), las=2)
  
  dev.off()
  
  ###  histogram of icu_duration
  fileName <- paste(rootDir,"/icuDuration_Hist_",
                    Sys.Date(),
                    ".png",
                    sep = "")
  png(file = fileName,
      width = width,
      height = height,
      res = 300,
      pointsize = ptSize)
  par(mfrow = c(1,1))
  
  barplot(table(inputData$icu_duration_days),
       col = c("darkblue"),
       ylim = c(0,6000),
       ylab = "Frequency",
       xlab = "ICU duration (days)",
       main = "Histogram of ICU duration")
  
  dev.off()
  
  
  ###  histogram of alive or dead
  fileName <- paste(rootDir,"/aliveDead_Hist_",
                    Sys.Date(),
                    ".png",
                    sep = "")
  png(file = fileName,
      width = width,
      height = height,
      res = 300,
      pointsize = ptSize)
  par(mfrow = c(1,1))
  
  plot(inputData$alive,
       col = c("#A3C1AD", "darkblue"),
       ylim = c(0,21000),
       ylab = "Frequency",
       xlab = "Classifier",
       main = "Histogram of outcome classifier")
  
  dev.off()
  
  ###  Plot the Beanplots
  
  plotList <- unique(select(continuousVariableDF, variableSubtitle, graphTitle, type, units))
  
  for(i in 1:nrow(plotList)) {
    fileName <- paste(rootDir,"/",
                      plotList$variableSubtitle[i],
                      "_Beanplot_",
                      Sys.Date(),
                      ".png",
                      sep = "")
    png(file = fileName,
        width = width,
        height = height,
        res = 300,
        pointsize = ptSize)
    par(mfrow = c(1,1))
    
    plotBeanplots(variable = plotList$variableSubtitle[i], 
                  plotName = plotList$graphTitle[i], 
                  type = plotList$type[i], 
                  plotUnits = plotList$units[i],
                  inputData = inputData,
                  legend = legend,
                  titled = titled)
    
    dev.off()
  }
  
  
  ### Plot ventilated data
  aliveID <- dataSet$alive_dead_icu == "A"
  deadID <- dataSet$alive_dead_icu != "A"
  
  #pull out ventilated data into a data frame based on the classifier
  generateVentTable <- function(classifierList) {
    #pull out ventilated data for the classifier
    tempVentData <- data.frame(summary(inputData[classifierList,grepl("Ventilated",colnames(inputData))]))
    
    #reformat dataframe
    tempVentData$Type <- c("No","Yes","NA")
    tempVentData$Freq <- lapply(tempVentData$Freq, gsub, pattern = "Yes|No|:|NA's", replacement = "")
    tempVentData$Freq <- as.integer(lapply(tempVentData$Freq, trimws, which = "both"))
    tempVentData <- tempVentData[,2:length(tempVentData)]
    colnames(tempVentData) <- c("Day","Freq","Type")
    
    #return vetilation table
    return(tempVentData)
  }
  
  #function that returns the percentages of ventilated patients based on the classified data frame
  getClassifierPercs <- function(tempVentData) {
    #return % of ventilated for each day
    return(tempVentData$Freq[tempVentData$Type == "Yes"] / (tempVentData$Freq[tempVentData$Type == "Yes"] + tempVentData$Freq[tempVentData$Type == "No"]))
  }
  
  getVentilatedPercs <- function(tempVentDataAlive, tempVentDataDead, ventilated) {
    tempVentDataAlive$Freq[tempVentDataDead$Type == ventilated] / (tempVentDataDead$Freq[tempVentDataAlive$Type == ventilated] + tempVentDataAlive$Freq[tempVentDataDead$Type == ventilated])
  }
  
  ventData1 = data.frame(list(c(getVentilatedPercs(generateVentTable(aliveID),generateVentTable(!aliveID),"Yes"), getVentilatedPercs(generateVentTable(aliveID),generateVentTable(!aliveID),"No")), 
                              c(replicate(5,"Ventilated"),replicate(5,"Not Ventilated")),
                              c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5")))
  
  colnames(ventData1) = c("Percent_Alive", "Ventilated", "Day")
  
  ventData2 = data.frame(list(c(getClassifierPercs(generateVentTable(aliveID)), getClassifierPercs(generateVentTable(!aliveID))), 
                              c(replicate(5,"Alive"),replicate(5,"Dead")),
                              c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5")))
  
  colnames(ventData2) = c("Percent_Ventilated", "Classifier", "Day")
  
  #draw the graphs
  library(lattice)
  
  fileName <- paste(rootDir,
                    "/Ventilated",
                    "_Barplot_V1_",
                    Sys.Date(),
                    ".png",
                    sep = "")
  png(file = fileName,
      width = width,
      height = height,
      res = 300,
      pointsize = 12)
  par(mfrow = c(1,1))
  
  barchart(Percent_Alive ~ Day, 
           groups=Ventilated, 
           ventData1, 
           auto.key = list(columns = 3),
           par.settings = list(
             superpose.polygon=list(col=c("darkgrey","darkred"), border="black"),
             strip.background=list(col="red"),
             strip.border=list(col="black")),
           ylim = c(0,1),
           main="Percentage dead based on ventilation status")
  
  dev.off()
  
  fileName <- paste(rootDir,
                    "/Ventilated",
                    "_Barplot_V2_",
                    Sys.Date(),
                    ".png",
                    sep = "")
  png(file = fileName,
      width = width,
      height = height,
      res = 300,
      pointsize = 12)
  par(mfrow = c(1,1))
  
  barchart(Percent_Ventilated ~ Day, 
           groups=Classifier, 
           ventData2, 
           auto.key = list(columns = 3),
           par.settings = list(
             superpose.polygon=list(col=c("#A3C1AD","darkblue"), border="black"),
             strip.background=list(col="red"),
             strip.border=list(col="black")),
           ylim = c(0,1),
           main="Percentage ventilated for each classifier")
  
  dev.off()
}

plotBeanplots <- function(variable, plotName, plotUnits, type, inputData, legend = TRUE, titled = TRUE, splitName = "alive", splitLevel1 = "alive", splitLevel2 = "dead") {
  
  #pull out variable i into a long dataframe
  tempDataFrame = gather(select(inputData, contains(variable)))
  tempDataFrame$splitter <- inputData[[splitName]]
  
  #remove variable from names of days and reconstruct naming
  tempDataFrame$key = gsub(variable, "", tempDataFrame$key)
  tempDataFrame$key = gsub("day", "Day ", tempDataFrame$key)
  
  level.1.DF <- filter(tempDataFrame, splitter == splitLevel1)
  level.2.DF <- filter(tempDataFrame, splitter == splitLevel2)
  
  #set up beanplot
  if (type == "time") {
    xlimits <- c(0,6)
  } else {
    xlimits <- c(0,2)
  }
  
  ylimits <- c(min(tempDataFrame$value, na.rm = TRUE), max(tempDataFrame$value, na.rm = TRUE))
  
  #set plotting units
  plotUnits <- ifelse(!is.na(plotUnits), paste(" / ", plotUnits, sep = ""), "")
  
  #plot alive/level1
  beanplot(value~key, level.1.DF, 
           side = "first", 
           col=c("#A3C1AD","#A3C1AD", "lightgrey"),
           xlim = xlimits,
           ylim = ylimits,
           main = ifelse(titled == TRUE,
                         paste("Beanplot showing distribution of",
                                plotName,
                                "by vital status at discharge"), NA),
           ylab = paste(plotName, plotUnits, sep = ""),
           log = "",
           what = c(1,1,0,1), 
           las = 1)
  
  #plot deceased/level2
  beanplot(value~key, level.2.DF, 
           side = "second", 
           col=c("darkblue","darkblue", "lightgrey"), 
           add = TRUE,
           what = c(1,1,0,1))
  
  #add arithmetic mean lines
  if (type == "time") {
    for (day in 1:5) {
      meanVal <- mean(level.1.DF$value[level.1.DF$key == paste("Day ",day,sep="")], na.rm = TRUE)
      lines(x = c(day, day - 0.5), 
            y = c(meanVal,meanVal), 
            col = "black", 
            lwd = 2)
      
      meanVal <- mean(level.2.DF$value[level.2.DF$key == paste("Day ",day,sep="")], na.rm = TRUE)
      lines(x = c(day, day + 0.5), 
            y = c(meanVal,meanVal), 
            col = "black", 
            lwd = 2)
    } 
  } else {
    meanVal <- mean(level.1.DF$value, na.rm = TRUE)
    lines(x = c(1, 1 - 0.5), 
          y = c(meanVal,meanVal), 
          col = "black", 
          lwd = 2)
    
    meanVal <- mean(level.2.DF$value, na.rm = TRUE)
    lines(x = c(1, 1 + 0.5), 
          y = c(meanVal,meanVal), 
          col = "black", 
          lwd = 2)
  }
  
  #add a legend
  if(legend == TRUE) legend('topright', fill=c('#A3C1AD','darkblue'), legend= c(splitLevel1, ifelse(splitLevel2 == "dead", "deceased", splitLevel2)), bty ="n")
}
