### Plot the graphs and save to PNG files ###
###          CM 2017 - 11 - 28            ###

generateRootDir <- function(directory) {
  rootDir <- file.path("./Graphs",Sys.Date())
  dir.create(rootDir, showWarnings = FALSE, recursive = TRUE)
  rootDir <- file.path("./Graphs",Sys.Date(),directory)
  dir.create(rootDir, showWarnings = FALSE, recursive = TRUE)
  return(rootDir)
}

plotGraphs <- function(inputData, directory, exclusions = NULL, height = 6000, width = 3000, titled = TRUE, legend = TRUE, ptSize = 14) {
  ### load packages
  library(beanplot)
  library(readxl)
  library(tidyr)
  library(dplyr)
  
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

plotBeanplots <- function(variable, plotName, plotUnits, type, inputData, legend = TRUE, titled = TRUE) {

  #pull out variable i into a long dataframe
  tempDataFrame = gather(select(inputData, contains(variable)))
  tempDataFrame$alive <- inputData$alive
  
  #remove variable from names of days and reconstruct naming
  tempDataFrame$key = gsub(variable, "", tempDataFrame$key)
  tempDataFrame$key = gsub("day", "Day ", tempDataFrame$key)
  
  aliveDF <- filter(tempDataFrame, alive == "alive")
  deceasedDF <- filter(tempDataFrame, alive == "dead")
  
  #set up beanplot and plot alive 
  if (type == "time") {
    xlimits <- c(0,6)
  } else {
    xlimits <- c(0,2)
  }
  
  ylimits <- c(min(tempDataFrame$value, na.rm = TRUE), max(tempDataFrame$value, na.rm = TRUE))
  
  #set plotting units
  plotUnits <- ifelse(!is.na(plotUnits), paste(" / ", plotUnits, sep = ""), "")
  
  #plot alive
  beanplot(value~key, aliveDF, 
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
  
  #plot dead
  beanplot(value~key, deceasedDF, 
           side = "second", 
           col=c("darkblue","darkblue", "lightgrey"), 
           add = TRUE,
           what = c(1,1,0,1))
  
  #add arithmetic mean lines
  if (type == "time") {
    for (day in 1:5) {
      meanVal <- mean(aliveDF$value[aliveDF$key == paste("Day ",day,sep="")], na.rm = TRUE)
      lines(x = c(day, day - 0.5), 
            y = c(meanVal,meanVal), 
            col = "black", 
            lwd = 2)
      
      meanVal <- mean(deceasedDF$value[deceasedDF$key == paste("Day ",day,sep="")], na.rm = TRUE)
      lines(x = c(day, day + 0.5), 
            y = c(meanVal,meanVal), 
            col = "black", 
            lwd = 2)
    } 
  } else {
    meanVal <- mean(aliveDF$value, na.rm = TRUE)
    lines(x = c(1, 1 - 0.5), 
          y = c(meanVal,meanVal), 
          col = "black", 
          lwd = 2)
    
    meanVal <- mean(deceasedDF$value, na.rm = TRUE)
    lines(x = c(1, 1 + 0.5), 
          y = c(meanVal,meanVal), 
          col = "black", 
          lwd = 2)
  }
  
  #add a legend
  if(legend == TRUE) legend('topright', fill=c('#A3C1AD','darkblue'), legend= c('Alive', 'Deceased'), bty ="n")
}

# plotBeanplotPanel <- function() {
#   variables <- c("age", "apache_score", "HR", "MAP", "PaO2FiO2", "Lactate", "Sodium", "Potassium", "PH", "Creatinine", "CRP", "NoradrenalineTotal", "AdrenalineTotal", "VasopressinTotal")
#   plotting.var.df <- unique(select(filter(allVariablesDF, variableTitle %in% variables), variableSubtitle, graphTitle, type, units))
#   
#   plotMiscGraph("Panel_Plots", graphName = "BeanplotPanel_1", 8000, 16000)
#   
#   plotBeanplots(variable = "maxHR", plotName = "maximum HR", plotUnits = "bpm", type = "time", imputData = cleanData.all, legend = FALSE, titled = FALSE)
#   
#   par(mfrow = c(2,4))
# }

plotImputedGraph <- function(inputDataImputed, inputDataNonImputed, variableName) {
  rootDir <- generateRootDir(directory = "Graphs-Imputed-SimpleMeanHistograms")
  fileName <- paste(rootDir,
                    "/",variableName,
                    "_Histograms_",
                    Sys.Date(),
                    ".png",
                    sep = "")

  png(file = fileName,
      width = 6000,
      height = 3000,
      res = 300,
      pointsize = 12)
  
  par(mfrow = c(2,2))
  
  xlims <- c(min(inputDataImputed, na.rm=TRUE), max(inputDataImputed, na.rm=TRUE))
  hist(unlist(inputDataImputed)[aliveID], 
       breaks = 20, 
       col = "#A3C1AD", 
       main = paste("Histogram of imputed ", variableName, " for patients with Alive classifier", sep =""),
       xlim = xlims)
  hist(unlist(inputDataNonImputed)[aliveID], 
       breaks = 20, 
       col = "#A3C1AD", 
       main = paste("Histogram of original ",variableName," for patients with Alive classifier",sep =""),
       xlim = xlims)
  hist(unlist(inputDataImputed)[deadID], 
       breaks = 20, 
       col = "darkblue", 
       main = paste("Histogram of imputed ",variableName," for patients with Dead classifier",sep =""),
       xlim = xlims)
  hist(unlist(inputDataNonImputed)[deadID], 
       breaks = 20, 
       col = "darkblue", 
       main = paste("Histogram of original ",variableName," for patients with Dead classifier",sep =""),
       xlim = xlims)
  
  dev.off()
}

plotMiscGraph <- function(dirName, graphName, width = 6000, height = 3000, ptSize = 14) {
  rootDir <- generateRootDir(directory = dirName)
  fileName <- paste(rootDir,
                    "/",graphName,
                    "_",
                    Sys.Date(),
                    ".png",
                    sep = "")
  
  png(file = fileName,
      width = width,
      height = height,
      res = 300,
      pointsize = ptSize)
}
