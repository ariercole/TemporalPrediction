###         Functions to remove the outliers        ###
###                 CM 2018 - 03 - 06               ###

# Remove outliers by combining min & max for each day to calculate the mean and stdevs

#load support functions
#source("3_outliers/20180107_outliersSupportFunctions.R")

### Remove outliers from the data
### Transform ventialtion Data
transformVentilationData <- function(dataSet) {
  tempDataSet <- dataSet
  for(i in 1:5) {
    for (j in c("min", "max")) {
      multiplier <- (6.6 * as.numeric(dataSet[[paste("day",i,j,"PaO2FiO2",sep="")]] > 80)) + 1
      tempDataSet[[paste("day",i,j,"PaO2FiO2",sep="")]] <- (dataSet[[paste("day",i,j,"PaO2FiO2",sep="")]] / multiplier)
      print("Transformed PaO2FiO2 data")
    }
  }
  return(tempDataSet)
}

### build fenction to set the upper and lower cutoff limites for each variable according to a specified method
generateLimits <- function(dataSet, method = "singleDaySeparate") {
  limitsDF <- data.frame(variable = allVariablesDF$variable, UL = rep(NA, nrow(allVariablesDF)), LL = rep(NA, nrow(allVariablesDF)))
  
  
  if (method == "singleDaySeparate") {
    
    ### set limits based on mena and sd of each variable on each day, separated by min and max
    for (variableNo in 1:nrow(allVariablesDF)) {
      if (allVariablesDF$class[variableNo] != "factor") {
        varVals <- unlist(select(dataSet, contains(allVariablesDF$variable[variableNo])))
        limitsDF[variableNo, "UL"] <- mean(varVals, na.rm = TRUE) + 5 * sd(varVals, na.rm = TRUE)
        limitsDF[variableNo, "LL"]  <- mean(varVals, na.rm = TRUE) - 5 * sd(varVals, na.rm = TRUE)
      }
    }
    
    
  } else if (method == "singleDayCombined") {
    
    ### set limits based on mean and sd of each variable on each day, regardless of min/max
    for (variableNo in 1:nrow(allVariablesDF)) {
      if (allVariablesDF$class[variableNo] != "factor") {
        if (is.na(allVariablesDF$minMax[variableNo])) {
          varVals <- unlist(select(dataSet, contains(allVariablesDF$variable[variableNo])))
          limitsDF[variableNo, "UL"] <- mean(varVals, na.rm = TRUE) + 5 * sd(varVals, na.rm = TRUE)
          limitsDF[variableNo, "LL"]  <- mean(varVals, na.rm = TRUE) - 5 * sd(varVals, na.rm = TRUE)
        } else {
          for(day in 1:5) {
            varVals <- unlist(dataSet %>% select(contains(allVariablesDF$variableTitle[variableNo])) %>% select(contains(paste("day", day, sep = ""))))
            limitsDF[variableNo, "UL"] <- mean(varVals, na.rm = TRUE) + 5 * sd(varVals, na.rm = TRUE)
            limitsDF[variableNo, "LL"]  <- mean(varVals, na.rm = TRUE) - 5 * sd(varVals, na.rm = TRUE)
          }
        }
      }
    }
    
  } else if (method == "multiDayCombined") {
    
    ### set limits based on mean and sd of each variable, regardless of day, regardless of min/max
    for (variableNo in 1:nrow(allVariablesDF)) {
      if (allVariablesDF$class[variableNo] != "factor") {
        if (is.na(allVariablesDF$minMax[variableNo])) {
          varVals <- unlist(select(dataSet, contains(allVariablesDF$variableTitle[variableNo])))
          limitsDF[variableNo, "UL"] <- mean(varVals, na.rm = TRUE) + 5 * sd(varVals, na.rm = TRUE)
          limitsDF[variableNo, "LL"]  <- mean(varVals, na.rm = TRUE) - 5 * sd(varVals, na.rm = TRUE)
        } else {
          varVals <- unlist(dataSet %>% select(contains(allVariablesDF$variableTitle[variableNo])) %>% select(contains("day1"), contains("day2"), contains("day3"), contains("day4"), contains("day5")))
          limitsDF[variableNo, "UL"] <- mean(varVals, na.rm = TRUE) + 5 * sd(varVals, na.rm = TRUE)
          limitsDF[variableNo, "LL"]  <- mean(varVals, na.rm = TRUE) - 5 * sd(varVals, na.rm = TRUE)
        }
      }
    }
  }
  
  ### set manual limits on particular variables
  limitsDF[grepl("Potassium", limitsDF$variable), "UL"] <- 10 # potassium upper limit of 10
  limitsDF[grepl("Lactate", limitsDF$variable), "UL"] <- 20 # lactate upper limit of 10
  limitsDF[grepl("Lactate", limitsDF$variable), "LL"] <- 0 # no lower limit on lactate
  limitsDF[grepl("Creatinine", limitsDF$variable), "LL"] <- 0 # no lower limit on creatinine
  limitsDF[grepl("Adrenaline|Noradrenaline|Vasopressin|CRP|age|apache_score", limitsDF$variable), "UL"] <- 1000000000 # no limits on Adrenaline, Noradrenaline, Vasopressin, CRP and age
  limitsDF[grepl("Adrenaline|Noradrenaline|Vasopressin|CRP|age|apache_score", limitsDF$variable), "LL"] <- 0 # no limits on Adrenaline, Noradrenaline, Vasopressin, CRP and age
  
  return(limitsDF)
}

### build function to remove outliers
removeOutliers <- function(dataSet, method) {
  limitsDF <- generateLimits(dataSet = dataSet, method = method)
  
  returnDF <- dataSet
  
  for (variable in allVariablesDF$variable[allVariablesDF$outlierRemoval == TRUE]) {
    outliers <- dataSet[[variable]] > limitsDF[limitsDF$variable == variable, "UL"] | dataSet[[variable]] < limitsDF[limitsDF$variable == variable, "LL"]
    returnDF[[variable]] <- ifelse(outliers == TRUE,
                                   NA,
                                   dataSet[[variable]])
  }
  
  return(returnDF)
}