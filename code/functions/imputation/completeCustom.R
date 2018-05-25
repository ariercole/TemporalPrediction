# custom wrapper for the complete function to remove non-expected values

library(mice)
library(dplyr)

completeCustom <- function(mids, imput.number) {
  temp.imput <- mice::complete(mids, imput.number)
  
  for(colname in colnames(temp.imput)) {
    if (colname %in% filter(allVariablesDF, outlierRemoval == TRUE)$variable) {
      day <- allVariablesDF[allVariablesDF$variable == colname, ]$day
      naPos <- temp.imput$icu_duration_days < day
      temp.imput[naPos, colname] <- NA
    }
  }
  return(temp.imput)
}
