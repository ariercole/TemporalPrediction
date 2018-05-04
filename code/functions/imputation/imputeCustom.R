# function to impute the missing data, both for the variables missing not at random and those missing at random

source("functions/imputation/parlMICE.R") # parallel wrapper for the mice function

imputeNorAdVas <- function(cleanData) {
  outputData <- cleanData
  
  norAdVas = c("AdrenalineTotal","NoradrenalineTotal","VasopressinTotal")
  for(variable in norAdVas) {
    for(day in 1:5) {
      varString <- paste("day",day,variable,sep="")
      zeroValues <- is.na(cleanData[[varString]]) & (day <= cleanData$icu_duration_days)
      outputData[zeroValues,varString] <- 0
    }
    print(paste("Imputed drugs",variable,"data to add 0's in the place of NAs where the patient is still on ICU", sep = " "))
  }
  
  return(outputData)
}

### Step 2 --> perform imputation
miceCustom <- function(partiallyImputedData) {
  return(parlMICE(partiallyImputedData,
                  m = 9,
                  maxit = 20,
                  meth = 'pmm',
                  seed = 42))
}

### Combine Steps 1 & 2 in a wrapper function
imputeCustom <- function(cleanedData, dayCutOff = 3) {
  cleanedData %>%
    imputeNorAdVas() %>%
    miceCustom()
}