# function to transform the ventialtion data by dividing values above 80 by 7.6
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