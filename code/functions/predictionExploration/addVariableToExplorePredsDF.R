# looks up the values of the input variable, or all variables, for the correct imputation, for the correct admission index and returns the input dataframe with the new variable attached

addVariableToExplorePredsDF <- function(explorePreds.df, variableName) {
  out.df <- data.frame()
  
  # set up progress bar
  pb <- tkProgressBar(title = "Add variable", label = "Adding variables...",
                      min = 0, max = 9 * 5, initial = 0, width = 300)
  
  for (imput in 1:9) {
    imputedSet <- completeCustom(imputedData.all, imput)
    
    imput.df <- data.frame()
    for (day.in in 1:5) {
      dataSet.filtered <- filter(imputedSet, icu_duration_days >= day.in)
      
      filtered.df <- filter(explorePreds.df, imputation == imput, day == day.in)
      
      imput.df <- rbind(imput.df,
                        cbind(filtered.df, 
                              dataSet.filtered[filtered.df$rowIndex,variableName]))
      setTkProgressBar(pb, (imput * 5) - 5 + day.in)
    }
    out.df <- rbind(out.df, imput.df)
  }
  
  close(con = pb)
  
  colnames(out.df)[length(out.df) - (length(variableName) - 1): length(out.df)] <- variableName
  
  return(out.df)
}


addAllVariablesToExplorePredsDF <- function(explorePreds.df) {
  out.df <- data.frame()
  
  # set up progress bar
  pb <- tkProgressBar(title = "Add variable", label = "Adding variables...",
                      min = 0, max = 9 * 5, initial = 0, width = 300)
  
  for (imput in 1:9) {
    imputedSet <- completeCustom(imputedData.all, imput)
    
    imput.df <- data.frame()
    for (day.in in 1:5) {
      dataSet.filtered <- filter(imputedSet, icu_duration_days >= day.in)
      
      filtered.df <- filter(explorePreds.df, imputation == imput, day == day.in)
      
      imput.df <- rbind(imput.df,
                        cbind(filtered.df, 
                              dataSet.filtered[filtered.df$rowIndex,]))
      setTkProgressBar(pb, (imput * 5) - 5 + day.in)
    }
    out.df <- rbind(out.df, imput.df)
  }
  
  close(con = pb)
  
  return(out.df)
}
