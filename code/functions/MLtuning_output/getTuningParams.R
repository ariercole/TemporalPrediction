# determine the tuning functions for the machine learning method in question

getTuningParams <- function(input.df, type = "results") {
  if (type == "results") {
    return(colnames(input.df)[1:grep("ROC", colnames(input.df))[1]-1])
  } else if (type == "pred") {
    return(colnames(input.df)[(grep("rowIndex", colnames(input.df))[1] + 1) : (grep("Resample", colnames(input.df))[1] - 1)])
  }
}
