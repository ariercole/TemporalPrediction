# function to load the results or predictions of the models for each imputation

library(tcltk)

getImputationResults <- function(MLmethod, imputationPath, output.type = "results", invert = FALSE) {
  # generate list of possible classifiers
  combinations <- expand.grid(day = 1:5,
                              iter = 1:9,
                              cumulative = c("cumulative", ""))
  
  # output model df is currently empty
  out.model.df.length <- 0
  out.model.df <- data.frame()
  
  # set up progress bar
  pb <- tkProgressBar(title = MLmethod, label = "Reading RDS files...",
                      min = 0, max = nrow(combinations), initial = 0, width = 300)
  
  # try each classifier and if it exists, add it to a list
  for (i in 1:nrow(combinations)) {
    # select classifier to try from combinations
    combTry <- combinations[i,]
    
    #get the right path
    path <- imputationPath
    
    # generate path for this model
    model.path <- paste(path, "/",output.type,"/", MLmethod, "/ROC.tuningML.", MLmethod,".day", combTry$day, combTry$cumulative, ".iterIMPVAR", combTry$iter, ".", output.type, ".RDS", sep = "")
    
    # try to load the results (smallest of the three options --> fastest)
    if(file.exists(model.path)) {
      model.results <- readRDS(model.path)
      
      out.model.df.row <- data.frame(MLmethod = MLmethod,
                                     day = combTry$day, 
                                     cumulative = ifelse(combTry$cumulative == "cumulative", TRUE, FALSE), 
                                     imputation = combTry$iter,  
                                     model.results)
      
      if (out.model.df.length == 0) {
        out.model.df <- out.model.df.row
        out.model.df.length <- out.model.df.length + 1
      } else {
        out.model.df <- rbind(out.model.df, out.model.df.row)
        out.model.df.length <- out.model.df.length + 1
      }
    }
    
    setTkProgressBar(pb, i)
  }
  close(con = pb)
  
  if (invert == TRUE) out.model.df$ROC <- 1 - out.model.df$ROC
  
  return(out.model.df)
}
