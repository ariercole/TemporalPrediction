# function to load and combine results or predicitons into one data frame

library(dplyr)
library(pROC)
library(parallel)
library(tcltk)

buildPredResDF <- function(MLmethod, pred.res, model.df, path) {
  
  # select models according to input method
  model.df.temp <- filter(model.df, method == MLmethod)
  
  # for each model, add predictions to the grand prediction output df, with labelling
  for(i in 1:nrow(model.df.temp)) {
    model <- model.df.temp[i,]
    
    model.pred <- readRDS(model[[paste(pred.res, "Path", sep = "")]])
    
    model.pred <- mutate(model.pred, ML.method = model$method, day = model$day, iter = model$iter, cumulative = model$cumulative)
    
    if (i == 1) {
      model.predictions.df <- model.pred
    } else {
      model.predictions.df <- rbind(model.predictions.df, model.pred)
    }
  }
  
  if (pred.res == "results") {
    model.predictions.df$ROC <- 0.5 + abs(0.5 - model.predictions.df$ROC)
  }
  
  if (!dir.exists(paste(path, "/output/", MLmethod, sep = ""))) dir.create(paste(path, "/output/", MLmethod, sep = ""), recursive = TRUE)
  saveRDS(model.predictions.df, paste(path, "/output/", MLmethod, "/", pred.res, ".", MLmethod, ".RDS", sep = ""))
  return(model.predictions.df)
}