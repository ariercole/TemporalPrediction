# retrieve the predictions made for the validation dataset from the best performing tuning parameters for a given machine learning method

library(dplyr)
library(parallel)
library(tcltk)

source("functions/MLtuning_output/getBestTune.R")

getBestTunePreds <- function(MLmethod, path) {
  # get the tuning parameters for the best tune for each day
  bestTune <- getBestTune(MLmethod = MLmethod)
  
  # find the number of tuning parameters
  tune.no <- ncol(bestTune) - 4
  
  # find the names of the tuning parameters
  tuners <- colnames(bestTune)[2 : (1 + tune.no)]
  
  # correct labelling of complexity in DeepNN to standard number of digits
  if(MLmethod == "DeepNN") bestTune$complexity.multiplier <- as.character(format(as.numeric(bestTune$complexity.multiplier), nsmall = 4))
  
  # collapse the tuning parameters for the best tunes to lookup
  if (tune.no > 1) {
    bestTune$bt.Str <- apply(bestTune[ , tuners] , 1 , paste , collapse = "-" )
  } else {
    bestTune$bt.Str <- bestTune[ , tuners]
  }
  
  pb <- tkProgressBar(title = "Progress", label = MLmethod,
                      min = 0, max = nrow(bestTune), initial = 0, width = 300)
  
  # load the predictions for the best tunes only and add the resampled predictions to an output data frame
  out.df <- data.frame()
  for (i in 1:nrow(bestTune)) {
    tune <- bestTune[i,]
    load.df <- readRDS(paste(path, "/pred/", MLmethod, "/ROC.tuningML.", MLmethod, ".day", tune$day, ifelse(tune$cumulative, "cumulative", ""), ".iter", tune$iter, ".pred.RDS", sep = ""))
    newPred.df <- mutate(load.df, ML.method = MLmethod, day = tune$day, iter = tune$iter, cumulative = tune$cumulative)
    
    # correct labelling of complexity in DeepNN to standard number of digits
    if(MLmethod == "DeepNN") newPred.df$complexity.multiplier <- as.character(format(as.numeric(newPred.df$complexity.multiplier), nsmall = 4))
    
    if (tune.no > 1) {
      if(nrow(newPred.df) > 3000000) {
        cl <- makeCluster(detectCores() - 1)
        newPred.df$bt.Str <- parApply(cl, newPred.df[ , tuners] , 1 , paste , collapse = "-" )
        stopCluster(cl)
      } else {
        newPred.df$bt.Str <- apply(newPred.df[ , tuners] , 1 , paste , collapse = "-" )
      }
    } else {
      newPred.df$bt.Str <- newPred.df[ , tuners]
    }
  
    out.df <- rbind(out.df, filter(newPred.df, 
                                   bt.Str == tune$bt.Str, 
                                   day == bestTune[i,"day"], 
                                   iter == bestTune[i,"iter"], 
                                   cumulative == bestTune[i,"cumulative"]))
    
    setTkProgressBar(pb, i)
  }
  close(pb)
  return(out.df)
}
