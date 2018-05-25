# function to find the best performing tuning parameters for each day for a machine learning method

library(dplyr)
source("functions/MLtuning_output/getTuningParams.R")

getBestTune <- function(MLmethod) {
  results.df <- get(paste("ML.results.df.", MLmethod, sep = ""))

  tuning.params <- getTuningParams(results.df, "results")
  
  bestPreds <- results.df %>% group_by(day, cumulative) %>% summarise(ROC.best = max(ROC))
  
  bestTune <- as.data.frame(results.df[results.df$ROC %in% bestPreds$ROC.best, c("day", tuning.params,"ROC", "iter", "cumulative")])

  return(bestTune)
}