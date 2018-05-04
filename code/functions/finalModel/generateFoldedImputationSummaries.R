# function to produce summary statistics from a dataframe of resample results across all imputations

library(tcltk)
library(dplyr)
library(rlang)

source("functions/MLtuning_output/generateCustomSummary.R")

generateFoldedImputationSummaries <- function(imputationPredsDF, invert = FALSE, switch = FALSE) {
  # make list of all resamples
  individual.resamples.list <- unique(select(imputationPredsDF, MLmethod, day, cumulative, imputation, Resample))
  
  MLmethod <- as.character(imputationPredsDF[1,"MLmethod"])
  
  pb <- tkProgressBar(title = MLmethod, label = "Generating resampling statistics...",
                      min = 0, max = nrow(individual.resamples.list), initial = 0, width = 300)
  
  resample.list <- data.frame()
  for(i in 1:nrow(individual.resamples.list)) {
    IRL.element <- individual.resamples.list[i,]
    preds.df <- filter(imputationPredsDF,
                       day == IRL.element$day,
                       cumulative == IRL.element$cumulative,
                       imputation == IRL.element$imputation,
                       Resample ==  IRL.element$Resample)
    
    resampleResults.row <- cbind(MLmethod = IRL.element$MLmethod,
                                 day = IRL.element$day,
                                 cumulative = IRL.element$cumulative,
                                 imputation = IRL.element$imputation,
                                 Resample =  IRL.element$Resample,
                                 customSummary(data.obs = preds.df$obs,
                                               data.pred = preds.df$pred,
                                               data.alive = preds.df$alive,
                                               invert = invert,
                                               switch = switch,
                                               regularise = TRUE))
    
    
    
    resample.list <- rbind(resample.list, resampleResults.row)
    
    setTkProgressBar(pb, i)
    
  }
  close(con = pb)
  
  return(resample.list)
}