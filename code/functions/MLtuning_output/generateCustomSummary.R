# functions to produce summary statistics for the resampled validation set predictions

library(dplyr)
library(pROC)
library(parallel)
library(tcltk)

# a function to produce a summary of the performance of one fold of one model (i.e. auc, sensitivity, specificity etc.)
customSummary <- function(data.obs, data.pred, data.alive, invert = FALSE, switch = FALSE, regularise = TRUE) {
  if (regularise == TRUE) {
    # regularise levels
    data.obs <- as.character(data.obs)
    data.obs <- factor(data.obs, levels = c("alive", "dead"))
    
    data.pred <- as.character(data.pred)
    data.pred <- factor(data.pred, levels = c("alive", "dead"))
  }
  
  # swap predictions if they are the wrong way round
  if (invert == TRUE) {
    data.alive <- 1 - data.alive
  }
  
  confMat.maxAcc <- caret::confusionMatrix(data = data.pred, reference = data.obs)
  
  # some resamples fail to converge and so the summary wil be unable to be generated 
  # test for this and only run the summary if it will not throw an error
  # generate ROC
  roc.x <- try(pROC::roc(response = ifelse(data.obs == "alive", 0, 1), predictor = data.alive))
  
  if (class(roc.x) == "try-error") {
    rocAUC <- NA
    sens.out.balAcc <- NA
    spec.out.balAcc <- NA
    PPV.out.balAcc <- NA
    NPV.out.balAcc <- NA
    maxAcc.balAcc <- NA
    kappa.balAcc <- NA
    balAcc.balAcc <- NA
  } else {
    # get AUC
    rocAUC <- pROC::auc(roc.x)
    
    # get coordinates for optimum balanced accuracy
    coords.x <- pROC::coords(roc.x, "best")
    
  
    pred.x <- factor(ifelse(data.alive > coords.x[1], "alive", "dead"), levels = c("alive","dead"))
    
    confMat.balanced <- caret::confusionMatrix(data = pred.x, reference = data.obs)
    
    sens.out.balAcc <- unname(confMat.balanced$byClass["Sensitivity"])
    spec.out.balAcc <- unname(confMat.balanced$byClass["Specificity"])
    PPV.out.balAcc <- unname(confMat.balanced$byClass["Pos Pred Value"])
    NPV.out.balAcc <- unname(confMat.balanced$byClass["Neg Pred Value"])
    maxAcc.balAcc <- unname(confMat.balanced$overall["Accuracy"])
    kappa.balAcc <- unname(confMat.balanced$overall["Kappa"])
    balAcc.balAcc <- unname(confMat.balanced$byClass["Balanced Accuracy"])
  }
  
  sens.out.maxAcc <- unname(confMat.maxAcc$byClass["Sensitivity"])
  spec.out.maxAcc <- unname(confMat.maxAcc$byClass["Specificity"])
  PPV.out.maxAcc <- unname(confMat.maxAcc$byClass["Pos Pred Value"])
  NPV.out.maxAcc <- unname(confMat.maxAcc$byClass["Neg Pred Value"])
  maxAcc.maxAcc <- unname(confMat.maxAcc$overall["Accuracy"])
  kappa.maxAcc <- unname(confMat.maxAcc$overall["Kappa"])
  balAcc.maxAcc <- unname(confMat.maxAcc$byClass["Balanced Accuracy"])
  
  output <- data.frame(AUROC = rocAUC,
                       Accuracy.maxAccuracy = maxAcc.maxAcc,
                       Kappa.maxAccuracy = kappa.maxAcc,
                       Sensitivity.maxAccuracy = sens.out.maxAcc,
                       Specificity.maxAccuracy = spec.out.maxAcc,
                       PosPredValue.maxAccuracy = PPV.out.maxAcc,
                       NegPredValue.maxAccuracy = NPV.out.maxAcc,
                       BalancedAccuracy.maxAccuracy = maxAcc.balAcc,
                       Accuracy.balanced = maxAcc.balAcc,
                       Kappa.balanced = kappa.balAcc,
                       Sensitivity.balanced = sens.out.balAcc,
                       Specificity.balanced = spec.out.balAcc,
                       PosPredValue.balanced = PPV.out.balAcc,
                       NegPredValue.balanced = NPV.out.balAcc,
                       BalancedAccuracy.balanced = balAcc.balAcc,
                       stringsAsFactors = FALSE)
  return(output)
}

# wrapper for the above function to apply it to the resampled data generated below
generateCustomSummary <- function(bestPred.df) {
  MLmethod <- bestPred.df$ML.method[1] # get ML method
  
  resamples.df <- unique(select(bestPred.df, day, bt.Str, cumulative, Resample))
  
  for (i in 1:nrow(resamples.df)) {
    resample <- resamples.df[i,]
    data <- filter(bestPred.df, 
                   day == resample$day,
                   bt.Str == resample$bt.Str,
                   Resample == resample$Resample, 
                   cumulative == resample$cumulative)
    
    invert <- ifelse(MLmethod == "svmRadialWeights", TRUE, FALSE)
    
    output <- cbind(MLmethod = MLmethod,
                    day = resample$day, 
                    cumulative = resample$cumulative,
                    bt.Str = as.character(resample$bt.Str), 
                    Resample = resample$Resample,
                    customSummary(data.obs = data$obs, 
                                  data.pred = data$pred, 
                                  data.alive = data$alive, 
                                  invert = invert,
                                  regularise = TRUE))
    
    if (i == 1) {
      ML.resampleSummary.out <- output
    } else {
      ML.resampleSummary.out <- rbind(ML.resampleSummary.out, output)
    }
  }
  return(ML.resampleSummary.out)
}
