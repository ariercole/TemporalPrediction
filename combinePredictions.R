### Combine the predictions and results for each model type into big dataframes and generate output summaries

library(dplyr)
library(pROC)
library(parallel)
library(tcltk)

path.D <- "~/RDSfiles"

#### 1. Load the model dfs ####
model.df.all <- readRDS("model.df.all.RDS")

#### 2. Define functions ####

# function to combine results or predicitons into one data frame
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

# determine the tuning functions for the machine learning method in question
getTuningParams <- function(input.df, type = "results") {
  if (type == "results") {
    return(colnames(input.df)[1:grep("ROC", colnames(input.df))[1]-1])
  } else if (type == "pred") {
    return(colnames(input.df)[(grep("rowIndex", colnames(input.df))[1] + 1) : (grep("Resample", colnames(input.df))[1] - 1)])
  }
}

# find the best performing tuning parameters for each day for a machine learning method
getBestTune <- function(MLmethod) {
  results.df <- get(paste("ML.results.df.", MLmethod, sep = ""))

  tuning.params <- getTuningParams(results.df, "results")
  
  bestPreds <- results.df %>% group_by(day, cumulative) %>% summarise(ROC.best = max(ROC))
  
  bestTune <- as.data.frame(results.df[results.df$ROC %in% bestPreds$ROC.best, c("day", tuning.params,"ROC", "iter", "cumulative")])

  return(bestTune)
}

# retrieve the predictions made for the validation dataset from the best performing tuning parameters for a given machine learning method
getBestTunePreds <- function(MLmethod) {
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
    path = path.D
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

# a function to produce a summary of the performance of one fold of one model (i.e. auc, sensitivity, specificity etc.)
customSummary <- function(data.obs, data.pred, data.alive, invert = FALSE, switch = FALSE, regularise = TRUE) {
  if (regularise == TRUE) {
    # regularise levels
    data.obs <- as.character(data.obs)
    data.obs <- factor(data.obs, levels = c("alive", "dead"))
    
    data.pred <- as.character(data.pred)
    data.pred <- factor(data.pred, levels = c("alive", "dead"))
  }
  
  if (invert == TRUE) data.alive <- 1 - data.alive # swap predictions if they are the wrong way round
  
  confMat.maxAcc <- caret::confusionMatrix(data = data.pred, reference = data.obs)
  
  # generate ROC
  roc.x <- pROC::roc(response = ifelse(data.obs == "alive", 0, 1), predictor = data.alive)
  
  # get AUC
  rocAUC <- pROC::auc(roc.x)
  
  # get coordinates for optimum balanced accuracy
  coords.x <- pROC::coords(roc.x, "best")
  

  pred.x <- factor(ifelse(data.alive > coords.x[1], "alive", "dead"), levels = c("alive","dead"))
  
  confMat.balanced <- caret::confusionMatrix(data = pred.x, reference = data.obs)
  
  sens.out.maxAcc <- unname(confMat.maxAcc$byClass["Sensitivity"])
  spec.out.maxAcc <- unname(confMat.maxAcc$byClass["Specificity"])
  PPV.out.maxAcc <- unname(confMat.maxAcc$byClass["Pos Pred Value"])
  NPV.out.maxAcc <- unname(confMat.maxAcc$byClass["Neg Pred Value"])
  
  sens.out.balAcc <- unname(confMat.balanced$byClass["Sensitivity"])
  spec.out.balAcc <- unname(confMat.balanced$byClass["Specificity"])
  PPV.out.balAcc <- unname(confMat.balanced$byClass["Pos Pred Value"])
  NPV.out.balAcc <- unname(confMat.balanced$byClass["Neg Pred Value"])
  
  
  output <- data.frame(AUROC = rocAUC,
                       Accuracy.maxAccuracy = unname(confMat.maxAcc$overall["Accuracy"]),
                       Kappa.maxAccuracy = unname(confMat.maxAcc$overall["Kappa"]),
                       Sensitivity.maxAccuracy = sens.out.maxAcc,
                       Specificity.maxAccuracy = spec.out.maxAcc,
                       PosPredValue.maxAccuracy = PPV.out.maxAcc,
                       NegPredValue.maxAccuracy = NPV.out.maxAcc,
                       BalancedAccuracy.maxAccuracy = unname(confMat.maxAcc$byClass["Balanced Accuracy"]),
                       Accuracy.balanced = unname(confMat.balanced$overall["Accuracy"]),
                       Kappa.balanced = unname(confMat.balanced$overall["Kappa"]),
                       Sensitivity.balanced = sens.out.balAcc,
                       Specificity.balanced = spec.out.balAcc,
                       PosPredValue.balanced = PPV.out.balAcc,
                       NegPredValue.balanced = NPV.out.balAcc,
                       BalancedAccuracy.balanced = unname(confMat.balanced$byClass["Balanced Accuracy"]),
                       stringsAsFactors = FALSE)
  return(output)
}

# wrapper for the above function to apply it to the resampled data generated below
generateCustomSummary <- function(bestPred.df, MLmethod) {
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
