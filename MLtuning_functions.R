### Architecture for building and testing machine learning models with caret and deep learning ###

library(caret)
library(doMC)
library(mice)
library(dplyr)

fullData.imput1 <- readRDS("fullData.imput1.RDS")
source("formulae.R")

# set path to save the output files
path.D <- "~/RDSfiles"
path.impVar <- "~/RDSfiles/imputationVariation/"

#### 1. Set seeds for caret train control and print the list of seeds ####
set.seed(42)
seed.list <- list()
for (i in 1:20) {
  seed.list[[i]] <- sample.int(n = 10000, size = 200)
}
seed.list[[21]] <- sample.int(n = 10000, size = 1)
seed.list

#### 2. Register number of cores ####
no.parallel.cores <- floor(2 * detectCores() / 3)
registerDoMC(cores = no.parallel.cores)

#### 3. Define save function to save the model, results and predictions for the resamples ####
saveRDSFiles <- function(classifier.x, MLmethod, day, iter, cumulative, path, saveClassifier = TRUE) {
  if (cumulative == TRUE) {
    cumul <- "cumulative"
  } else {
    cumul <- ""
  }
  
  for (folder in c("pred", "tuning", "results")) {
    if(!dir.exists(paste(path, folder, MLmethod, sep = "/"))) {
      dir.create(paste(path, folder, MLmethod, sep = "/"), recursive = TRUE)
    }
  }
  
  if(saveClassifier == TRUE) saveRDS(classifier.x, paste(path,"/tuning/",MLmethod,"/ROC.tuningML.",MLmethod,".day",day,cumul,".iter",iter,".RDS", sep = ""))
  saveRDS(classifier.x$pred, paste(path,"/pred/",MLmethod,"/ROC.tuningML.",MLmethod,".day",day,cumul,".iter",iter,".pred.RDS", sep = ""))
  saveRDS(classifier.x$results, paste(path,"/results/",MLmethod,"/ROC.tuningML.",MLmethod,".day",day,cumul,".iter",iter,".results.RDS", sep = ""))
}

#### 4. Define the rain control for use across all caret models ####
train.control <- trainControl(method = "LGOCV", 
                              number = 20, 
                              summaryFunction = twoClassSummary, 
                              classProbs = TRUE, 
                              verboseIter = FALSE,
                              seeds = seed.list,  
                              savePredictions = "all",
                              returnResamp = "all",
                              p = 0.8)


#### 5. Define days for model building ####
day.cumul <- data.frame(day = c(1,2,2,3,3,4,4,5,5), cumul = c(F,F,T,F,T,F,T,F,T))


#### 6. Define function to build the models ####
#        for each tune for each day, simple and cumultive, selecting the formulas with interactions for glm and those 
#        without interactions for all other methods, and just the APACHE score for the APACHE glm 

tuneModels <- function(Iter, DeepIter, MLmethods) {
  for (MLmethod in MLmethods) {
    
    tune.grid <- get(paste("tune.grid.", MLmethod, sep = ""))
    
    for(i in 1:nrow(day.cumul)) {
      day <- day.cumul$day[i]
      cumul <- day.cumul$cumul[i]
  
      if(cumul == T) {
        formula.custom <- get(paste(c("formula.day",1:day,".custom"), collapse = ""))
        formula.simple <- get(paste(c("formula.day",1:day,".simple"), collapse = ""))
      } else {
        formula.custom <- get(paste("formula.day",day,".custom",sep=""))
        formula.simple <- get(paste("formula.day",day,".simple",sep=""))
      }
      
      trainingData <- filter(fullData.imput1, icu_duration_days >= day)
        
      set.seed(42)
      if(MLmethod == "glm") {
        classifier <- train(formula.custom, 
                            trainingData, 
                            method = MLmethod,
                            metric = "ROC", 
                            trControl = train.control, 
                            preProcess = c("YeoJohnson", "center", "scale"))
        
      } else if (MLmethod == "APACHE") {
        classifier <- train(formula.APACHE, 
                            trainingData, 
                            method = "glm",
                            metric = "ROC", 
                            trControl = train.control, 
                            preProcess = c("YeoJohnson", "center", "scale"))
        
      } else if (MLmethod == "DeepNN") {
        deepLearningModelTune(tune.grid = tune.grid, 
                              dataset_input = trainingData, 
                              day = day, 
                              cumulative = cumul, 
                              seed.list = seed.list, 
                              file.path = path.D,
                              iter = DeepIter, 
                              verbose = TRUE,
                              no.parallel.cores = no.parallel.cores,
                              tranches.per.core = 2,
                              imputVarTest = FALSE)
        
      } else {
        classifier <- train(formula.simple, 
                            trainingData, 
                            method = MLmethod,
                            metric = "ROC", 
                            trControl = train.control, 
                            preProcess = c("YeoJohnson", "center", "scale"),
                            tuneGrid = tune.grid)
      }
      
      if(MLmethod != "DeepNN") {
        saveRDSFiles(classifier.x = classifier, MLmethod = MLmethod, day = day, iter = Iter, cumulative = cumul, path = path.D)
        rm(classifier)
      }
      gc()
    }
  }
}


#### 7. Define function to test imputation variation for each model, once the optimal tuning paramaters have been establised ####
imputationVariationModels <- function(MLmethods) {
  iter.base <- "IMPVAR"
  day.cumul <- data.frame(day = c(1,2,2,3,3,4,4,5,5), cumul = c(F,F,T,F,T,F,T,F,T))
  path.D <- 
  
  for (MLmethod in MLmethods) {
    
    best.tunes <- getBestTune(MLmethod)
    
    tuning.params <- colnames(best.tunes)[(grep("day", colnames(best.tunes)) + 1):(grep("ROC", colnames(best.tunes)) - 1)]
    
    for(i in 1:nrow(day.cumul)) {
      day <- day.cumul$day[i]
      cumul <- day.cumul$cumul[i]
      tune.grid <- best.tunes %>% filter(day == day.cumul$day[i], cumulative == cumul) %>% select(one_of(tuning.params))
      
      if(cumul == T) {
        formula.custom <- get(paste(c("formula.day",1:day,".custom"), collapse = ""))
      } else {
        formula.custom <- get(paste("formula.day",day,".custom",sep=""))
      }
      
      
      # run model for best tune on each day
      for(imput.number in 1:9) {
        iter <- paste(iter.base, imput.number, sep = "")
        imputedSet <- mice::complete(imputedData.all, imput.number)
        
        trainingData <- filter(imputedSet, icu_duration_days >= day)
        
        set.seed(42)
        if(MLmethod == "glm") {
          classifier <- train(formula.custom, 
                              trainingData, 
                              method = MLmethod,
                              metric = "ROC", 
                              trControl = train.control, 
                              preProcess = c("YeoJohnson", "center", "scale"))
          
        } else if (MLmethod == "APACHE") {
          classifier <- train(formula.APACHE, 
                              trainingData, 
                              method = "glm",
                              metric = "ROC", 
                              trControl = train.control, 
                              preProcess = c("YeoJohnson", "center", "scale"))
          
        } else if (MLmethod == "DeepNN") {
          deepLearningModelTune(tune.grid = tune.grid, 
                                dataset_input = trainingData, 
                                day = day, 
                                cumulative = cumul, 
                                seed.list = seed.list, 
                                file.path = path.impVar,
                                iter = iter, 
                                verbose = TRUE,
                                no.parallel.cores = 1,
                                tranches.per.core = 1,
                                iterVar = TRUE)
          
        } else {
          classifier <- train(formula.custom, 
                              trainingData, 
                              method = MLmethod,
                              metric = "ROC", 
                              trControl = train.control, 
                              preProcess = c("YeoJohnson", "center", "scale"),
                              tuneGrid = tune.grid)
        }
        
        if(MLmethod != "DeepNN") {
          saveRDSFiles(classifier.x = classifier, MLmethod = MLmethod, day = day, iter = iter, cumulative = cumul, path = path.impVar, saveClassifier = FALSE)
        }
      }
    }
  }
}
