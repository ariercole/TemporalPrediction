### Architecture for building and testing machine learning models with caret and deep learning ###

library(caret)
library(doMC)
library(mice)
library(dplyr)
source("functions/MLtuning/saveRDSFiles.R") # function to save files
source("functions/MLtuning/formulae.R") # define the required formulae
source("functions/MLtuning/tuneDeepLearningModel.R") # function to mimic caret in tuning a deep learning model


tuneMachineLearningModels <- function(Iter, DeepIter, MLmethods, path.D, seed.list, dayList = data.frame(day = c(1,2,2,3,3,4,4,5,5), cumul = c(F,F,T,F,T,F,T,F,T))) {

  for (MLmethod in MLmethods) {
    
    tune.grid <- get(paste("tune.grid.", MLmethod, sep = ""))
    
    for(i in 1:nrow(dayList)) {
      day <- dayList$day[i]
      cumul <- dayList$cumul[i]
  
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
        tuneDeepLearningModel(tune.grid = tune.grid, 
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