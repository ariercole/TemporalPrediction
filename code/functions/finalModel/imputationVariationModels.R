### Build the final model for each of nine imputations for each method and day

library(caret)
library(doMC)
library(mice)
library(dplyr)
source("functions/MLtuning/saveRDSFiles.R") # function to save files
source("functions/MLtuning/formulae.R") # define the required formulae
source("functions/MLtuning_output/getBestTune.R") 

imputationVariationModels <- function(MLmethods, path.impVar, dayList = data.frame(day = c(1,2,2,3,3,4,4,5,5), cumul = c(F,F,T,F,T,F,T,F,T))) {
  iter.base <- "IMPVAR"
  
  for (MLmethod in MLmethods) {
    
    best.tunes <- getBestTune(MLmethod)
    
    tuning.params <- colnames(best.tunes)[(grep("day", colnames(best.tunes)) + 1):(grep("ROC", colnames(best.tunes)) - 1)]
    
    for(i in 1:nrow(dayList)) {
      day <- dayList$day[i]
      cumul <- dayList$cumul[i]
      tune.grid <- best.tunes %>% filter(day == dayList$day[i], cumulative == cumul) %>% select(one_of(tuning.params))
      
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
                                imputVarTest = TRUE)
          
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
