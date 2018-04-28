# This code defines a function to run 20-fold cross-validation of a keras deep learning model when fed a tuning grid

##################  Save function  ##################
saveRDSFiles.DeepNN <- function(preds, results, day, iter, cumulative, path) {
  if (cumulative == TRUE) {
    cumul <- "cumulative"
  } else {
    cumul <- ""
  }
  
  for (folder in c("pred", "tuning", "results")) {
    if(!dir.exists(paste(path, folder, "DeepNN", sep = "/"))) {
      dir.create(paste(path, folder, "DeepNN", sep = "/"), recursive = TRUE)
    }
  }
  
  saveRDS(preds, paste(path,"/pred/DeepNN/ROC.tuningML.DeepNN.day",day,cumul,".iter",iter,".pred.RDS", sep = ""))
  saveRDS(results, paste(path,"/results/DeepNN/ROC.tuningML.DeepNN.day",day,cumul,".iter",iter,".results.RDS", sep = ""))
}

##################  Deep learning function  ##################
deepLearningModelTune <- function(tune.grid, dataset_input, day, cumulative, seed.list, file.path, iter, imputVarTest = FALSE, verbose = FALSE, no.parallel.cores, tranches.per.core = 2) {
  set.seed(42)
  
  # load required packages
  library(devtools)
  library(keras)
  library(caTools)
  library(ggplot2)
  library(caret)
  library(doMC)
  library(pROC)
  library(dplyr)
  
  # gather appropriate variables
  if(cumulative == FALSE) {
    dataset <- select(dataset_input, 
                      alive, icu_duration_days, age, sex,
                      contains(paste("day", day, sep = "")))
  } else {
    dataset <- select(dataset_input, 
                      alive, icu_duration_days, age, sex,
                      contains("day1"))
    for(day_add in 2:day) {
      dataset <- cbind(dataset, select(dataset_input, 
                                       contains(paste("day", day_add, sep = ""))))
    }
  }
  
  # filter out values not expected to be present for all days (i.e. patients who leave before day of interest)
  dataset <- filter(dataset, icu_duration_days >= day)
  
  # save a copy of the outcome-labelled dataset and then convert outcome to binary 1/0
  dataset.1 <- dataset
  dataset$alive <- as.factor(ifelse(dataset$alive == "alive", 1, 0))
  
  ### run a logistic classifier to generate the indices for each fold according to the defined seed list (i.e. to match the folds for the models built through caret)
  train.control <- trainControl(method = "LGOCV", 
                                number = 20, 
                                classProbs = TRUE, 
                                verboseIter = FALSE, 
                                seeds = seed.list,
                                savePredictions = "all",
                                summaryFunction = twoClassSummary,
                                returnResamp = "all",
                                p = 0.8)
  
  logistic.classifier <- train(alive ~ age, 
                               dataset.1, 
                               method = "glm",
                               metric = "ROC",
                               trControl = train.control)
  
  # define function to be parallelised which perfoms 20 fold cross-validation of the deep learning model to produce an output equivalent to caret output
  runTuneGridRow <- function(tuneRow, tune.grid, day, cumulative, iter, dataset, logistic.classifier, verbose) {
    library(devtools)
    library(keras)
    library(caTools)
    library(ggplot2)
    library(caret)
    library(pROC)
    library(dplyr)
    
    # define output dataframes for the tune row
    pred.tuneRow <- data.frame()
    results.tuneRow <- data.frame()
    
    ### extract tuning parameters from the tune grid
    complexity.multiplier <- tune.grid[tuneRow, "complexity.multiplier"]
    activation.layer_1 <- as.character(tune.grid[tuneRow, "activation.layer_1"])
    activation.layer_2 <- as.character(tune.grid[tuneRow, "activation.layer_2"])
    activation.layer_3 <- as.character(tune.grid[tuneRow, "activation.layer_3"])
    activation.layer_4 <- as.character(tune.grid[tuneRow, "activation.layer_4"])
    activation.layer_5 <- as.character(tune.grid[tuneRow, "activation.layer_5"])
    activation.layer_6 <- as.character(tune.grid[tuneRow, "activation.layer_6"])
    rate.dropout_1 <- tune.grid[tuneRow, "rate.dropout_1"]
    rate.dropout_2 <- tune.grid[tuneRow, "rate.dropout_2"]
    rate.dropout_3 <- tune.grid[tuneRow, "rate.dropout_3"]
    rate.dropout_4 <- tune.grid[tuneRow, "rate.dropout_4"]
    rate.dropout_5 <- tune.grid[tuneRow, "rate.dropout_5"]
    rate.dropout_6 <- tune.grid[tuneRow, "rate.dropout_6"]
    Epochs <- tune.grid[tuneRow, "Epochs"]
    
    # define a holidng dataframe for the results which will then be used for summary statistics for the overall "results" output
    results.raw <- data.frame()
    
    # build and test the deep learning model for each fold of 20 folds
    for (fold in 1:20) {
      # name the resample by caret nomenclature
      resample.name <- paste(ifelse(fold <= 9, "Resample0", "Resample"), fold, sep = "")
      
      # define the row indexes of the traiing and test data
      indexIn <- logistic.classifier$control$index[[fold]]
      indexOut <- logistic.classifier$control$indexOut[[fold]]
      
      # split into training / test set according to the fold generated by the caret model
      training.df.raw <- dataset[indexIn, ]
      test.df.raw <- dataset[indexOut, ]
      
      # preprocess the data to match the caret pre-processing
      training.df.preProc <- preProcess(training.df.raw, method = c("center", "scale", "YeoJohnson"))
      
      training.df <- predict(training.df.preProc, training.df.raw)
      test.df <- predict(training.df.preProc, test.df.raw)
      
      # split the dataset into "x" and "y" and prepare the "y" data for training
      xtrain <- data.matrix(select(training.df, -alive))
      xtest <- data.matrix(select(test.df, -alive))
      ytrain <- data.matrix(select(training.df, alive))
      ytest.raw <- data.matrix(select(test.df, alive))
      
      ytrain <- to_categorical(ytrain, 3)
      ytest <- to_categorical(ytest.raw, 3)
      
      # define the model
      set.seed(42)
      model <- keras_model_sequential()   
      model %>% 
        layer_dense(units = 128 * complexity.multiplier, activation = activation.layer_1, input_shape = c(ncol(xtrain)), trainable = TRUE) %>% 
        layer_dropout(rate = rate.dropout_1) %>% 
        layer_dense(units = 96 * complexity.multiplier, activation = activation.layer_2) %>%
        layer_dropout(rate = rate.dropout_2) %>%
        layer_dense(units = 64 * complexity.multiplier, activation = activation.layer_3) %>%
        layer_dropout(rate = rate.dropout_3) %>%
        layer_dense(units = 48 * complexity.multiplier, activation = activation.layer_4) %>%
        layer_dropout(rate = rate.dropout_4) %>% 
        layer_dense(units = 32 * complexity.multiplier, activation = activation.layer_5) %>%
        layer_dropout(rate = rate.dropout_5) %>% 
        layer_dense(units = 16 * complexity.multiplier, activation = activation.layer_6) %>%
        layer_dropout(rate = rate.dropout_6) %>% 
        layer_dense(units = 3, activation = 'softmax')  
      
      # print a summary of the model
      summary(model)
      
      # compile the model
      model %>% compile(
        loss = 'binary_crossentropy',
        optimizer = "Adam",
        metrics = c('accuracy')
      )
      
      # fit the model
      history <- model %>% fit(
        xtrain, ytrain, 
        epochs = Epochs, batch_size = 128, 
        validation_split = 0.2, 
        verbose = T
      )
      
      # predict the probabilities of the binary outcome on the validation dataset
      y_pred_prob <- model %>% predict(xtest)
      roc.y <- pROC::roc(as.vector(ytest.raw), as.vector(y_pred_prob[,3]))

      # predict the binary outcome class (alive/dead) on the validation dataset
      y_pred <- model %>% predict_classes(xtest)
      conf.mat <- confusionMatrix(y_pred - 1, ytest[,3])
      
      # build the predictions data frame to store all the predictions for the current fold
      suppressWarnings(
        pred.fold <- cbind(data.frame(pred = ifelse(y_pred == 2, "alive", "dead"),
                                      obs = ifelse(as.vector(ytest.raw) == 2, "alive", "dead"),
                                      dead = as.vector(y_pred_prob[,2]),
                                      alive = as.vector(y_pred_prob[,3]),
                                      rowIndex = indexOut),
                           tune.grid[tuneRow, ],
                           Resample = resample.name)
      )
      
      # generate the results of the current fold
      results.fold <- data.frame(ROC = pROC::auc(roc.y),
                                 Sens = conf.mat$byClass["Sensitivity"],
                                 Spec = conf.mat$byClass["Specificity"])
      
      # add the results and preds of this fold to the list of all folds for this tuning row
      pred.tuneRow <- rbind(pred.tuneRow, pred.fold)
      results.raw <- rbind(results.raw, results.fold)
      
      # print confirmation of fold completion
      if (verbose == TRUE) print(paste("Completed: Tune row ", tuneRow, " fold ", fold, sep = ""))
    }

    # coompile the results of each fold into a single row DF of summarised results for this tune
    results.tuneRow <- cbind(tune.grid[tuneRow, ], 
                             ROC = mean(results.raw$ROC),
                             Sens = mean(results.raw$Sens),
                             Spec = mean(results.raw$Spec),
                             ROCSD = sd(results.raw$ROC),
                             SensSD = sd(results.raw$Sens),
                             SpecSD = sd(results.raw$Spec))
    
    if (verbose == TRUE) print(paste("Day ", day, ifelse(cumulative == TRUE, " cumulative,", ","), " iter ", iter, ": Completed tuning grid row ", tuneRow, " at ", Sys.time(), sep = ""))
    
    # return a list containing two dataframes: 1. The predictions made for each fold on this tune row. 2. A one row DF of the results (i.e. ROC/sens/spec) for this tune
    return(list(pred.tuneRow, results.tuneRow))
  }
  
  # determine number of models in each tranche and thus number of trances reuired for the given tuning grid
  tranche.size <- no.parallel.cores * tranches.per.core
  no.tranches <- ceiling(nrow(tune.grid)/tranche.size)
  
  for(tranche in 1:no.tranches) {
    # select the list tuning parameters for the current tranche
    tune.grid.subset <- tune.grid[(1 + ((tranche - 1) * tranche.size)) : min(c((tranche * tranche.size),nrow(tune.grid))), ]
  
    # set up parallel cluster
    cl <- makeCluster(no.parallel.cores, outfile = "")
    
    # run all the tune rows in current tranche in parallel
    outputList <- parLapply(cl = cl, 
                            X = 1:nrow(tune.grid.subset),
                            fun = runTuneGridRow, 
                            tune.grid = tune.grid.subset, 
                            day = day, 
                            cumulative = cumulative, 
                            iter = iter, 
                            dataset = dataset, 
                            logistic.classifier = logistic.classifier, 
                            verbose = verbose)
    #stop the cluster
    stopCluster(cl)
    
    # define output dataframes
    pred.out <- data.frame()
    results.out <- data.frame()
    
    # fill output dataframes
    for(i in 1:length(outputList)) {
      pred.out <- rbind(pred.out, outputList[[i]][[1]])
      results.out <- rbind(results.out, outputList[[i]][[2]])
      
    }
    
    # save output dataframes
    saveRDSFiles.DeepNN(preds = pred.out, results = results.out, day = day, iter = ifelse(imputVarTest == TRUE, iter, iter + tranche - 1), cumulative = cumulative, path = file.path)
    
    # print tranche completion confirmation
    print(paste("Day ", day, ifelse(cumulative == TRUE, " cumulative,", ","), ": Completed tuning grid tranche ", tranche, " out of ", no.tranches, " at ", Sys.time(), sep = ""))
    
    # free memory ready for next tranche
    rm(outputList)
    rm(pred.out)
    rm(results.out)
    gc()
  }
}
