#### Master Code: Tuning of machine learning models by 20 fold cross-vlaidation to discover optimal values for tuning parameters ####

#### 1. Set up environment ####

# load the functions
source("functions/MLtuning/tuneDeepLearningModel.R") # functions to build deep learning models
source("functions/MLtuning/tuneMachineLearningModels.R") # functions to train logistic regressions and machine learning models
source("functions/MLtuning_output/generateModelDF.R") # defines a function to determine which models have been completed
source("functions/MLtuning_output/buildPredResDF.R") # function to compile results or predictions into one dataframe
source("functions/MLtuning_output/getBestTunePreds.R") # function to compile validation set predictions for the optimal tuning parameters
source("functions/MLtuning_output/generateCustomSummary.R") # function to summarise validation predicions into ROC, sensitivity, specificity, etc.
source("functions/graph_plotting/plotTuningGraphs.R")
source("functions/graph_plotting/plotMLOutputGraph.R")

# set directory to save the predictions and results files
path.save <- "~/RDSfiles"

# set seeds for tuning caret models
set.seed(42)
seed.list <- list()
for (i in 1:20) {
  seed.list[[i]] <- sample.int(n = 10000, size = 200)
}
seed.list[[21]] <- sample.int(n = 10000, size = 1)
seed.list

# set the number of parallel cores
no.parallel.cores <- floor(2 * detectCores() / 3)
registerDoMC(cores = no.parallel.cores)

# define the train control for all models
train.control <- trainControl(method = "LGOCV", 
                              number = 20, 
                              summaryFunction = twoClassSummary, 
                              classProbs = TRUE, 
                              verboseIter = FALSE,
                              seeds = seed.list,  
                              savePredictions = "all",
                              returnResamp = "all",
                              p = 0.8)

#### 2. First tuning run ####

# define the tuning grids for the first run
tune.grid.avNNet <- expand.grid(size = c(2,3,5,10,20), 
                                decay = c(0.5,1,3,4,5,10), 
                                bag = c(TRUE,FALSE))
tune.grid.svmRdialWeights <- expand.grid(C = c(1,3,5,10,20), 
                                         Weight = c(0.1,0.5,1,2,3,5,10),
                                         sigma = c(0.0005,0.001,0.005,0.01,0.05))
tune.grid.adaboost <- expand.grid(method = "Adaboost.M1", nIter = c(10,30,100,300,1000))
tune.grid.DeepNN <- expand.grid(complexity.multiplier = c(0.3,0.5,0.8,1,1.2),
                                activation.layer_1 = c('tanh', 'relu'),
                                activation.layer_2 = c('tanh', 'relu'),
                                activation.layer_3 = c('sigmoid', 'tanh', 'relu'),
                                activation.layer_4 = 'tanh',
                                activation.layer_5 = 'relu',
                                activation.layer_6 = 'relu',
                                rate.dropout_1 = c(0.3,0.4),
                                rate.dropout_2 = c(0.2,0.3),
                                rate.dropout_3 = 0.2,
                                rate.dropout_4 = 0.2,
                                rate.dropout_5 = 0.2,
                                rate.dropout_6 = 0.2,
                                Epochs = c(30,50))

# run the model building function
iter <- 1 # iteration 1 for caret models
deep.iter <- 1 # iteration 1 for deep learning models
ML.methods <- c("adaboost", "APACHE", "avNNet", "DeepNN", "glm", "parRF", "svmRadialWeights") # define the methods to use
day.cumul <- data.frame(day = c(1,2,2,3,3,4,4,5,5), 
                        cumul = c(F,F,T,F,T,F,T,F,T)) # define the days on which to build models

tuneMachineLearningModels(Iter = iter, 
                          DeepIter = deep.iter, 
                          MLmethods = ML.methods, 
                          seed.list = seed.list,
                          path.D = path.save, 
                          dayList = day.cumul, 
                          fullData.imput.in = fullData.imput1)


#### 3. Determine tuning outcomes ####

# determine which models have been run 
model.df.all <- generateModelDF()

# combine the results into one dataframe for each machine learning method
# build dataframes of the results for each machine learning method
for (MLmethod in c("adaboost", "APACHE", "avNNet", "DeepNN", "glm", "parRF", "svmRadialWeights")) {
  assign(paste("ML.results.df.", MLmethod, sep = ""), 
         buildPredResDF(MLmethod = MLmethod,  
                        pred.res = "results", 
                        model.df = model.df.all, 
                        path = path.save))
}

# d. Plot graphs to inspect tuning results and determine any further tuning required
plotTuningGraphs()

#### 4. Second tuning run ####

# define the tuning grids for the second tuning run
tune.grid.adaboost <- expand.grid(method = "Adaboost.M1", nIter = c(3000,5000))
tune.grid.DeepNN <- expand.grid(complexity.multiplier = c(seq(from = 3/16, to = 20/16, by = 1/16)),
                                activation.layer_1 = 'tanh',
                                activation.layer_2 = 'tanh',
                                activation.layer_3 = 'sigmoid',
                                activation.layer_4 = 'tanh',
                                activation.layer_5 = 'relu',
                                activation.layer_6 = 'relu',
                                rate.dropout_1 = c(0.4,0.5,0.6),
                                rate.dropout_2 = c(0.3,0.4,0.5),
                                rate.dropout_3 = c(0.2,0.3),
                                rate.dropout_4 = 0.2,
                                rate.dropout_5 = 0.2,
                                rate.dropout_6 = 0.2,
                                Epochs = 30)
tune.grid.svmRadialWeights <- expand.grid(C = c(0.1,0.3,0.5,0.8), 
                                          Weight = c(2,3,4,5,7.5,10), 
                                          sigma = c(0.001,0.025,0.005,0.01))


# run the model building function
iter <- 2 # iteration 2 for caret models
deep.iter <- 1 + nrow(tune.grid.DeepNN) / (2 * no.parallel.cores) # calculate iteration for deep leanring models
ML.methods <- c("adaboost", "DeepNN", "svmRadialWeights")

tuneMachineLearningModels(Iter = iter, 
                          DeepIter = deep.iter, 
                          MLmethods = ML.methods,
                          seed.list = seed.list, 
                          path.D = path.save, 
                          dayList = day.cumul,
                          fullData.imput.in = fullData.imput1)

#### 5. Plot outputs ####

model.df.all <- generateModelDF()


# build dataframes of the results for each machine learning method
for (MLmethod in c("adaboost", "APACHE", "avNNet", "DeepNN", "glm", "parRF", "svmRadialWeights")) {
  assign(paste("ML.results.df.", MLmethod, sep = ""), 
         buildPredResDF(MLmethod = MLmethod,  
                        pred.res = "results", 
                        model.df = model.df.all, 
                        path = path.save))
}

# plot graphs to inspect tuning results and determine any further tuning required
plotTuningGraphs()

# Calculate summary statistics for all 20 folds for each day for each machine learning method (time consuming - may choose to operate in parallel)
for (MLmethod in c("adaboost", "APACHE", "avNNet", "DeepNN", "glm", "parRF", "svmRadialWeights")) {
  assign(paste("ML.resampleResults.df.", MLmethod, sep = ""), 
         getBestTunePreds(MLmethod, path = path.save) %>% generateCustomSummary())
}

ML.resampleResults.df.ALL <- rbind(ML.resampleResults.df.adaboost,
                                   ML.resampleResults.df.avNNet,
                                   ML.resampleResults.df.parRF,
                                   ML.resampleResults.df.svmRadialWeights,
                                   ML.resampleResults.df.glm,
                                   ML.resampleResults.df.DeepNN,
                                   ML.resampleResults.df.APACHE)

# g. plot graphs to show the interim results before imputaiton variability has been assessed
plotMLOutputGraph(plotMetric = "AUROC", 
                  plotName = "AUROC", 
                  resampleResults = ML.resampleResults.df.ALL,
                  day.range = 5, 
                  MLmethods = c("glm", "parRF", "adaboost", "avNNet", "svmRadialWeights", "DeepNN"), 
                  file.name = "AUROC_allMLmethods", 
                  lines.plot = FALSE)
plotMLOutputGraph(plotMetric = "AUROC",
                  plotName = "AUROC", 
                  resampleResults = ML.resampleResults.df.ALL, 
                  day.range = 5, MLmethods = c("glm"), 
                  file.name = "AUROC_glm", 
                  lines.plot = FALSE)
plotMLOutputGraph(plotMetric = "AUROC", 
                  plotName = "AUROC", 
                  resampleResults = ML.resampleResults.df.ALL,
                  day.range = 5, MLmethods = c("APACHE"), 
                  file.name = "AUROC_APACHE", 
                  lines.plot = FALSE)

# GLM vs. DeepNN
plotMLOutputGraph(plotMetric = "AUROC", 
                  plotName = "AUROC", 
                  resampleResults = ML.resampleResults.df.ALL, 
                  day.range = 5, 
                  MLmethods = c("glm", "DeepNN"), 
                  file.name = "AUROC_glmDeepNN_lines", 
                  lines.plot = TRUE)

# APACHE vs. DeepNN
plotMLOutputGraph(plotMetric = "AUROC", 
                  plotName = "AUROC", 
                  resampleResults = ML.resampleResults.df.ALL, 
                  day.range = 5, 
                  MLmethods = c("APACHE", "DeepNN"), 
                  file.name = "AUROC_APACHEcompareDeepNN_lines", 
                  lines.plot = TRUE)
