#### Master Code ####

source("loadData.R") # loads dataset into a formatted dataframe
source("plotGraphs.R") # functions for plotting descriptive graphs
source("removeOutliers.R") # functions for removal of outliers
source("parlMICE.R") # parallel wrapper for the mice function
source("MICE_impute.R") # functions for imputation of missing data

#### 1. Prepare dataSet ####
# cut patients who have no classifier
dataSet.all <- filter(dataSet, !is.na(alive))

# factorise the classifiers
dataSet.all$alive_dead_icu = as.factor(dataSet.all$alive_dead_icu)
levels(dataSet.all$alive_dead_icu) = c("Alive","Dead","E")
levels(dataSet.all$alive) <- c("dead","alive")

# plot raw graphs
plotGraphs(dataSet.all, directory = "Graphs-Raw", 4000, 2000)

#### 2. Remove outliers ####
# apply transformation to ventilation data
tempDataSet.all <- transformVentilationData(dataSet.all)
cleanData.all <- removeOutliers(tempDataSet.all, "singleDaySeparate")

# plot cleaned graphs
plotGraphs(cleanData.all, directory = "Graphs-Cleaned", 4000, 2000)
plotGraphs(cleanData.all, directory = "Graphs-Cleaned-noTitles", 4000, 2500, titled = FALSE, legend = FALSE, ptSize = 20)


#### 3. Impute missing values ####
set.seed(42)
imputedData.all <- impute.custom(cleanData.all) ########### TODO: need to test imputation
saveRDS(imputedData.all, "imputedData.all.RDS")

# plot graphs comparing imputations and original distribution
manualDensityPLots(mids = imputedData.all, cleanData = cleanData.all)
manualDensityPLots(mids = imputedData.all, cleanData = cleanData.all, title = FALSE, minititles = FALSE, plotDir = "densityPLots_manual_vFig")

# save an imputed dataset as an RDS file
fullData.imput1 <- complete.custom(imputedData.all, 1)
saveRDS(fullData.imput1, "fullData.imput1.RDS")

#### 4. Tune machine learning models and build lgoistic regression models tuning runs ####
source("formulae.R") # define the required formulae
source("deepLearningFunctions.R") # functions to build deep learning models
source("MLtuning_functions.R") # functions to train logistic regressions and machine learning models

# a. First iteration of tuning for the ML methods 
source("ML_tuning_run_1.R") # performs the firs iteration of tuning for all machine learning methods

# b. Determine which models have been run 
source("generateModelDF.R") # defines a function to determine which models have been completed
model.df.all <- generateModelDF()

# c. Combine the results into one dataframe for each machine learning method
source("combinePredictions.R") # functions to combine all the results into dataframes for each ML method
# build dataframes of the results for each machine learning method
ML.results.df.adaboost         <- buildPredResDF(MLmethod = "adaboost",  pred.res = "results", model.df = model.df.all, path = path.D)
ML.results.df.avNNet           <- buildPredResDF(MLmethod = "avNNet",    pred.res = "results", model.df = model.df.all, path = path.D)
ML.results.df.parRF            <- buildPredResDF(MLmethod = "parRF",     pred.res = "results", model.df = model.df.all, path = path.D)
ML.results.df.svmRadialWeights <- buildPredResDF(MLmethod = "svmRadialWeights", pred.res = "results", model.df = model.df.all, path = path.D)
ML.results.df.glm              <- buildPredResDF(MLmethod = "glm",       pred.res = "results", model.df = model.df.all, path = path.D)
ML.results.df.DeepNN           <- buildPredResDF(MLmethod = "DeepNN",    pred.res = "results", model.df = model.df.all, path = path.D)
ML.results.df.APACHE           <- buildPredResDF(MLmethod = "APACHE",    pred.res = "results", model.df = model.df.all, path = path.D)

# d. Plot graphs to inspect tuning results and determine any further tuning required
source("plotTuningGraphs.R")

# e. Second iteration of tuning for the ML methods which show clear directions for optimal tuning or have parameters as yet
source("ML_tuning_run_2.R")
model.df.all <- generateModelDF()

# f. Plot outputs before testing for imputation variation
# Calculate summary statistics for all 20 folds for each day for each machine learning method (time consuming - may choose to operate in parallel)
ML.resampleResults.df.adaboost         <- getBestTunePreds("adaboost")         %>% generateCustomSummary("adaboost")
ML.resampleResults.df.avNNet           <- getBestTunePreds("avNNet")           %>% generateCustomSummary("avNNet")
ML.resampleResults.df.parRF            <- getBestTunePreds("parRF")            %>% generateCustomSummary("parRF")
ML.resampleResults.df.svmRadialWeights <- getBestTunePreds("svmRadialWeights") %>% generateCustomSummary("svmRadialWeights")
ML.resampleResults.df.glm              <- getBestTunePreds("glm")              %>% generateCustomSummary("glm")
ML.resampleResults.df.DeepNN           <- getBestTunePreds("DeepNN")           %>% generateCustomSummary("DeepNN")
ML.resampleResults.df.APACHE           <- getBestTunePreds("APACHE")           %>% generateCustomSummary("APACHE")

ML.resampleResults.df.ALL <- rbind(ML.resampleResults.df.adaboost,
                                   ML.resampleResults.df.avNNet,
                                   ML.resampleResults.df.parRF,
                                   ML.resampleResults.df.svmRadialWeights,
                                   ML.resampleResults.df.glm,
                                   ML.resampleResults.df.DeepNN,
                                   ML.resampleResults.df.APACHE)

# g. plot graphs to show the interim results before imputaiton variability has been assessed
source("resultsGraphsWithoutImpvar.R")


#### 5. Test for variablilty due to imputation by running tuned models for each imputation ####
# a. Rebuild the optimum model on all 9 imputations
ML.methods <- c("adaboost", "APACHE", "avNNet", "DeepNN", "glm", "parRF", "svmRadialWeights")
imputationVariationModels(MLmethods = ML.methods) # uses the optimal tuning parameters for each day to build models for each of 9 imputations (for each method & day)

# b. Aggregate, process and plot outputs of imputation variatiability assessment
source("imputationVariationPlottingFunctions.R") # functions to combine, process and plot models built across imputations
imputation.results.APACHE           <- getImputations(MLmethod = "APACHE", imputationPath = path.imputation, output.type = "results")
imputation.results.glm              <- getImputations(MLmethod = "glm", imputationPath = path.imputation, output.type = "results")
imputation.results.DeepNN           <- getImputations(MLmethod = "DeepNN", imputationPath = path.imputation, output.type = "results")
imputation.results.svmRadialWeights <- getImputations(MLmethod = "svmRadialWeights", imputationPath = path.imputation, output.type = "results", invert = TRUE)
imputation.results.avNNet           <- getImputations(MLmethod = "avNNet", imputationPath = path.imputation, output.type = "results")
imputation.results.parRF            <- getImputations(MLmethod = "parRF", imputationPath = path.imputation, output.type = "results")
imputation.results.adaboost         <- getImputations(MLmethod = "adaboost", imputationPath = path.imputation, output.type = "results")

imputation.resampleResults.glm              <- getImputations(MLmethod = "APACHE", imputationPath = path.imputation, output.type = "pred") %>% 
                                                generateFoldedImputationSummaries()
imputation.resampleResults.APACHE           <- getImputations(MLmethod = "glm", imputationPath = path.imputation, output.type = "pred") %>% 
                                                generateFoldedImputationSummaries()
imputation.resampleResults.DeepNN           <- getImputations(MLmethod = "DeepNN", imputationPath = path.imputation, output.type = "pred") %>% 
                                                generateFoldedImputationSummaries()
imputation.resampleResults.svmRadialWeights <- getImputations(MLmethod = "svmRadialWeights", imputationPath = path.imputation, output.type = "pred") %>% 
                                                generateFoldedImputationSummaries(invert = TRUE)
imputation.resampleResults.avNNet           <- getImputations(MLmethod = "avNNet", imputationPath = path.imputation, output.type = "pred") %>% 
                                                generateFoldedImputationSummaries()
imputation.resampleResults.parRF            <- getImputations(MLmethod = "parRF", imputationPath = path.imputation, output.type = "pred") %>% 
                                                generateFoldedImputationSummaries()
imputation.resampleResults.adaboost         <- getImputations(MLmethod = "adaboost", imputationPath = path.imputation, output.type = "pred") %>% 
                                                generateFoldedImputationSummaries()

# c. Plot the variability for each machine learning method
for (MLmethod in ML.methods) {
  plotImputationResults(imputation.results.glm, MLmethod)
}

# d. Aggregate to give combined dataframes for the overall results
imputation.results.ALL <- rbind(imputation.results.glm[,c(1:3,(length(imputation.results.glm)-5):length(imputation.results.glm))],
                                imputation.results.DeepNN[,c(1:3,(length(imputation.results.DeepNN)-5):length(imputation.results.DeepNN))],
                                imputation.results.APACHE[,c(1:3,(length(imputation.results.APACHE)-5):length(imputation.results.APACHE))],
                                imputation.results.avNNet[,c(1:3,(length(imputation.results.avNNet)-5):length(imputation.results.avNNet))],
                                imputation.results.svmRadialWeights[,c(1:3,(length(imputation.results.svmRadialWeights)-5):length(imputation.results.svmRadialWeights))],
                                imputation.results.parRF[,c(1:3,(length(imputation.results.parRF)-5):length(imputation.results.parRF))])

imputation.resampleResults.ALL <- rbind(imputation.resampleResults.glm,
                                        imputation.resampleResults.DeepNN,
                                        imputation.resampleResults.APACHE,
                                        imputation.resampleResults.svmRadialWeights,
                                        #imputation.resampleResults.adaboost,
                                        #imputation.resampleResults.parRF,
                                        imputation.resampleResults.avNNet)

### Pool the imputations and resamples to give one overall estimate of variance
imputation.results.ALL.pooled <- imputation.results.ALL %>% group_by(MLmethod, day, cumulative) %>% summarise(ROCmean = mean(ROC), 
                                                                                                              ROCImpVar = sd(ROC)^2,
                                                                                                              ROCResampleVar = mean(ROCSD^2),
                                                                                                              ROCPooledVar = ROCImpVar + ROCResampleVar,
                                                                                                              ROCPooledSD = sqrt(ROCPooledVar),
                                                                                                              ROCul = ROCmean + 1.96*ROCPooledSD,
                                                                                                              ROCll = ROCmean - 1.96*ROCPooledSD)

plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "parRF", "avNNet", "svmRadialWeights", "DeepNN"), plotMetric = "AUROC", plotName = "AUROC", legendPosition = "topright", title = FALSE, errorBarScale = 0.35)
plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("APACHE"), plotMetric = "AUROC", plotName = "AUROC", legendPosition = "topright", title = FALSE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Accuracy.maxAccuracy", plotName = "maxAcc", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Accuracy.balanced", plotName = "balAcc", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "PosPredValue.balanced", plotName = "PPVbal", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "NegPredValue.balanced", plotName = "NPVbal", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Sensitivity.balanced", plotName = "sensitivity", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Specificity.balanced", plotName = "specificity", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Kappa.maxAccuracy", plotName = "kappa.max", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Kappa.balanced", plotName = "kappa.balanced", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
