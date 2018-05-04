#### Master Code: Test tuned models on all imputations to determine imputation variability and folding variability  ####

source("functions/MLtuning/tuneDeepLearningModel.R")
source("functions/finalModel/imputationVariationModels.R")
source("functions/finalModel/getImputationResults.R")
source("functions/finalModel/generateFoldedImputationSummaries.R")
source("functions/graph_plotting/plotImputationResults.R")
source("functions/graph_plotting/plotMLOutputGraph_vImp.R")


path.imputation <- "~/RDSFiles/imputationVariation"
imputedData.all <- readRDS("imputedData.all.RDS")

registerDoMC(detectCores() - 6) # set up parallel workers

#### 1. Build one 20-fold crossvalidated model per imputation per day ####
ML.methods <- c("adaboost", "APACHE", "avNNet", "DeepNN", "glm", "parRF", "svmRadialWeights")
day.cumul = data.frame(day = c(1,2,2,3,3,4,4,5,5), cumul = c(F,F,T,F,T,F,T,F,T))
imputationVariationModels(MLmethods = ML.methods, path.impVar = path.imputation, dayList = day.cumul) # uses the optimal tuning parameters for each day to build models for each of 9 imputations (for each method & day)

#### 2. Aggregate, process and plot outputs of imputation variatiability assessment ####
# load the imputation results into a dataframe for each MLmethod
for (MLmethod in c("adaboost", "APACHE", "avNNet", "DeepNN", "glm", "parRF", "svmRadialWeights")) {
  assign(paste("imputation.results.", MLmethod, sep = ""), 
         getImputationResults(MLmethod = MLmethod, 
                              imputationPath = path.imputation, 
                              output.type = "results",
                              invert = ifelse(MLmethod == "svmRadialWeights", TRUE, FALSE)))
}

# plot the variability for each machine learning method
for (MLmethod in c("adaboost", "APACHE", "avNNet", "DeepNN", "glm", "parRF", "svmRadialWeights")) {
  plotImputationResults(get(paste("imputation.results.", MLmethod, sep = "")), MLmethod)
}

# generate resampling summary statistics for each ML method (time consuming, not parallel, kept separate to limit memory use)
imputation.resampleResults.APACHE           <- getImputationResults(MLmethod = "APACHE", imputationPath = path.imputation, output.type = "pred") %>% 
                                                  generateFoldedImputationSummaries()
imputation.resampleResults.glm              <- getImputationResults(MLmethod = "glm", imputationPath = path.imputation, output.type = "pred") %>% 
                                                  generateFoldedImputationSummaries()
imputation.resampleResults.DeepNN           <- getImputationResults(MLmethod = "DeepNN", imputationPath = path.imputation, output.type = "pred") %>% 
                                                  generateFoldedImputationSummaries()
imputation.resampleResults.svmRadialWeights <- getImputationResults(MLmethod = "svmRadialWeights", imputationPath = path.imputation, output.type = "pred") %>% 
                                                  generateFoldedImputationSummaries(invert = TRUE)
imputation.resampleResults.avNNet           <- getImputationResults(MLmethod = "avNNet", imputationPath = path.imputation, output.type = "pred") %>% 
                                                  generateFoldedImputationSummaries()
imputation.resampleResults.parRF            <- getImputationResults(MLmethod = "parRF", imputationPath = path.imputation, output.type = "pred") %>% 
                                                  generateFoldedImputationSummaries()
imputation.resampleResults.adaboost         <- getImputationResults(MLmethod = "adaboost", imputationPath = path.imputation, output.type = "pred") %>% 
                                                  generateFoldedImputationSummaries()


# d. Aggregate to give combined dataframes for the overall results
imputation.results.ALL <- rbind(imputation.results.glm[,c(1:3,(length(imputation.results.glm)-5):length(imputation.results.glm))],
                                imputation.results.DeepNN[,c(1:3,(length(imputation.results.DeepNN)-5):length(imputation.results.DeepNN))],
                                imputation.results.APACHE[,c(1:3,(length(imputation.results.APACHE)-5):length(imputation.results.APACHE))],
                                imputation.results.avNNet[,c(1:3,(length(imputation.results.avNNet)-5):length(imputation.results.avNNet))],
                                imputation.results.svmRadialWeights[,c(1:3,(length(imputation.results.svmRadialWeights)-5):length(imputation.results.svmRadialWeights))],
                                imputation.results.parRF[,c(1:3,(length(imputation.results.parRF)-5):length(imputation.results.parRF))],
                                imputation.results.adaboost[,c(1:3,(length(imputation.results.adaboost)-5):length(imputation.results.adaboost))])

imputation.resampleResults.ALL <- rbind(imputation.resampleResults.glm,
                                        imputation.resampleResults.DeepNN,
                                        imputation.resampleResults.APACHE,
                                        #imputation.resampleResults.svmRadialWeights,
                                        imputation.resampleResults.adaboost,
                                        imputation.resampleResults.parRF,
                                        imputation.resampleResults.avNNet)

### Pool the imputations and resamples to give one overall estimate of variance
imputation.results.ALL.pooled <- imputation.results.ALL %>% group_by(MLmethod, day, cumulative) %>% summarise(ROCmean = mean(ROC), 
                                                                                                              ROCImpVar = sd(ROC)^2,
                                                                                                              ROCResampleVar = mean(ROCSD^2),
                                                                                                              ROCPooledVar = ROCImpVar + ROCResampleVar,
                                                                                                              ROCPooledSD = sqrt(ROCPooledVar),
                                                                                                              ROCul = ROCmean + 2*ROCPooledSD,
                                                                                                              ROCll = ROCmean - 2*ROCPooledSD)

plotMLOutputGraph_vImp(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "parRF", "avNNet", "adaboost", "DeepNN"), plotMetric = "AUROC", plotName = "AUROC_allML", legendPosition = "topright", title = FALSE, errorBarScale = 0.4, yLabel = "AUC", height = 3500, vLines = TRUE, barColour = "black", dotSize = 0.65, lineThickness = 1.2)
plotMLOutputGraph_vImp(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "parRF", "avNNet", "adaboost", "DeepNN"), plotMetric = "AUROC", plotName = "AUROC_allML_APACHE", legendPosition = "topright", title = FALSE, errorBarScale = 0.35, addAPACHEday1 = TRUE, yLabel = "AUC", height = 3500, vLines = TRUE, barColour = "black", dotSize = 0.65, lineThickness = 1.2)
plotMLOutputGraph_vImp(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("APACHE"), plotMetric = "AUROC", plotName = "APACHE", legendPosition = "topright", title = FALSE, errorBarScale = 0.1, yLabel = "AUC", height = 2000, legend = FALSE, width = 3000)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Accuracy.maxAccuracy", plotName = "maxAcc", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Accuracy.balanced", plotName = "balAcc", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "PosPredValue.balanced", plotName = "PPVbal", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "NegPredValue.balanced", plotName = "NPVbal", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Sensitivity.balanced", plotName = "sensitivity", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Specificity.balanced", plotName = "specificity", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Kappa.maxAccuracy", plotName = "kappa.max", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)
# plotGrandMLOutputGraph_vImp_vResample(imputationResampleResultsALL = imputation.resampleResults.ALL, MLmethods = c("glm", "avNNet", "DeepNN", "svmRadialWeights"), plotMetric = "Kappa.balanced", plotName = "kappa.balanced", legendPosition = "topright", title = TRUE, errorBarScale = 0.35)

for(day in 1:5) {
  for(imput in 1:9) {
    filePath <- paste(path.imputation, "/pred/DeepNN/ROC.tuningML.DeepNN.day",day,"cumulative.iterIMPVARIMPVAR",imput,".pred.RDS", sep ="")
    if(file.exists(filePath)) {
      x <- readRDS(filePath)
      saveRDS(x, paste(path.imputation, "/pred/DeepNN/ROC.tuningML.DeepNN.day",day,"cumulative.iterIMPVAR",imput,".pred.RDS", sep =""))
    }  
  }
}
