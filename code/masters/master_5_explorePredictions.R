#### Master Code: Explores differences between correct and incorrect predictions for the models in age and length of admission ####

source("functions/finalModel/getImputationResults.R")
source("functions/imputation/completeCustom.R")
source("functions/predictionExploration/addVariableToExplorePredsDF.R")
source("functions/graph_plotting/plotPredictionsBeanplotPanel.R")
source("functions/graph_plotting/plotDescriptiveGraphs.R")

library(dplyr)

# set path of final model predictions
path.imputation <- "~/RDSFiles/imputationVariation"

# get all predictiions, generate summaries and aggregate into one large data frame
imputation.explorePreds.all <- data.frame()
for(MLmethod in c("adaboost", "APACHE", "avNNet", "DeepNN", "glm", "parRF", "svmRadialWeights")) {
   assign(paste("imputation.allPreds.", MLmethod, sep = ""), 
           getImputationResults(MLmethod = MLmethod, 
                                imputationPath = path.imputation, 
                                output.type = "pred"))
  
  temp.df <- transmute(get(paste("imputation.allPreds.", MLmethod, sep = "")),
                       MLmethod = MLmethod,
                       rowIndex = rowIndex, 
                       imputation = imputation,
                       day = day,
                       cumulative = cumulative,
                       obs = obs, 
                       pred = pred, 
                       correct = ifelse(pred == obs, TRUE, FALSE))

  imputation.explorePreds.all <- rbind(imputation.explorePreds.all, temp.df)
}
saveRDS(imputation.explorePreds.all, "imputation.explorePreds.all.RDS")

# retrieve the predictions for the deep learning models and look up the values of all corresponding variables 
imputation.explorePreds.DeepNN <- filter(imputation.explorePreds.all, 
                                         MLmethod == "DeepNN")

predictionOutputs.df.DeepNN <- addVariablesToExplorePredsDF(imputation.explorePreds.DeepNN)

# plot the beanplots to compare distributions
plotPredictionsBeanplotPanel(variable = "age",
                             inputDF = predictionOutputs.df.DeepNN, 
                             density = 2,
                             MLmethod = "Deep Learning", 
                             title = FALSE, footer = FALSE)

plotPredictionsBeanplotPanel(variable = "icu_duration_days", 
                             inputDF = predictionOutputs.df.DeepNN, 
                             density = 2, 
                             MLmethod = "Deep Learning", 
                             ylimits = c(-10,60), 
                             title = FALSE, 
                             footer = FALSE)

# generate tabular version
deepLearningTable <- predictionOutputs.df.DeepNN %>% 
  group_by(day, cumulative, obs, correct) %>% 
  summarise(meanAge = round(mean(age),2), 
            sdAge = round(sd(age),2), 
            meanLOS = round(mean(icu_duration_days),2), 
            sdLOS = round(sd(icu_duration_days),2))

deepLearningTable
