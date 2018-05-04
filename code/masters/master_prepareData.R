#### Master Code : Loads dataset, cleans outliers and imputes missing data  ####

set.seed(42)

# load functions
source("functions/graph_plotting/plotDescriptiveGraphs.R") # functions for plotting descriptive graphs
source("functions/graph_plotting/plotMiscGraph.R") # function to plot generic graph to png file
source("functions/outliers/removeOutliers.R") # functions for removal of outliers
source("functions/outliers/transformVentilationData.R") # function for removal of outliers
source("functions/imputation/imputeCustom.R") # function for imputation of missing data
source("functions/imputation/completeCustom.R") # function for imputation of missing data
source("functions/imputation/manualDensityPlots.R") # function for imputation of missing data

#### 1. Prepare dataSet ####
source("functions/loadData.R") # loads dataset into a formatted dataframe

# cut patients who have no classifier
dataSet.all <- filter(dataSet, !is.na(alive))

# factorise the classifiers
dataSet.all$alive_dead_icu = as.factor(dataSet.all$alive_dead_icu)
levels(dataSet.all$alive_dead_icu) = c("Alive","Dead","E")
levels(dataSet.all$alive) <- c("dead","alive")

# plot raw graphs
plotDescriptiveGraphs(dataSet.all, directory = "Graphs-Raw", 4000, 2000)

#### 2. Remove outliers ####
# apply transformation to ventilation data
tempDataSet.all <- transformVentilationData(dataSet.all)

# remove outliers, treating each variable, max/min, for each day as a sepearte distribution
cleanData.all <- removeOutliers(tempDataSet.all, "singleDaySeparate")

# plot cleaned graphs
plotDescriptiveGraphs(cleanData.all, directory = "Graphs-Cleaned", 4000, 2000)
plotDescriptiveGraphs(cleanData.all, directory = "Graphs-Cleaned-noTitles", 4000, 2500, titled = FALSE, legend = FALSE, ptSize = 20)


#### 3. Impute missing values ####
imputedData.all <- impute.custom(cleanData.all) ########### TODO: need to test imputation
saveRDS(imputedData.all, "imputedData.all.RDS")

# plot graphs comparing imputations and original distribution
manualDensityPlots(mids = imputedData.all, cleanData = cleanData.all)
manualDensityPlots(mids = imputedData.all, cleanData = cleanData.all, title = FALSE, minititles = FALSE, plotDir = "densityPLots_manual_vFig")

# save an imputed dataset as an RDS file
fullData.imput1 <- completeCustom(imputedData.all, 1)
saveRDS(fullData.imput1, "fullData.imput1.RDS")