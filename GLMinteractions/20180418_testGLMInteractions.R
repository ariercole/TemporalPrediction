### Code to discover and test interactions for logistic regression

library(caret)

### Set seeds
set.seed(42)
seed.list <- list()
for (i in 1:20) {
  seed.list[[i]] <- sample.int(n = 10000, size = 200)
}
seed.list[[21]] <- sample.int(n = 10000, size = 1)
seed.list

train.control <- trainControl(method = "LGOCV", 
                              number = 20, 
                              summaryFunction = twoClassSummary, 
                              classProbs = TRUE, 
                              verboseIter = TRUE,
                              seeds = seed.list,  
                              savePredictions = "all",
                              returnResamp = "all",
                              p = 0.8)

dataset <- complete.custom(imputedData.all, 1)

classifier.allInt <- train(formula.allInt.day1, 
                           dataset, 
                           method = "glm",
                           metric = "ROC", 
                           trControl = train.control, 
                           preProcess = c("YeoJohnson", "center", "scale"))

summary(classifier.allInt)

classifier.selectInt <- train(formula.selectInt.day1, 
                           dataset, 
                           method = "glm",
                           metric = "ROC", 
                           trControl = train.control, 
                           preProcess = c("YeoJohnson", "center", "scale"))

summary(classifier.selectInt)


rm(dataset, classifier.allInt, classifier.selectInt, train.control)