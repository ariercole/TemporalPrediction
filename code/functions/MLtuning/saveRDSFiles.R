# function to save the model, results and predictions for the resamples for caret and deep leanring models

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
