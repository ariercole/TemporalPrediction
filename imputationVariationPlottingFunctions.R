library(tcltk)
library(dplyr)
library(rlang)

path.imputation <- "~/RDSfiles/imputationVariation"

getImputations <- function(MLmethod, imputationPath, output.type = "results", invert = FALSE) {
  # generate list of possible classifiers
  combinations <- expand.grid(day = 1:5,
                              iter = 1:9,
                              cumulative = c("cumulative", ""))
  
  # output model df is currently empty
  out.model.df.length <- 0
  out.model.df <- data.frame()
  
  # set up progress bar
  pb <- tkProgressBar(title = MLmethod, label = "Reading RDS files...",
                      min = 0, max = nrow(combinations), initial = 0, width = 300)
  
  # try each classifier and if it exists, add it to a list
  for (i in 1:nrow(combinations)) {
    # select classifier to try from combinations
    combTry <- combinations[i,]
    
    #get the right path
    path <- imputationPath
    
    # generate path for this model
    model.path <- paste(path, "/",output.type,"/", MLmethod, "/ROC.tuningML.", MLmethod,".day", combTry$day, combTry$cumulative, ".iterIMPVAR", combTry$iter, ".", output.type, ".RDS", sep = "")
    
    # try to load the results (smallest of the three options --> fastest)
    if(file.exists(model.path)) {
      model.results <- readRDS(model.path)
      
      out.model.df.row <- data.frame(MLmethod = MLmethod,
                                     day = combTry$day, 
                                     cumulative = ifelse(combTry$cumulative == "cumulative", TRUE, FALSE), 
                                     imputation = combTry$iter,  
                                     model.results)
      
      if (out.model.df.length == 0) {
          out.model.df <- out.model.df.row
          out.model.df.length <- out.model.df.length + 1
        } else {
          out.model.df <- rbind(out.model.df, out.model.df.row)
          out.model.df.length <- out.model.df.length + 1
        }
    }
    
    setTkProgressBar(pb, i)
  }
  close(con = pb)
  
  if (invert == TRUE) out.model.df$ROC <- 1 - out.model.df$ROC
  
  return(out.model.df)
}

generateFoldedImputationSummaries <- function(imputationPredsDF, invert = FALSE, switch = FALSE) {
  # make list of all resamples
  individual.resamples.list <- unique(select(imputationPredsDF, MLmethod, day, cumulative, imputation, Resample))
  
  pb <- tkProgressBar(title = "Progress of resampling", label = "Generating resampling statistics...",
                      min = 0, max = nrow(individual.resamples.list), initial = 0, width = 300)
  
  resample.list <- data.frame()
  for(i in 1:nrow(individual.resamples.list)) {
    IRL.element <- individual.resamples.list[i,]
    preds.df <- filter(imputationPredsDF,
                       day == IRL.element$day,
                       cumulative == IRL.element$cumulative,
                       imputation == IRL.element$imputation,
                       Resample ==  IRL.element$Resample)
    
    resampleResults.row <- cbind(MLmethod = IRL.element$MLmethod,
                                 day = IRL.element$day,
                                 cumulative = IRL.element$cumulative,
                                 imputation = IRL.element$imputation,
                                 Resample =  IRL.element$Resample,
                                 customSummary(data.obs = preds.df$obs,
                                               data.pred = preds.df$pred,
                                               data.alive = preds.df$alive,
                                               invert = invert,
                                               switch = switch,
                                               regularise = TRUE))
    
    resample.list <- rbind(resample.list, resampleResults.row)
    
    setTkProgressBar(pb, i)
    
  }
  close(con = pb)
  
  return(resample.list)
}

plotImputationResults <- function(imputationResultsDF, MLmethod) {
  plotMiscGraph("imputation_variation", paste("ImpVar_", MLmethod, sep = ""), 4000, 2000)
  
  Tcrit <- qt(.975, 20 - 1) # calculate Tcrit for sample size of 20
  
  imputationResultsDF <- imputationResultsDF %>% mutate(dayImp = paste(day, cumulative, imputation, sep = ""),
                                                        ROCSEM = (ROCSD / sqrt(20)),
                                                        ROCUL = ROC + Tcrit * ROCSEM,
                                                        ROCLL = ROC - Tcrit * ROCSEM) %>%
                                                 arrange(dayImp)
  
  loc.strip <- c(1:9,11:19,21:29,31:39,41:49,51:59,61:69,71:79,81:89)
  ylims <- c(min(imputationResultsDF$ROCLL),
             max(imputationResultsDF$ROCUL))
  
  stripchart(ROC ~ dayImp,
             imputationResultsDF,
             vertical = TRUE,
             pch = "-",
             cex = 2,
             at = loc.strip,
             xaxt = "n",
             xlim = c(0,90),
             ylim = ylims,
             main = paste("AUROC of 9 ", MLmethod, " models for each of 9 imputations.\nMean +/- 1.96 SEM", sep = ""))
  
  stripchart(ROCUL ~ dayImp,
             imputationResultsDF,
             vertical = TRUE,
             pch = "-",
             cex = 1,
             at = loc.strip, 
             add = TRUE)
  
  stripchart(ROCLL ~ dayImp,
             imputationResultsDF,
             vertical = TRUE,
             pch = "-",
             cex = 1,
             at = loc.strip, 
             add = TRUE)
  
  segments(x0 = loc.strip, x1 = loc.strip,
           y0 = imputationResultsDF$ROCUL, y1 = imputationResultsDF$ROCLL,
           lwd = 1)
  
  axis(side = 1, at = seq(5,85,10), labels = c("Day 1", "Day 2\nsimple", "Day 2\ncumul.", "Day 3\nsimple", "Day 3\ncumul.", "Day 4\nsimple", "Day 4\ncumul.", "Day 5\nsimple", "Day 5\ncumul."), tick = FALSE)
  dev.off()
}

plotGrandMLOutputGraph_vImp_vResample <- function(imputationResampleResultsALL, MLmethods, plotMetric = "AUROC", plotName, legendPosition = "topright", title = TRUE, errorBarScale = 0.3) {
  # correct methodological differences in PPV/NPV/Sens/Spec
  if ("DeepNN" %in% MLmethods) {
    newTempSet <- imputationResampleResultsALL
    newTempSet[newTempSet$MLmethod == "DeepNN", grepl("PosPredValue", colnames(imputationResampleResultsALL))] <- 1 - imputationResampleResultsALL[imputationResampleResultsALL$MLmethod == "DeepNN", grepl("PosPredValue", colnames(imputationResampleResultsALL))]
    newTempSet[newTempSet$MLmethod == "DeepNN", grepl("NegPredValue", colnames(imputationResampleResultsALL))] <- 1 - imputationResampleResultsALL[imputationResampleResultsALL$MLmethod == "DeepNN", grepl("NegPredValue", colnames(imputationResampleResultsALL))]

    newTempSet[newTempSet$MLmethod == "DeepNN", grepl("Sensitivity", colnames(imputationResampleResultsALL))] <- 1 - imputationResampleResultsALL[imputationResampleResultsALL$MLmethod == "DeepNN", grepl("Sensitivity", colnames(imputationResampleResultsALL))]
    newTempSet[newTempSet$MLmethod == "DeepNN", grepl("Specificity", colnames(imputationResampleResultsALL))] <- 1 - imputationResampleResultsALL[imputationResampleResultsALL$MLmethod == "DeepNN", grepl("Specificity", colnames(imputationResampleResultsALL))]
  
    newTempSet[newTempSet$MLmethod == "DeepNN", grepl("Accuracy.balanced", colnames(imputationResampleResultsALL))] <- 1 - imputationResampleResultsALL[imputationResampleResultsALL$MLmethod == "DeepNN", grepl("Accuracy.balanced", colnames(imputationResampleResultsALL))]

    imputationResampleResultsALL <- newTempSet
  }
  
  # prepare data
  imputation.resampleResults.ALL.pooled <- imputationResampleResultsALL %>% group_by(MLmethod, day, cumulative, Resample) %>% summarise(MetricMean = mean(!!sym(plotMetric)),
                                                                                                                                        ImputVar = sd(!!sym(plotMetric))^2)
  
  imputation.resampleResults.ALL.pooled.summarised <-imputation.resampleResults.ALL.pooled %>% group_by(MLmethod, day, cumulative) %>% summarise(MetricMeanAvg = mean(MetricMean), 
                                                                                                                                            FoldVar = sd(MetricMean)^2,
                                                                                                                                            ImputVarAvg = mean(ImputVar),
                                                                                                                                            TotalVar = FoldVar + ImputVarAvg,
                                                                                                                                            TotalSD = sqrt(TotalVar),
                                                                                                                                            TotalSEM = TotalSD / sqrt(n()),
                                                                                                                                            UL95 = MetricMeanAvg + 1.96 * TotalSEM,
                                                                                                                                            LL95 = MetricMeanAvg - 1.96 * TotalSEM,
                                                                                                                                            UL2SD = MetricMeanAvg + 1.96 * TotalSD,
                                                                                                                                            LL2SD = MetricMeanAvg - 1.96 * TotalSD)
                                                                                                                                          
  
  # define colours
  colour.df <- arrange(data.frame(MLmethod = c("adaboost", "avNNet", "parRF", "svmRadialWeights", "glm", "DeepNN", "APACHE"), 
                          col = c("darkred", "darkblue", "green", "pink", "blue", "orange", "grey"), 
                          stringsAsFactors = FALSE), MLmethod)
  
  # filter for ML methods
  plot.df <- filter(imputation.resampleResults.ALL.pooled, MLmethod %in% MLmethods)
  plot.limits.df <- filter(imputation.resampleResults.ALL.pooled.summarised, MLmethod %in% MLmethods)
  
  # Generate labels
  labs.all <- c("Day 1", "Day 1\nCumul.", "Day 2", "Day 2\nCumul.", "Day 3", "Day 3\nCumul.", "Day 4", "Day 4\nCumul.", "Day 5", "Day 5\nCumul.")
  labs <- labs.all[1:(day.range * 2)]
  
  # determine how wide each day plot should be based on how many ML methods are used plus a visual buffer
  no.x.plots <- length(MLmethods) + 2
  
  # determine the "base" points from which to reference the plots
  loc.base <- c(1, 1:9 * no.x.plots - (no.x.plots - 1)) #determine start of each day, including space for cumulative plots
  
  # set limits on the axes
  x.limits <- c(0.5, max(loc.base) - 1.5 + no.x.plots) # set x axis limits
  y.limits <- c(min(plot.df$MetricMean), max(plot.df$MetricMean))
  
  # plot day 1 cumulative outside the plot range as it never exists
  loc.base[2] <- 1000 
  
  
  # set up grph and plot by ML method as segments
  plotMiscGraph("PooledImputationsMLoutput", plotName, 4000, 2000)

  
  for(i in 1:length(MLmethods)) {
    addBool <- ifelse(i == 1, FALSE, TRUE) # is this the first graph?
    
    plot.df.filtered <- filter(plot.df, MLmethod == MLmethods[i])
    plot.limits.df.filtered <- filter(plot.limits.df, MLmethod == MLmethods[i])
    
    loc.strip <- loc.base + i - 1
    
    stripchart(MetricMean ~ cumulative + day,
               plot.df.filtered, 
               vertical = TRUE, 
               at = loc.strip, 
               xlim = x.limits, 
               ylim = y.limits,
               pch = 16,
               cex = 0.5,
               method = "jitter",
               las = 2,
               col = colour.df$col[colour.df$MLmethod == MLmethods[i]],
               xaxt = "n",
               ylab = plotName,
               main = ifelse(title == TRUE, paste("Comparison of ML methods tuned to AUROC: ", plotName, ".\nMean +/- 1.96 SD", sep = ""), NA),
               add = addBool)
    
    loc.bars <- loc.strip[c(1,3:10)]
    
    segments(x0 = c(loc.bars - errorBarScale, loc.bars - errorBarScale, loc.bars - errorBarScale, loc.bars),
             x1 = c(loc.bars + errorBarScale, loc.bars + errorBarScale, loc.bars + errorBarScale, loc.bars),
             y0 = c(plot.limits.df.filtered$UL2SD, plot.limits.df.filtered$LL2SD, plot.limits.df.filtered$MetricMeanAvg, plot.limits.df.filtered$UL2SD),
             y1 = c(plot.limits.df.filtered$UL2SD, plot.limits.df.filtered$LL2SD, plot.limits.df.filtered$MetricMeanAvg, plot.limits.df.filtered$LL2SD),
             lwd = rep(c(1,1,2,0.8), each = length(loc.bars)), 
             col = colour.df$col[colour.df$MLmethod == MLmethods[i]])
  }
  
  axis(side = 1, at = loc.base + ((no.x.plots - 3) / 2), labels = labs, tick = FALSE)
  legend(legendPosition, inset = 0.02, bty = "n", pch = 16, legend = sort(MLmethods), col = (colour.df$col[colour.df$MLmethod %in% MLmethods]))
  
  dev.off()
}