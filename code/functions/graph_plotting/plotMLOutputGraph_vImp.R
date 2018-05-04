# flexible function to plot the output graphs taking into account the variability of the imputations in the error bars and plotting the mean for each fold across all imputations

plotMLOutputGraph_vImp <- function(imputationResampleResultsALL, 
                                   MLmethods, 
                                   plotMetric = "AUROC", 
                                   plotName, 
                                   legendPosition = "topright", 
                                   title = TRUE, 
                                   legend = TRUE,
                                   errorBarScale = 0.3, 
                                   addAPACHEday1 = FALSE,
                                   yLabel = plotMetric, 
                                   height = 2000, 
                                   width = 4000, 
                                   vLines = FALSE,
                                   barColour = "darkgrey",
                                   dotSize = 0.5,
                                   lineThickness = 1) {
  
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
  imputation.resampleResults.ALL.pooled <- imputationResampleResultsALL %>% 
                                              group_by(MLmethod, day, cumulative, Resample) %>% 
                                                summarise(MetricMean = mean(!!sym(plotMetric), na.rm = TRUE),
                                                          ImputVar = sd(!!sym(plotMetric), na.rm = TRUE)^2)
  
  imputation.resampleResults.ALL.pooled.summarised <- imputation.resampleResults.ALL.pooled %>% 
                                                          group_by(MLmethod, day, cumulative) %>% 
                                                              summarise(MetricMeanAvg = mean(MetricMean, na.rm = TRUE), 
                                                                        FoldVar = sd(MetricMean, na.rm = TRUE)^2,
                                                                        ImputVarAvg = mean(ImputVar, na.rm = TRUE),
                                                                        TotalVar = FoldVar + ImputVarAvg,
                                                                        TotalSD = sqrt(TotalVar),
                                                                        TotalSEM = TotalSD / sqrt(n()),
                                                                        UL95 = MetricMeanAvg + 1.96 * TotalSEM,
                                                                        LL95 = MetricMeanAvg - 1.96 * TotalSEM,
                                                                        UL2SD = MetricMeanAvg + 2 * TotalSD,
                                                                        LL2SD = MetricMeanAvg - 2 * TotalSD)

  
  # define colours
  colour.df <- arrange(data.frame(MLmethod = c("adaboost", "avNNet", "parRF", "svmRadialWeights", "glm", "DeepNN", "APACHE"), 
                                  col = c("darkred", "red", "darkgreen", "pink", "blue", "orange", "black"), 
                                  stringsAsFactors = FALSE), MLmethod)
  
  # filter for ML methods
  plot.df <- filter(imputation.resampleResults.ALL.pooled, MLmethod %in% MLmethods)
  plot.limits.df <- filter(imputation.resampleResults.ALL.pooled.summarised, MLmethod %in% MLmethods)
  
  # Generate labels
  labs.1 <- c("", "", "simple", "cumul.", "simple", "cumul.", "simple", "cumul.", "simple", "cumul.")
  labs.2 <- c("  \nDay 1", "  \nDay 2", "  \nDay 3", "  \nDay 4", "  \nDay 5")
  labs.apache <- c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5")

  # determine how wide each day plot should be based on how many ML methods are used plus a visual buffer
  no.x.plots <- length(MLmethods) + 2
  
  # determine the "base" points from which to reference the plots, with different method if only APACHE is to be plotted
  if(paste(MLmethods, collapse = "") == "APACHE") {
    loc.base <- c(1, 100, 2, 100, 3, 100, 4, 100, 5, 100)
    x.limits <- c(0.5,5.5)
  } else {
    loc.base <- c(1, 1:9 * no.x.plots - (no.x.plots - 1)) #determine start of each day, including space for cumulative plots
    x.limits <- c(0.5, max(loc.base) - 1.5 + no.x.plots) # set x axis limits
  }
  
  # set limits on the y axis
  y.limits <- c(min(c(min(plot.limits.df$LL2SD), 
                      min(plot.df$MetricMean))), 
                max(c(max(plot.limits.df$UL2SD), 
                      max(plot.df$MetricMean))))
  
  # plot day 1 cumulative outside the plot range as it never exists
  loc.base[2] <- 1000 
  
  if (addAPACHEday1 == TRUE) loc.base <- loc.base + 1
  
  # set up grph and plot by ML method as segments
  plotMiscGraph("PooledImputationsMLoutput", plotName, width, height)
    
  j <- 1
  for(i in c(1:length(MLmethods),1)) {
    addBool <- ifelse(j == 1, FALSE, TRUE) # is this the first graph?
    
    plot.df.filtered <- filter(plot.df, MLmethod == MLmethods[i])
    plot.limits.df.filtered <- filter(plot.limits.df, MLmethod == MLmethods[i])
    
    loc.strip <- loc.base + i - 1
    
    if (addBool == TRUE) {
      loc.bars <- loc.strip[c(1,3:10)]
      
      segments(x0 = c(loc.bars - errorBarScale, loc.bars - errorBarScale, loc.bars - errorBarScale, loc.bars),
               x1 = c(loc.bars + errorBarScale, loc.bars + errorBarScale, loc.bars + errorBarScale, loc.bars),
               y0 = c(plot.limits.df.filtered$UL2SD, plot.limits.df.filtered$LL2SD, plot.limits.df.filtered$MetricMeanAvg, plot.limits.df.filtered$UL2SD),
               y1 = c(plot.limits.df.filtered$UL2SD, plot.limits.df.filtered$LL2SD, plot.limits.df.filtered$MetricMeanAvg, plot.limits.df.filtered$LL2SD),
               lwd = lineThickness * rep(c(1,1,2,0.8), each = length(loc.bars)), 
               #col = colour.df$col[colour.df$MLmethod == MLmethods[i]],
               col = barColour)
    }
    
    stripchart(MetricMean ~ cumulative + day,
               plot.df.filtered, 
               vertical = TRUE, 
               at = loc.strip, 
               xlim = x.limits, 
               ylim = y.limits,
               pch = 16,
               cex = dotSize,
               method = "jitter",
               las = 2,
               col = ifelse(j == 1, "white", colour.df$col[colour.df$MLmethod == MLmethods[i]]),
               xaxt = "n",
               ylab = yLabel,
               main = ifelse(title == TRUE, paste("Comparison of ML methods tuned to AUROC: ", plotName, ".\nMean +/- 1.96 SD", sep = ""), NA),
               add = addBool)
    
    j <- j + 1
  }
  
  if (addAPACHEday1 == TRUE) {
    loc.apache <- 1
    
    apacheDay1.df <- filter(imputation.resampleResults.ALL.pooled, MLmethod == "APACHE", day == 1)
    apacheDay1.limits.df <- filter(imputation.resampleResults.ALL.pooled.summarised, MLmethod == "APACHE", day == 1)
    
    segments(x0 = c(loc.apache - errorBarScale, loc.apache - errorBarScale, loc.apache - errorBarScale, loc.apache),
             x1 = c(loc.apache + errorBarScale, loc.apache + errorBarScale, loc.apache + errorBarScale, loc.apache),
             y0 = c(apacheDay1.limits.df$UL2SD, apacheDay1.limits.df$LL2SD, apacheDay1.limits.df$MetricMeanAvg, apacheDay1.limits.df$UL2SD),
             y1 = c(apacheDay1.limits.df$UL2SD, apacheDay1.limits.df$LL2SD, apacheDay1.limits.df$MetricMeanAvg, apacheDay1.limits.df$LL2SD),
             lwd = lineThickness * rep(c(1,1,2,0.8), each = length(loc.apache)), 
             #col = colour.df$col[colour.df$MLmethod == MLmethods[i]],
             col = barColour)
  
    stripchart(MetricMean ~ cumulative + day,
               apacheDay1.df, 
               vertical = TRUE, 
               at = loc.apache, 
               pch = 16,
               cex = dotSize,
               method = "jitter",
               las = 2,
               col = colour.df$col[colour.df$MLmethod == "APACHE"],
               xaxt = "n",
               main = ifelse(title == TRUE, paste("Comparison of ML methods tuned to AUROC: ", plotName, ".\nMean +/- 1.96 SD", sep = ""), NA),
               add = TRUE)
  }
  
  lab.pos.1 <- loc.base + ((no.x.plots - 3) / 2)
  lab.pos.1[1] <- ifelse(addAPACHEday1 == TRUE, lab.pos.1[1] - 0.5, lab.pos.1[1])
  lab.pos.2 <- c(lab.pos.1[1], 
                 mean(c(lab.pos.1[3],lab.pos.1[4])), 
                 mean(c(lab.pos.1[5],lab.pos.1[6])), 
                 mean(c(lab.pos.1[7],lab.pos.1[8])),
                 mean(c(lab.pos.1[9],lab.pos.1[10])))
  
  if(paste(MLmethods, collapse = "") == "APACHE") {
    axis(side = 1, at = loc.base[c(1,3,5,7,9)], labels = labs.apache, tick = FALSE)
  } else {
    axis(side = 1, at = lab.pos.1, labels = labs.1, tick = FALSE)
    axis(side = 1, at = lab.pos.2, labels = labs.2, tick = FALSE, line = NA, padj = 1, font = 2)
  }
  
  if(vLines == TRUE) {
    line.pos.main <- c(mean(c(lab.pos.2[1],lab.pos.2[2])) - 2, 
                       mean(c(lab.pos.2[2],lab.pos.2[3])), 
                       mean(c(lab.pos.2[3],lab.pos.2[4])),
                       mean(c(lab.pos.2[4],lab.pos.2[5])))
    
    segments(y0 = 0, y1 = 1,
             x0 = line.pos.main, x1 = line.pos.main, lty = 3, col = "grey")
  }
  
  # choose what to put in the legend
  if(addAPACHEday1== TRUE) {
    MLmethods.legend <- c("APACHE",MLmethods)
  } else {
    MLmethods.legend <- MLmethods 
  }
  
  if (legend == TRUE) legend(legendPosition, inset = 0.02, bty = "n", pch = 16, legend = sort(MLmethods.legend), col = (colour.df$col[colour.df$MLmethod %in% MLmethods.legend])) # make the legend
  
  dev.off()
}
