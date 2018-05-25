# plots a graph of the results of the nine imputations for each day

library(dplyr)

source("functions/graph_plotting/plotMiscGraph.R")

plotImputationResults <- function(imputationResultsDF, MLmethod, title = FALSE) {
  plotMiscGraph("imputation_variation", paste("ImpVar_", MLmethod, sep = ""), 4000, 2000)
  
  imputationResultsDF <- imputationResultsDF %>% mutate(dayImp = paste(day, cumulative, imputation, sep = ""),
                                                        ROCSEM = (ROCSD / sqrt(20)),
                                                        ROCUL = ROC + 2 * ROCSD,
                                                        ROCLL = ROC - 2 * ROCSD) %>%
                                                 arrange(dayImp)
  
  loc.strip <- c(1:9,11:19,21:29,31:39,41:49,51:59,61:69,71:79,81:89)
  ylims <- c(min(imputationResultsDF$ROCLL),
             max(imputationResultsDF$ROCUL))
  
  stripchart(ROC ~ dayImp,
             imputationResultsDF,
             vertical = TRUE,
             pch = "-",
             cex = 2,
             las = 2,
             ylab = "AUC",
             at = loc.strip,
             xaxt = "n",
             xlim = c(0,90),
             ylim = ylims,
             main = ifelse(title == TRUE, paste("AUC of 20 folds of 9 ", MLmethod, " models for each of 9 imputations.\nMean +/- 2SD", sep = ""), NA))
  
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
  
  # add x-axis
  labs.1 <- c("", "simple", "cumul.", "simple", "cumul.", "simple", "cumul.", "simple", "cumul.")
  labs.2 <- c("  \nDay 1", "  \nDay 2", "  \nDay 3", "  \nDay 4", "  \nDay 5")
  
  lab.pos.1 <- seq(5,85,10)
  lab.pos.2 <- c(5, seq(20,80,20))
  
  axis(side = 1, at = lab.pos.1, labels = labs.1, tick = FALSE)
  axis(side = 1, at = lab.pos.2, labels = labs.2, tick = FALSE, line = NA, padj = 1, font = 2)
  
  # add vertical lines
  line.pos.main <- c(seq(10,70,20))
  
  segments(y0 = 0, y1 = 1,
           x0 = line.pos.main, x1 = line.pos.main, lty = 3, col = "grey")
  
  
  dev.off()
}

plotImputationResults_dayCompare <- function(imputationResultsDF.1, imputationResultsDF.2, MLmethod, group1name, group2name, title = FALSE, day) {
  plotMiscGraph("imputation_variation", paste("ImpVar_", MLmethod, sep = ""), 4000, 2000)
  
  imputationResultsDF <- rbind(mutate(imputationResultsDF.1, group = 1), 
                               mutate(imputationResultsDF.2, group = 2))
  
  imputationResultsDF <- imputationResultsDF %>% mutate(dayImp = paste(day, cumulative, imputation, sep = ""),
                                                        ROCSEM = (ROCSD / sqrt(20)),
                                                        ROCUL = ROC + 2 * ROCSD,
                                                        ROCLL = ROC - 2 * ROCSD) %>%
                                                        arrange(group, dayImp)
  
  loc.strip <- c(1:9,11:19,21:29,31:39)
  ylims <- c(min(imputationResultsDF$ROCLL),
             max(imputationResultsDF$ROCUL))
  
  stripchart(ROC ~ dayImp + group,
             imputationResultsDF,
             vertical = TRUE,
             pch = "-",
             cex = 2,
             las = 2,
             ylab = "AUC",
             at = loc.strip,
             xaxt = "n",
             xlim = c(0,40),
             ylim = ylims,
             main = ifelse(title == TRUE, paste("AUC of 20 folds of 9 ", MLmethod, " models for each of 9 imputations.\nMean +/- 2SD", sep = ""), NA))
  
  stripchart(ROCUL ~ dayImp + group,
             imputationResultsDF,
             vertical = TRUE,
             pch = "-",
             cex = 1,
             at = loc.strip, 
             add = TRUE)
  
  stripchart(ROCLL ~ dayImp + group,
             imputationResultsDF,
             vertical = TRUE,
             pch = "-",
             cex = 1,
             at = loc.strip, 
             add = TRUE)
  
  segments(x0 = loc.strip, x1 = loc.strip,
           y0 = imputationResultsDF$ROCUL, y1 = imputationResultsDF$ROCLL,
           lwd = 1)
  
  groupMeans <- imputationResultsDF %>% group_by(group, cumulative) %>% summarise(meanROC = mean(ROC))
  
  segments(x0 = c(1,11,21,31,-10,-10,-10,-10), x1 = c(9,19,29,39,50,50,50,50),
           y0 = groupMeans$meanROC, y1 = groupMeans$meanROC,
           lwd = rep(c(2,1),each = 4), col = c("blue", "red", "blue", "red"), lty = rep(c(1,3),each = 4))
  
  # add x-axis
  labs.1 <- c("simple", "cumul.", "simple", "cumul.")
  labs.2 <- c(group1name, group2name)
  
  lab.pos.1 <- seq(5,35,10)
  lab.pos.2 <- seq(10,30,20)
  
  axis(side = 1, at = lab.pos.1, labels = labs.1, tick = FALSE)
  axis(side = 1, at = lab.pos.2, labels = labs.2, tick = FALSE, line = NA, padj = 1, font = 2)
  
  # add vertical lines
  line.pos.main <- mean(lab.pos.2)
  
  segments(y0 = 0, y1 = 1,
           x0 = line.pos.main, x1 = line.pos.main, lty = 1, col = "grey")
  
  
  dev.off()
}
