# plots a graph of the results of the nine imputations for each day

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
