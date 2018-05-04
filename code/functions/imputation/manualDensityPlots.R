# function for plotting the density plots showing original values vs. imputations

library(mice)
library(parallel)
library(dplyr)

manualDensityPlots <- function(mids, cleanData, title = TRUE, minititles = TRUE, plotDir = "densityPLots_manual") {
  for(title in unique(filter(allVariablesDF, outlierRemoval == TRUE, day > 0, !(variableTitle %in% c("NoradrenalineTotal", "AdrenalineTotal", "VasopressinTotal")))$variableTitle)) {
    varDF <- allVariablesDF %>% filter(variableTitle == title) %>% arrange(variableSubtitle)
    
    maxVars <- varDF[1:(nrow(varDF)/2),]$variable
    minVars <- varDF[(1 + nrow(varDF)/2):nrow(varDF),]$variable
    
    plotMiscGraph(plotDir, 
                  paste("density_manual_", title, sep = ""), 
                  4000,
                  2000)
    
    par(mfrow = c(2,5), oma = c(0, 0, 3, 0))
    for(minMax in c("min", "max")) {
      temp.vars <- get(paste(minMax,"Vars", sep = ""))
      for (day in 1:(nrow(varDF)/2)) {
        d <- density(cleanData[[temp.vars[day]]][!is.na(cleanData[[temp.vars[day]]])])
        plot(d, 
             col = "darkblue", 
             xlab = temp.vars[day],
             ylab = ifelse(day == 1, "Density", ""),
             main = ifelse(minititles == TRUE, temp.vars[day], NA),
             ylim = c(0,max(d$y) * 1.3))
        for (i in 1:7) {
          compl.full <- mice::complete(mids,i)
          
          # find values which shouldn't exist
          naPos <- compl.full$icu_duration_days < day
          wherePos <- as.data.frame(mids$where)[[temp.vars[day]]]
          
          imputedPoints <- wherePos & !naPos
          
          #plot density without extroneous values
          d.x <- density(compl.full[imputedPoints, temp.vars[day]])
          lines(d.x, col = "#A3C1AD", lwd = 0.5)
        }
        lines(d, col = "darkblue")
        text(y = max(d$y) + 0.15 * max(d$y), x = max(d$x), pos = 2, labels = paste("Original values: ", sum(!wherePos), sep = ""), col = "darkblue", cex = 0.9)
        text(y = max(d$y) + 0.08 * max(d$y), x = max(d$x), pos = 2, labels = paste("Imputed values: ", sum(imputedPoints), sep = ""), col = "#A3C1AD", cex = 0.9)
      }
    }
    
    if (title == TRUE) mtext(paste("Density plot of existing (blue) vs imputed (orange) data for ", title, sep = ""), outer = TRUE, cex = 1.5)
    
    dev.off()
  }
}
