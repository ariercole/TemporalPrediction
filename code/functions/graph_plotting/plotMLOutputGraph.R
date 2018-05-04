# flexible graph plotting function for output of ML tuning

source("functions/graph_plotting/plotMiscGraph.R")

plotMLOutputGraph <- function(plotMetric, plotName, resampleResults, MLmethods, file.name, day.range = 5, lines.plot = FALSE, legend.position = "bottomleft") {
  colour.df <- data.frame(MLmethod = c("adaboost", "avNNet", "parRF", "svmRadialWeights", "glm", "DeepNN", "APACHE"), 
                          col = c("darkred", "darkblue", "green", "pink", "blue", "orange", "grey"), 
                          stringsAsFactors = FALSE)
  
  plotMiscGraph("Grand_ML_output_graphs", file.name, 4000, 2000)
  
  plot.df <- filter(resampleResults, day <= day.range)
  
  apache.present <- 0 %in% plot.df$day
  if (apache.present) {
    loc.start <- 4
    APACHE.df <- filter(plot.df, day == 0)
    plot.df <- filter(plot.df, day > 0)
  } else {
    loc.start <- 1
  }
  
  
  labs.all <- c("Day 1", "Day 1\nCumul.", "Day 2", "Day 2\nCumul.", "Day 3", "Day 3\nCumul.", "Day 4", "Day 4\nCumul.", "Day 5", "Day 5\nCumul.")
  labs <- labs.all[1:(day.range * 2)]
  
  # generate list of plotted factors
  plottingFactors <- unique(plot.df %>% filter(day > 0) %>% select(MLmethod, day, cumulative))
  
  # determine how wide each day plot should be based on how many ML methods are used plus a visual buffer
  no.x.plots <- length(MLmethods) + 2
  
  # determine the "base" points from which to reference the plots
  loc.base <- c(loc.start, (1:(day.range + day.range - 1) * no.x.plots - (no.x.plots - loc.start))) #determine start of each day, including space for cumulative plots
  
  # generate df for calculating appropriate y axis
  ylims.df <- filter(resampleResults, MLmethod %in% MLmethods)
  
  # set limits on the axes
  x.limits <- c(0.5, max(loc.base) - 1.5 + no.x.plots) # set x axis limits
  y.limits <- c(min(ylims.df[[plotMetric]]), max(ylims.df[[plotMetric]]))
  
  # define the plotting formula
  formula <- formula(paste(plotMetric,"~ cumulative + day", sep = ""))
  
  # plot day 1 cumulative outside the plot range as it never exists
  loc.base[2] <- 1000 
  
  for(i in 1:length(MLmethods)) {
    addBool <- ifelse(i == 1, FALSE, TRUE) # is this the first graph?
    
    filtered.plot.df <- filter(plot.df, MLmethod == MLmethods[i])
    
    loc.strip.all <- loc.base + i - 1
    
    filtered.plot.df.2 <- filtered.plot.df
    filtered.plot.df.2$day <- factor(filtered.plot.df.2$day, levels = 1:day.range)
    filtered.plot.df.2$cumulative <- factor(filtered.plot.df.2$cumulative, levels = c(FALSE,TRUE))
    
    stripchart(formula,
               filtered.plot.df.2, 
               vertical = TRUE, 
               at = loc.strip.all, 
               xlim = x.limits, 
               ylim = y.limits,
               pch = 16,
               cex = 0.5,
               method = "jitter",
               col = as.character(colour.df$col[colour.df$MLmethod == MLmethods[i]]),
               las = 2,
               xaxt = "n",
               ylab = plotName,
               main = paste("Comparison of ML methods tuned to AUROC: difference in ", plotName, ".\nMean +/- 1.96SEM", sep = ""),
               add = addBool)
    
    # add means and error bars dynamically
    mutated.plot.df <- mutate_(filtered.plot.df, outVar = plotMetric)
    summary.df <- mutated.plot.df %>% 
      group_by(day, cumulative) %>%
      summarise(mean = mean(outVar),
                sd = sd(outVar),
                sem = sd / sqrt(n()),
                ul = mean + 1.96 * sem,
                ll = mean - 1.96 * sem)
    
    total.df <- expand.grid(day = 1:day.range, cumulative = c(TRUE,FALSE))
    total.df <- suppressMessages(left_join(total.df, summary.df))
    
    for(col in colnames(total.df)) {
      total.df[[col]] <- ifelse(is.na(total.df[[col]]), 0, total.df[[col]])
    }
    
    total.df <- arrange(total.df, day, cumulative)
    
    segments(x0 = c(loc.strip.all - 0.3, loc.strip.all - 0.3, loc.strip.all - 0.3, loc.strip.all),
             x1 = c(loc.strip.all + 0.3, loc.strip.all + 0.3, loc.strip.all + 0.3, loc.strip.all),
             y0 = c(total.df$ul, total.df$ll, total.df$mean, total.df$ul),
             y1 = c(total.df$ul, total.df$ll, total.df$mean, total.df$ll),
             lwd = rep(c(1,1,2,0.8), each = length(loc.strip.all)))
    
    ### add lines to indicate trend
    if (lines.plot == TRUE) {
      lines(filter(total.df, cumulative == FALSE)$mean, lwd = 0.5, lty = 2, x = loc.strip.all[c(1,3,5,7,9)], col = as.character(colour.df$col[colour.df$MLmethod == MLmethods[i]]))
      lines(c(filter(total.df, day == 1, cumulative == FALSE)$mean,filter(total.df, cumulative == TRUE, day > 1)$mean), lwd = 0.5, lty = 1, x = loc.strip.all[c(1,4,6,8,10)], col = as.character(colour.df$col[colour.df$MLmethod == MLmethods[i]]))
    }    
    
    # add APACHE if appropriate
    if(MLmethods[i] == "glm" & apache.present) {
      stripchart(formula,
                 APACHE.df, 
                 vertical = TRUE, 
                 at = 1, 
                 pch = 16,
                 cex = 0.5,
                 method = "jitter",
                 col = as.character(colour.df$col[colour.df$MLmethod == "glm"]),
                 las = 2,
                 add = TRUE)
      
      mean.APACHE <- mean(APACHE.df[[plotMetric]])
      sem.APACHE <- 1.96 * (sd(APACHE.df[[plotMetric]]) / sqrt(nrow(APACHE.df)))
      ul <- mean.APACHE + sem.APACHE
      ll <- mean.APACHE - sem.APACHE
      
      segments(x0 = c(0.7,0.7,0.7,1),
               x1 = c(1.3,1.3,1.3,1),
               y0 = c(ul, ll, mean.APACHE, ul),
               y1 = c(ul, ll, mean.APACHE, ll),
               lwd = c(1,1,2,0.8))
      
      axis(side = 1, at = 1, labels = "APACHE", tick = FALSE)
    }
  }
  colour.df <- arrange(colour.df, MLmethod)
  legend(legend.position, inset = 0.02, bty = "n", pch = 16, legend = sort(MLmethods), col = (colour.df$col[colour.df$MLmethod %in% MLmethods]))
  
  axis(side = 1, at = loc.base + ((no.x.plots - 3) / 2), labels = labs, tick = FALSE)
  
  dev.off()
}