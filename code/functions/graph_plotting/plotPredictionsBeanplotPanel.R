# plots either one beanplot of a panel of beanplots showing the distributino of the named variable for correct/incorrect predictions of alive/deceased classifier values

plotPredictionsBeanplot <- function(inputDF, day.in, cumul.in, variableName, legend = TRUE, ylimits = NA, density = 3, title = TRUE, xlabel = "Actual outcome", ylabel = TRUE) {
  plot.df <- filter(inputDF, day == day.in, cumulative == cumul.in) %>% select_("obs", "correct", variableName)
  
  levels(plot.df$obs) <- c("deceased", "alive")
  
  colnames(plot.df)[3] <- "variable"
  
  plot.df.correct <- filter(plot.df, correct == TRUE)
  plot.df.incorrect <- filter(plot.df, correct == FALSE)
  
  x.limits <- c(0.3,2.7)
  
  beanplot(variable ~ obs,
           plot.df.correct,
           side = "first",
           horizontal = FALSE,
           xlim = x.limits,
           ylim = ylimits,
           col=c("darkseagreen2","darkseagreen2", "lightgrey"), bw = density, ll = 0.002, 
           what = c(0,1,0,0), log = "", las = 1,
           ylab = ifelse(ylabel == TRUE, variableName, NA),
           main = ifelse(title == TRUE, paste("Beanplots of", variableName, "for correct and incorrect classifications on day", day.in, ifelse(cumul.in == TRUE, "cumul.", "simple"), sep = " "), NA))
  
  axis(1, at = 1.5, labels = xlabel, tick = FALSE, line = 1, font = 2)
  
  beanplot(variable ~ obs, plot.df.incorrect, 
           side = "second", 
           col=c("lightpink2","lightpink2", "lightgrey"), 
           add = TRUE, bw = density, ll = 0.002,
           what = c(0,1,0,0))
  
  means.df <- plot.df %>% group_by(obs, correct) %>% summarise(mean = mean(variable),
                                                               sd = sd(variable),
                                                               upSD = mean + sd,
                                                               downSD = mean - sd)
  bars.list <- unlist(c(filter(means.df, obs == "deceased", correct == TRUE)[,c("mean", "upSD", "downSD")],
                        filter(means.df, obs == "deceased", correct == FALSE)[,c("mean", "upSD", "downSD")],
                        filter(means.df, obs == "alive", correct == TRUE)[,c("mean", "upSD", "downSD")],
                        filter(means.df, obs == "alive", correct == FALSE)[,c("mean", "upSD", "downSD")]))
  segments(x0 = rep(c(1,2), each = 6), x1 = rep(c(0.55,1.45,1.55,2.45), each = 3),
           y0 = bars.list, y1 = bars.list,
           col = "black", 
           lwd = rep(c(1.5,1,1), times = 4),
           lty = rep(c(1,2,2), times = 4))
  
  if(legend == TRUE) legend('topright', fill=c('darkseagreen2','lightpink2'), legend= c("correct", "incorrect"), bty ="n")
}

plotPredictionsBeanplotPanel <- function(variable, inputDF, density = 1, ylimits = NA, title = TRUE, footer = TRUE, MLmethod) {
  
  plotMiscGraph("PredictionBeanplotPanels", 
                paste("beanplot_predictions_panel_", variable, sep = ""), 
                width = 4000,
                height = 2000)
  
  par(mfrow = c(2,5), oma = c(ifelse(footer == TRUE, 6, 1), 2, ifelse(title == TRUE, 6, 1), 2), mar = c(3, 3, 1.5, 0))
  
  day.cumul <- expand.grid(day = 1:5, cumul = c(FALSE,TRUE))
  for (i in 1:nrow(day.cumul)) {
    if (day.cumul$day[i] == 1 & day.cumul$cumul[i] == TRUE) {
      plot.new()
      legend('center', fill=c('darkseagreen2','lightpink2'), legend= c("correct", "incorrect"), bty ="n")
    } else {
      plotPredictionsBeanplot(inputDF = inputDF,
                              day.in = day.cumul$day[i], 
                              cumul.in = day.cumul$cumul[i],
                              variableName = variable,
                              legend = FALSE, 
                              density = density, 
                              ylimits = ylimits,
                              title = FALSE, 
                              xlabel = paste("Day", 
                                             day.cumul$day[i],
                                             ifelse(day.cumul$cumul[i] == TRUE, "cumul.", "simple"), sep = " "),
                              ylabel = ifelse(day.cumul$day[i] == 1 | (day.cumul$day[i] == 2 & day.cumul$cumul[i] == TRUE), TRUE, FALSE))
    }
  }
  
  if (title == TRUE) mtext(paste("Beanplots of distributions of", variable, "for correct and incorrect classifications, split by actual outcome.\n", MLmethod, "classifier.", sep = " "), outer = TRUE, cex = 1.2)
  if (footer == TRUE) mtext("Thick bars represent mean and dashed bars mean +/- one standard deviation", outer = TRUE, cex = 0.8, side = 1, at = 0.2, line = 2)
  
  dev.off()
}