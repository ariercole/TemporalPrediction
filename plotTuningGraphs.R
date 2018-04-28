### Plot tuning results
library(dplyr)

line.cols <- c("darkred", "darkblue", "darkgreen", "pink", "orange", "blue", "green", "purple", "red", "black", "grey", "yellow", "lilac", "violet", "brown")
pch.list <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

### SVM Radial

tuning.plot.list <- unique(select(ML.results.df.svmRadialWeights, day, cumulative))

for (i in 1:nrow(tuning.plot.list)) {
  day.x <- tuning.plot.list[i,]$day
  cumul <- tuning.plot.list[i,]$cumulative
  
  plotMiscGraph("MLtuning", paste("svmRadial_tuning_day", day.x, ifelse(cumul == TRUE, "_cumulative", ""), sep = ""), 4000, 2000)
  
  # filter for the correct range of days, 3 or 5
  ML.results.df.svmRadial.filtered <- filter(ML.results.df.svmRadialWeights, cumulative == cumul, day == day.x)
  
    
  nGraphs <- length(unique(ML.results.df.svmRadial.filtered$sigma)) + 1
  width <- ceiling(sqrt(nGraphs))
  height <- ceiling(nGraphs / width)
  par(mfrow = c(height, width), oma = c(0, 0, 3, 0))
  
  #set the colours for all weights
  line.cols.df <- data.frame(Weight = unique(ML.results.df.svmRadial.filtered$Weight), col = line.cols[1:length(unique(ML.results.df.svmRadial.filtered$Weight))])
  line.cols.df <- arrange(line.cols.df, Weight)
  
  # set y axis limits
  y.limits <- c(0.6,0.9)
  
  for(sig in unique(ML.results.df.svmRadial.filtered$sigma)) {
    tempDF <- filter(ML.results.df.svmRadial.filtered, sigma == sig)
    
    #find the right colour for the weights displayed
    line.cols.list <- line.cols.df$col[match(unlist(unique(tempDF$Weight)), line.cols.df$Weight)]

    interaction.plot(x.factor = tempDF$C, trace.factor = tempDF$Weight, response = tempDF$ROC, col = line.cols.list, lty = 1, legend = F, xlab = "Cost", ylab = "ROC", ylim = y.limits, main = paste("Sigma = ", sig, sep = ""))
    abline(h = list(0.5,0.6,0.7,0.8,0.9), lty = 2, lwd = 0.3, col = "grey")
    abline(h = list(0.55,0.65,0.75,0.85), lty = 3, lwd = 0.3, col = "lightgrey")
  }
  mtext(paste("SVM Radial Tuning: Day", day.x, ifelse(cumul == TRUE, "_cumulative", "only"), sep = " "), outer = TRUE, cex = 1.5)
  plot.new()
  
  # determine the necessary weights for the legend and plot the legend
  legend.df <- line.cols.df[match(unlist(unique(ML.results.df.svmRadial.filtered$Weight)), line.cols.df$Weight),]
  legend("center", legend = unique(line.cols.df$Weight), lty = 1, lwd = 2, col = line.cols.df$col, bty = "n", title = "Weight")
  dev.off()
}

### avNNet

tuning.plot.list <- unique(select(ML.results.df.avNNet, day, cumulative))

for (i in 1:nrow(tuning.plot.list)) {
  day.x <- tuning.plot.list[i,]$day
  cumul <- tuning.plot.list[i,]$cumulative
  
  plotMiscGraph("MLtuning", paste("avNNet_tuning_day", day.x, ifelse(cumul == TRUE, "_cumulative", ""), sep = ""), 4000, 2000)
  
  # filter for the correct range of days, 3 or 5
  ML.results.df.avNNet.filtered <- filter(ML.results.df.avNNet, cumulative == cumul, day == day.x)
  
  #set the colours and pch for all weights
  line.cols.df <- data.frame(size = unique(ML.results.df.avNNet$size), col = line.cols[1:length(unique(ML.results.df.avNNet$size))], pch = pch.list[1:length(unique(ML.results.df.avNNet$size))])
  line.cols.df <- arrange(line.cols.df, size)
  
  # set up the graph layout
  layout(cbind(1,1,1,2,2,2,3))
  par(oma = c(0, 0, 3, 0))

  # set y limits for all graphs
  y.limits <- c(max(min(ML.results.df.avNNet.filtered$ROC),
                    max(ML.results.df.avNNet.filtered$ROC)-0.1), 
                max(ML.results.df.avNNet.filtered$ROC) + 0.005)
 
    for(bag.x in c(TRUE,FALSE)) {
    tempDF <- filter(ML.results.df.avNNet.filtered, bag == bag.x)
    
    #find the right colour for the weights displayed
    line.cols.list <- line.cols.df$col[match(unlist(unique(tempDF$size)), line.cols.df$size)]
    line.pch.list <- line.cols.df$pch[match(unlist(unique(tempDF$size)), line.cols.df$size)]
    
    interaction.plot(x.factor = tempDF$decay, trace.factor = tempDF$size, response = tempDF$ROC, col = line.cols.list, lty = 1, legend = F, xlab = "Decay", ylab = "ROC", ylim = y.limits, main = paste("Bag = ", bag.x, sep = ""), type = "b", pch = line.pch.list, cex = 1)
    abline(h = seq(0.6,0.9,by = 0.01), lty = 2, lwd = 1, col = "lightgrey")
    abline(h = seq(0.605,0.905,by = 0.01), lty = 3, lwd = 0.8, col = "lightgrey")
  }
  
  mtext(paste("avNNet Tuning: Day", day.x, ifelse(cumul == TRUE, "_cumulative", "only"), sep = " "), outer = TRUE, cex = 1.5)
  plot.new()
  
  legend.df <- line.cols.df[match(unlist(unique(ML.results.df.avNNet.filtered$size)), line.cols.df$size),]
  legend("center", legend = legend.df$size, lty = 1, lwd = 1, col = legend.df$col, bty = "n", title = "Size", pch = legend.df$pch)
  dev.off()
}


### adaboost
# determine plotting colours
temp.DF <- mutate(ML.results.df.adaboost, legend = paste(day,cumulative))
legend.df <- data.frame(code = c("Adaboost.M1", "Real Adaboost", "", sort(unique(temp.DF$legend))),
                                  cols = c("black", "black", "black", "grey", "blue", "darkblue", "pink", "darkred", "orange", "darkorange", "green", "darkgreen")[1:(3+length(unique(temp.DF$legend)))],
                                  labs = c("Adaboost.M1", "Real Adaboost", "", "Day 1", "Day 2", "Day 2 cumul", "Day 3", "Day 3 cumul", "Day 4", "Day 4 cumul", "Day 5", "Day 5 cumul")[1:(3+length(unique(temp.DF$legend)))],
                                  stringsAsFactors = FALSE)


ML.results.df.adaboost.plot <- mutate(ML.results.df.adaboost, trace = paste(method,day,cumulative), legend = paste(day,cumulative))

nplots <- length(unlist(unique(ML.results.df.adaboost.plot$trace)))
nplots.M1 <- length(unlist(unique(filter(ML.results.df.adaboost.plot, method == "Adaboost.M1")$trace)))
nplots.real <- nplots - nplots.M1


line.cols.plot <- legend.df$col[sort(match(unlist(unique(ML.results.df.adaboost.plot$legend)), legend.df$code))]
legend.cols <- legend.df$col[sort(c(1,2,3,match(unlist(unique(ML.results.df.adaboost.plot$legend)), legend.df$code)))]
legend.labs <- legend.df$labs[sort(c(1,2,3,match(unlist(unique(ML.results.df.adaboost.plot$legend)), legend.df$code)))]


plotMiscGraph("MLtuning", "adaboost_tuning", 4000, 2000)
interaction.plot(x.factor = ML.results.df.adaboost.plot$nIter, 
                 trace.factor = ML.results.df.adaboost.plot$trace, 
                 response = ML.results.df.adaboost.plot$ROC, 
                 type = "c", 
                 trace.label = "Method & Day", 
                 ylab = "AUROC", 
                 xlab = "No. trees", 
                 main = "Adaboost tuning", 
                 col = c(line.cols.plot[1:nplots.M1], line.cols.plot[1:nplots.real]),
                 lty = c(rep(1, nplots.M1), rep(2, nplots.real)), 
                 legend = FALSE)

legend("right", 
       lty = c(1,2,0,rep(1, nplots)), 
       col = legend.cols, 
       legend = legend.labs, 
       bty = "n",
       inset = 0.05)
dev.off()



### parRF
# determine plotting colours
temp.DF <- mutate(ML.results.df.parRF, legend = paste(day,cumulative))
legend.df <- legend.df <- data.frame(code = c(sort(unique(temp.DF$legend))),
                                     cols = c("grey", "blue", "darkblue", "pink", "darkred", "orange", "darkorange", "green", "darkgreen"),
                                     labs = c("Day 1", "Day 2", "Day 2 cumul", "Day 3", "Day 3 cumul", "Day 4", "Day 4 cumul", "Day 5", "Day 5 cumul"),
                                     ltys = c(1,rep(c(1,2),4)),
                                     stringsAsFactors = FALSE)

ML.results.df.parRF.plot <- mutate(ML.results.df.parRF, trace = paste(day,cumulative), legend = paste(day,cumulative))
  
nplots <- length(unlist(unique(ML.results.df.parRF.plot$trace)))
  
line.cols.plot <- legend.df$col[sort(match(unlist(unique(ML.results.df.parRF.plot$legend)), legend.df$code))]
legend.cols <- legend.df$col[sort(match(unlist(unique(ML.results.df.parRF.plot$legend)), legend.df$code))]
legend.labs <- legend.df$labs[sort(match(unlist(unique(ML.results.df.parRF.plot$legend)), legend.df$code))]
legend.ltys <- legend.df$ltys[sort(match(unlist(unique(ML.results.df.parRF.plot$legend)), legend.df$code))]
  
plotMiscGraph("MLtuning", "parRF_tuning", 4000, 2000)
layout(cbind(1,1,1,1,1,1,2))
interaction.plot(x.factor = ML.results.df.parRF.plot$mtry, 
                 trace.factor = ML.results.df.parRF.plot$trace, 
                 response = ML.results.df.parRF.plot$ROC, 
                 type = "c", 
                 trace.label = "Method & Day", 
                 ylab = "AUROC",
                 xlab = "mtry", 
                 main = "parRF tuning", 
                 col = legend.cols,
                 lty = legend.ltys, 
                 legend = FALSE,
                 ylim = c(min(ML.results.df.parRF.plot$ROC), max(ML.results.df.parRF.plot$ROC)))
plot.new()
legend("center", 
       lty = legend.ltys, 
       col = legend.cols, 
       legend = legend.labs, 
       bty = "n",
       inset = 0.05)
dev.off()



#### DeepNN (done in seperate iterations to maintain tuning balance between parameters)
plotDeepNNtuningGraphs <- function(iters.in, results.df, iteration) {
  legend.df <- data.frame(code = c("1", "2", "2 cumul", "3", "3 cumul", "4", "4 cumul", "5", "5 cumul"),
                          cols = c("grey", "blue", "darkblue", "pink", "darkred", "orange", "darkorange", "green", "darkgreen"),
                          labs = c("Day 1", "Day 2", "Day 2 cumul", "Day 3", "Day 3 cumul", "Day 4", "Day 4 cumul", "Day 5", "Day 5 cumul"),
                          ltys = c(1,rep(c(1,2),4)),
                          stringsAsFactors = FALSE)
  
  iter.df <- filter(results.df, iter %in% iters.in)
  
  # iterate through tuning parameter plotting an aggregate graph
  for (col in colnames(iter.df)[1:14]) {
    if(length(unique(iter.df[[col]])) > 1) {
      plotMiscGraph("MLtuning", paste("DeepNN_iteration", iteration, "_", col, sep = ""), 4000, 2000)
      plot.df <- iter.df %>% group_by_(col, "day", "cumulative") %>% summarise(ROC = mean(ROC), dayCumul = paste(day[1],ifelse(cumulative[1] == TRUE, "cumul", ""))) %>% arrange(dayCumul)
      interaction.plot(x.factor = plot.df[[col]],trace.factor = plot.df$dayCumul, response = plot.df$ROC, pch = 16, col = legend.df$cols, trace.label = "Day", main = paste("Tuning paramater: ", col, sep = ""), lty = legend.df$ltys)
      dev.off()
    }
  }
}
  
# iter 1
plotDeepNNtuningGraphs(iters.in = 1:30, ML.results.df.DeepNN, 1)

# iter 2
plotDeepNNtuningGraphs(iters.in = 31:50, ML.results.df.DeepNN, 2)
