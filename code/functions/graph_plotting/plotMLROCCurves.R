
# TODO: FIX THIS!!
# ### 10. Plot the ROC curves
# 
# plotMLROCCurves <- function(bestPred.df, MLmethod, combined = FALSE, verbose = TRUE) {
#   bestPred.df <- filter(bestPred.df, day > 0)
#   roc.curve.plots <- arrange(unique(select(bestPred.df, day, cumulative)), day, cumulative)
#   for (i in 1:nrow(roc.curve.plots)) {
#     
#     day.n <- roc.curve.plots[i,]$day
#     cumul <- roc.curve.plots[i,]$cumulative
#     cumul.name <- ifelse(cumul, "cumulative", "only")
#     
#     #set up write file
#     if (combined == FALSE) {
#       plotMiscGraph("ML_output_ROCcurves", paste("ROCcurve", MLmethod, day.n, sep = "_"), 3000, 3000)
#     } else if (day.n == 1) {
#       plotMiscGraph("ML_output_ROCcurves", paste("ROCcurve", MLmethod, "allDays", sep = "_"), 3000, 3000)
#     }
#     
#     # calculate and plot ROC for each resample
#     for(resample in 1:20) {
#       if (resample <= 9) {
#         resample.name <- paste("Resample0", resample, sep = "")
#       } else {
#         resample.name <- paste("Resample", resample, sep = "")
#       }
#       
#       if (verbose == TRUE) print(paste("Plotting", resample.name, "for day", day.n, cumul.name, sep = " "))
#       
#       resample.df <- filter(bestPred.df, day == day.n, Resample == resample.name, cumulative == cumul)
#       
#       # draw ROC curve
#       result.roc.temp <- pROC::roc(resample.df$obs, 
#                                    resample.df$alive)
#       
#       legend.text <- c("Day 1", "Day 2", "Day 2 cumul.", "Day 3", "Day 3 cumul.", "Day 4", "Day 4 cumul.", "Day 5", "Day 5 cumul.")
#       cols <- c("grey", "lightblue", "darkblue", "pink", "darkred", "orange", "darkorange", "lightgreen", "darkgreen")
#       
#       if (combined == FALSE) {
#         addBool <- ifelse(resample == 1, FALSE, TRUE)
#         plot(result.roc.temp, 
#              main = paste("ROC: ", MLmethod, " - ", "day ", day.n, " ", cumul.name, sep = ""),
#              add = addBool,
#              lwd = 0.5,
#              col = cols[day.n])
#       } else {
#         addBool <- ifelse(resample == 1 & day.n == 1, FALSE, TRUE)
#         plot(result.roc.temp, 
#              main = paste("ROC: ", MLmethod, " - ", "all days", sep = ""),
#              add = addBool,
#              lwd = 0.5,
#              col = cols[day.n])
#         
#         legend("bottomright", pch = 15, legend = legend.text, col = cols, inset = 0.03, bty = "n")
#       }
#       
#     }
#     if(combined == FALSE) dev.off()
#   }
#   if(combined == TRUE) dev.off()
# }
# 
# # plot 3 day models
# plotMLROCcurves(ML.bestPred.df.adaboost, "adaboost", combined = TRUE)
# plotMLROCcurves(ML.bestPred.df.avNNet, "avNNet", combined = TRUE, verbose = TRUE)
# plotMLROCcurves(ML.bestPred.df.parRF, "parRF", combined = TRUE, verbose = TRUE)
# plotMLROCcurves(ML.bestPred.df.svmRadialWeights, "svmRadialWeights", combined = TRUE, verbose = TRUE)
# plotMLROCcurves(ML.bestPred.df.glm, "glm", combined = TRUE, verbose = TRUE)
# 
# # plot 5 day models
# plotMLROCcurves(ML.bestPred.df.adaboost, "adaboost", combined = TRUE)
# plotMLROCcurves(ML.bestPred.df.avNNet, "avNNet", combined = TRUE, verbose = TRUE)
# plotMLROCcurves(ML.bestPred.df.parRF, "parRF", combined = TRUE, verbose = TRUE)
# plotMLROCcurves(ML.bestPred.df.svmRadialWeights, "svmRadialWeights", combined = TRUE, verbose = TRUE)
# plotMLROCcurves(ML.bestPred.df.glm, "glm", combined = TRUE, verbose = TRUE)
# 
# 
# ### TODO: FIX this!
# # plot a comparison ROC curve
# ML.bestPred.df.ALL <- rbind(select(ML.bestPred.df.adaboost, pred, obs, dead, alive, Resample, ML.method, day, iter, bt.Str),
#                             select(ML.bestPred.df.avNNet, pred, obs, dead, alive, Resample, ML.method, day, iter, bt.Str),
#                             select(ML.bestPred.df.parRF, pred, obs, dead, alive, Resample, ML.method, day, iter, bt.Str),
#                             select(ML.bestPred.df.svmRadialWeights, pred, obs, dead, alive, Resample, ML.method, day, iter, bt.Str),
#                             select(ML.bestPred.df.glm, pred, obs, dead, alive, Resample, ML.method, day, iter, bt.Str))
# 
# MLmethods <- c("adaboost", "avNNet", "parRF", "svmRadialWeights", "glm")
# plotComparisonROCcurves <- function(verbose = FALSE) {
#   for (day.n in 1:3) {
#     for (MLmethod in MLmethods) {
#       #set up write file
#       if (MLmethod == "adaboost") {
#         plotMiscGraph("ML_output_ROCcurves_comparison", paste("MLcomparison_rocCurve_day", day.n, sep = "_"), 3000, 3000)
#       }
#       
#       if (verbose == TRUE) print(paste("PLotting",MLmethod,"for day",day.n, sep = " "))
#       # retrieve the right predictions to draw an aggregate ROC curve
#       resample.df <- filter(ML.bestPred.df.ALL, day == day.n, ML.method == MLmethod)
#       
#       # draw ROC curve
#       result.roc.temp <- pROC::roc(resample.df$obs, 
#                                    resample.df$alive)
#       
#       group.cols <- c("darkred", "darkblue", "green", "pink", "blue")
#       
#       addBool <- ifelse(MLmethod == "adaboost", FALSE, TRUE)
#       plot(result.roc.temp, 
#            main = paste("ROC: comparison of methods - day", day.n, sep = " "),
#            add = addBool,
#            lwd = 1,
#            col = group.cols[grep(MLmethod, MLmethods)])
#       
#     }
#     legend("bottomright", pch = 15, legend = MLmethods, col = group.cols, inset = 0.03, bty = "n")
#     dev.off()
#   }
# }
# plotComparisonROCcurves(verbose = TRUE)
