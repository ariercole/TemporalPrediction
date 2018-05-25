# Script to plot waterfall graph of admission length

library(dplyr)

plot.summary <- cleanData.all %>% group_by(icu_duration_days) %>% summarise(tot = n(), noAlive = sum(alive == "alive"), noDead = sum(alive != "alive"))

plot.summary$totLeft <- NA
plot.summary$missingClassifier <- NA


for (i in 1:nrow(plot.summary)) {
  plot.summary$totLeft[i] <- nrow(cleanData.all) - sum(plot.summary$tot[1:i])
}
plot.summary <- rbind(list(icu_duration_days = c(-2,-1,0),
                           tot = c(0,0,0),
                           noAlive = c(0,0,0),
                           noDead = c(0,0,0),
                           totLeft = c(nrow(dataSet),nrow(cleanData.all),nrow(cleanData.all)),
                           missingClassifier = c(NA, nrow(dataSet) - nrow(cleanData.all),0)),
                      plot.summary)


plot.summary.mut <- transmute(plot.summary, day = icu_duration_days, remaining = totLeft, alive = noAlive, dead = noDead, issingClassifier = missingClassifier)
plot.df.full <- t.data.frame(plot.summary.mut)
colnames(plot.df.full) <- plot.df.full[1,]
colnames(plot.df.full)[1:3] <- c("total","","")
plot.df <- plot.df.full[c(2:5),1:33]
plot.df <- rbind(plot.df, holdRow = 0)
plot.df <- plot.df[c(5,1,2,3,4),]

plot.df <- plot.df[,c(1:2, 4:ncol(plot.df))]

for(i in c(1,3,7, seq(12, 102, 10))) {
  if(ncol(plot.df) >= i) {
    plot.df[1,i] <- plot.df[2,i]
    plot.df[2,i] <- 0
  }
}

plotMiscGraph("Descriptive", "patientDecay_30", 4000, 2200)
barplot(plot.df, xlab="Day of admission", col=c("darkgrey","white","#A3C1AD","darkblue", "darkred"), border = NA, las = 1, ylim = c(0,26000), width = 0.9, space = 1/9, xlim = c(0,ncol(plot.df)+1))
legend("topright", inset = 0.03, legend = c("missing classifier","discharged alive", "deceased", "remain on ICU"), col = c("darkred", "#A3C1AD","darkblue","grey"), bty = "n", pch = 15)
box()

text(x = 1.15, y = 23300, labels = "22,524", pos = 3, adj = 0, srt = 45)
text(x = 3.15, y = 22700, labels = "21,911", pos = 3, adj = 0, srt = 45)
text(x = 7.6, y = 8550, labels = "6,916 (32%)", pos = 3, adj = 0, srt = 45)
text(x = 12.6, y = 4750, labels = "2,909 (13%)", pos = 3, adj = 0, srt = 45)
text(x = 22.5, y = 2500, labels = "1,092 (5%)", pos = 3, adj = 0, srt = 45)
text(x = 32.2, y = 1600, labels = "489 (2%)", pos = 3, adj = 0, srt = 45)
dev.off()

