# See if trends in mean/max/95th-%ile/hypereut/var are consistent for time series of different length

# SET THIS to alpha value for significance of bloom intensity trends
P_cutoff = 0.05

# read in results coefficients
coef_avg = read.csv("results_AvgChl_vs_Year_gls.csv", stringsAsFactors = FALSE)
coef_p95 = read.csv("results_p95Chl_vs_Year_gls.csv", stringsAsFactors = FALSE)
coef_tIM = read.csv("results_tIMug_vs_Year_gls.csv", stringsAsFactors = FALSE)

# combine coefficients for easier plotting
coef_avg$Variable = "Mean Concentration"
coef_p95$Variable = "95th percentile"
coef_tIM$Variable = "Prop. Impaired"
all_coef = rbind(coef_avg, coef_p95, coef_tIM)

# make plot of % of coef.s +/-/significant vs. TS length
VarsToPlot = c("Mean Concentration", "95th percentile", "Prop. Impaired")
yearGroupCutoffs = c(10, 15, 20, 25, 30, max(all_coef$ts.length) + 1)

pdf("SUMMARY_Trends_vs_TimeSeriesLength.pdf", width=6.5, height=5)
par(mfrow=c(2,3), oma=c(3,4,2,0.25), mar=c(2,3,3,1))
for(posNeg in c(1,-1)){
  for(i in 1:length(VarsToPlot)){
    indsToPlot_all = all_coef$Variable == VarsToPlot[i] & all_coef$recovery.flag == FALSE
    # Total number of lakes, and numbers + percent that are pos./neg., and pos./neg. and significant
    N_total = sum(indsToPlot_all)
    # Percent of all lakes that are positive/negative trend
    percentAllCutoffs_all = (sum((posNeg*all_coef[indsToPlot_all, "b1"]) > 0, na.rm=TRUE) / N_total)*100
    # Percent of all lakes that are positive/negative & significant trend
    percentAllCutoffs_sig = (sum((posNeg*all_coef[indsToPlot_all, "b1"]) > 0 & all_coef[indsToPlot_all, "b1.p"] < P_cutoff, na.rm=TRUE) / N_total)*100
    # Iterate through time series by group
    for(j in 1:(length(yearGroupCutoffs)-1)){
      # get indices of lakes w/ time sereies length w/in current group
      indsToPlot_current = indsToPlot_all & all_coef$ts.length >= yearGroupCutoffs[j] & all_coef$ts.length < yearGroupCutoffs[j+1]
      # make x-axis label for current group
      curLabel = ifelse(j<(length(yearGroupCutoffs)-1), 
                    paste0(yearGroupCutoffs[j], "-", (yearGroupCutoffs[j+1]-1)),
                    paste0(yearGroupCutoffs[j],"+"))
      # subset data for lakes w/ time sereis length in current group
      X = all_coef[indsToPlot_current,]
      # calculate percent pos/neg and pos/neg + significant
      curPercent_all = (sum((posNeg*X[, "b1"]) > 0, na.rm=TRUE) / nrow(X))*100
      curPercent_sig = (sum((posNeg*X[, "b1"]) > 0 & X[, "b1.p"] < P_cutoff, na.rm=TRUE) / nrow(X))*100
      # save label and percents
      if(j==1){
        allLabels = c(curLabel)
        allPercent_all = c(curPercent_all)
        allPercent_sig = c(curPercent_sig)
      }else{
        allLabels = c(allLabels, curLabel)
        allPercent_all = c(allPercent_all, curPercent_all)
        allPercent_sig = c(allPercent_sig, curPercent_sig)
      }
    }
    # make plot
    colSig = ifelse(posNeg==1, "palegreen3", "skyblue3")
    colAll = ifelse(posNeg==1, "palegreen4", "skyblue4")
    plot(c(1:5),allPercent_all, pch=19, cex=2, ylim=c(0,100), col=colAll, xaxt="n", ylab="", xlab="", xlim=c(0.5,5.5), cex.axis=1.25, las=2)
    abline(h=percentAllCutoffs_all, lwd=2, lty=2, col=colAll)
    axis(side=1, at=c(1,2,3,4,5), labels=allLabels, cex.axis=1.25, las=2)
    # mtext(side=3, line=0.5, VarsToPlot[i], cex=1.25)
    points(c(1:5), allPercent_sig, pch=15, cex=2, col=colSig)
    abline(h=percentAllCutoffs_sig, lwd=2, lty=2, col=colSig)
    if(i == 1){
      posNegLabel = ifelse(posNeg==1, "Positive", "Negative")
      Ylab = paste(posNegLabel, "Trend")
      mtext(side=2, Ylab, line=3, cex=1)
      # legend(x="topleft", legend=c("All", "Significant"), text.col=c(colAll, colSig), bty="n", cex=1.25)
    }
  }
  mtext(side=2, "% of Lakes", outer=TRUE, line=2, cex=1.25)
  mtext(side=3, line=-2.5, outer=TRUE, "     Severity", cex=1.1)
  mtext(side=3, line=-2.5, outer=TRUE, "Intensity                                                                          ", cex=1.1)
  mtext(side=3, line=-2.5, outer=TRUE, "                                                                                   Duration", cex=1.1)
}
dev.off()