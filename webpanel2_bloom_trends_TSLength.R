# WebPanel 2 - Wilkinson et al. FE&E
# See if trends in bloom metrics are consistent for time series of different length


# set threshold for determining if a trend is 'significant'
significance_pValue = 0.05

# read in results coefficients
coef_avg = read.csv("results_AvgChl_vs_Year_gls.csv", stringsAsFactors = FALSE)
coef_p95 = read.csv("results_p95Chl_vs_Year_gls.csv", stringsAsFactors = FALSE)
coef_tIM = read.csv("results_tIMug_vs_Year_gls.csv", stringsAsFactors = FALSE)

# Look at the lengths of the time series
png("../TimeSeriesLengths.png", width=1200, height=800)
par(mfrow=c(1,2))
hist(coef_avg$ts.length, breaks=30, main="Time Series Lengths", xlab="Years") # max of 42, "clusters" between (10-13), (18-25), (28-35)
plot(ecdf(coef_avg$ts.length), main="ecdf(Time Series Lengths)", xlab="Years")
abline(h=c(0.2, 0.4, .6, 0.8))
dev.off()

# combine coefficients for easier plotting
coef_avg$Variable = "Mean Concentration"
coef_p95$Variable = "95th percentile"
coef_tIM$Variable = "Prop. Impaired"

all_coef = rbind(coef_avg, coef_p95, coef_tIM)

# make the plots
VarsToPlot = c("Mean Concentration", "95th percentile", "Prop. Impaired")
yearGroupCutoffs = c(10, 15, 20, 25, 30, max(all_coef$ts.length) + 1)

# function for getting text for left and right of histogram
getLegendText <- function(X){
  N = nrow(X)
  N_pos = sum(X[, "b1"] > 0, na.rm=TRUE)
  Per_pos = round((N_pos / N)*100, 0)
  N_posSig = sum(X[, "b1"] > 0 & X[, "b1.p"] < significance_pValue, na.rm=TRUE)
  Per_posSig = round((N_posSig / N)*100, 0)
  
  N_neg = sum(X[, "b1"] < 0, na.rm=TRUE)
  Per_neg = round((N_neg / N)*100, 0)
  N_negSig = sum(X[, "b1"] < 0 & X[, "b1.p"] < significance_pValue, na.rm=TRUE)
  Per_negSig = round((N_negSig / N)*100, 0)
  
  textNeg = paste0(Per_neg, "%    - \n", Per_negSig, "%    - & *")
  textPos = paste0(Per_pos, "%    + \n", Per_posSig, "%    + & *")
  return(c("neg"=textNeg, "pos"=textPos))
}

# Histograms of coefficients for each group of time
pdf(paste0("Trends_vs_TimeSeriesLength_", Sys.Date(), "_v2.pdf"), width=11, height=8.5)
for(i in 1:length(VarsToPlot)){
  par(mfrow=c(3,2), oma=c(2,2,3,0.25), mar=c(3,3,3,1))
  indsToPlot_all = all_coef$Variable == VarsToPlot[i] & all_coef$recovery.flag == FALSE & !is.na(all_coef$b1)
  Max = max(abs(all_coef[indsToPlot_all, "b1"]), na.rm=TRUE)
  Xlim = c(-1*Max, Max)
  # Get total number, and numbers + percent that are pos./neg., and pos./neg. and significant
  N = sum(indsToPlot_all)

  hist(all_coef[indsToPlot_all, "b1"], breaks=15, main=paste("All t.s. lengths \n n =", 
                                                            N), xlab="", xlim=Xlim, ylab="")
  Text = getLegendText(X = all_coef[indsToPlot_all,])
  legend(x="topleft", legend=Text["neg"], text.col="red", bty="n", cex=1.5)
  legend(x="topright", legend=Text["pos"], text.col="blue", bty="n", cex=1.5)
  abline(v=0, col="black", lwd=4)
  abline(v=mean(all_coef[indsToPlot_all, "b1"], na.rm=T), col="blue", lwd=2, lty="dashed")
  abline(v=median(all_coef[indsToPlot_all, "b1"], na.rm=T), col="green", lwd=2, lty="dashed")
  for(j in 1:(length(yearGroupCutoffs)-1)){
    indsToPlot_current = indsToPlot_all & all_coef$ts.length >= yearGroupCutoffs[j] & all_coef$ts.length < yearGroupCutoffs[j+1]
    Label = paste("Length >=", yearGroupCutoffs[j], "and <", yearGroupCutoffs[j+1], "\n n =", sum(indsToPlot_current))
    Text = getLegendText(X=all_coef[indsToPlot_current, ])
    hist(all_coef[indsToPlot_current, "b1"], breaks=15, main=Label, xlab="", xlim=Xlim)
    legend(x="topleft", legend=Text["neg"], text.col="red", bty="n", cex=1.5)
    legend(x="topright", legend=Text["pos"], text.col="blue", bty="n", cex=1.5)
    abline(v=0, col="black", lwd=4)
    abline(v=mean(all_coef[indsToPlot_current, "b1"], na.rm=T), col="blue", lwd=2, lty="dashed")
    abline(v=median(all_coef[indsToPlot_current, "b1"], na.rm=T), col="green", lwd=2, lty="dashed")
  }
  mtext(VarsToPlot[i], side=3, outer=TRUE, line=0.5, cex=1.5)
  mtext("Trend", side=1, outer=TRUE, line=0, cex=1.25)
  mtext("Frequency", side=2, outer=TRUE, line=0, cex=1.25)
}
dev.off()

# define colors for plot

durationcol_light <- rgb(102,102,102, max = 255, alpha = 70)
severitycol_light <- rgb(16,78,139, max = 255, alpha = 70)
magcol_light <- rgb(0,139,139, max = 255, alpha = 70)
durationcol_dark <- rgb(102,102,102, max = 255, alpha = 150)
severitycol_dark <- rgb(16,78,139, max = 255, alpha = 150)
magcol_dark <- rgb(0,139,139, max = 255, alpha = 150)

colorDF = data.frame(Var=rep(VarsToPlot, each=2), allOrSig=rep(c("all", "sig"), times=3), Col=c(magcol_light, magcol_dark, severitycol_light, severitycol_dark,  durationcol_light, durationcol_dark), stringsAsFactors = FALSE)

# make plot of % of coef.s +/-/significant vs. TS length
calcPercents <- function(X){
  N = nrow(X)
  N_pos = sum(X[, "b1"] > 0, na.rm=TRUE)
  Per_pos = (N_pos / N)*100
  N_posSig = sum(X[, "b1"] > 0 & X[, "b1.p"] < significance_pValue, na.rm=TRUE)
  Per_posSig = (N_posSig / N)*100
  
  N_neg = sum(X[, "b1"] < 0, na.rm=TRUE)
  Per_neg = (N_neg / N)*100
  N_negSig = sum(X[, "b1"] < 0 & X[, "b1.p"] < significance_pValue, na.rm=TRUE)
  Per_negSig = (N_negSig / N)*100
  
  out = c("neg_total" = Per_neg, "neg_sig" = Per_negSig, "pos_total" = Per_pos, "pos_sig" = Per_posSig)
  return(out)
}

# Summary Plot of Time Series Length Analysis
# pdf(paste0("SUMMARY_Trends_vs_TimeSeriesLength_", Sys.Date(), ".pdf"), width=6.5, height=5)
tiff(paste0("SUMMARY_Trends_vs_TimeSeriesLength_", Sys.Date(), ".tiff"), width=6.5, height=5, units="in", res=300)
par(mfrow=c(2,3), oma=c(3,4,2,0.25), mar=c(2,3,3,1))
for(posNeg in c(1,-1)){
  for(i in 1:length(VarsToPlot)){
    indsToPlot_all = all_coef$Variable == VarsToPlot[i] & all_coef$recovery.flag == FALSE
    # Get total number, and numbers + percent that are pos./neg., and pos./neg. and significant
    N_total = sum(indsToPlot_all)
    percentAllCutoffs_all = (sum((posNeg*all_coef[indsToPlot_all, "b1"]) > 0, na.rm=TRUE) / N_total)*100
    percentAllCutoffs_sig = (sum((posNeg*all_coef[indsToPlot_all, "b1"]) > 0 & all_coef[indsToPlot_all, "b1.p"] < significance_pValue, na.rm=TRUE) / N_total)*100
    for(j in 1:(length(yearGroupCutoffs)-1)){
      indsToPlot_current = indsToPlot_all & all_coef$ts.length >= yearGroupCutoffs[j] & all_coef$ts.length < yearGroupCutoffs[j+1]
      curLabel = ifelse(j<(length(yearGroupCutoffs)-1), 
                    paste0(yearGroupCutoffs[j], "-", yearGroupCutoffs[j+1]),
                    paste0(yearGroupCutoffs[j],"+"))
      X = all_coef[indsToPlot_current,]
      curPercent_all = (sum((posNeg*X[, "b1"]) > 0, na.rm=TRUE) / nrow(X))*100
      curPercent_sig = (sum((posNeg*X[, "b1"]) > 0 & X[, "b1.p"] < significance_pValue, na.rm=TRUE) / nrow(X))*100
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
    
    colSig = colorDF[colorDF$Var == VarsToPlot[i] & colorDF$allOrSig == "sig", "Col"]
    colAll = colorDF[colorDF$Var == VarsToPlot[i] & colorDF$allOrSig == "all", "Col"]
    plot(c(1:5),allPercent_all, pch=16, cex=2, ylim=c(0,100), col=colAll, xaxt="n", ylab="", xlab="", xlim=c(0.5,5.5), cex.axis=1.25, las=2)
    abline(h=percentAllCutoffs_all, lwd=2, lty=2, col=colAll)
    axis(side=1, at=c(1,2,3,4,5), labels=allLabels, cex.axis=1.25, las=2)
    # mtext(side=3, line=0.5, VarsToPlot[i], cex=1.25)
    points(c(1:5), allPercent_sig, pch=15, cex=2, col=colSig)
    abline(h=percentAllCutoffs_sig, lwd=2, lty=2, col=colSig)
    if(i == 1){
      posNegLabel = ifelse(posNeg==1, "Positive", "Negative")
      Ylab = paste(posNegLabel, "Trend")
      mtext(side=2, Ylab, line=3, cex=1)
      # legend(x="topleft", legend=c("All", "Significant"), text.col=c(colAll, colSig), bty="n", cex=1.25, pt.cex=0.1)
    }
  }
  mtext(side=2, "% of Lakes", outer=TRUE, line=2, cex=1.25)
  mtext(side=3, line=-2.5, outer=TRUE, "     Severity", cex=1.1)
  mtext(side=3, line=-2.5, outer=TRUE, "Magnitude                                                                          ", cex=1.1)
  mtext(side=3, line=-2.5, outer=TRUE, "                                                                                   Duration", cex=1.1)
}
dev.off()
