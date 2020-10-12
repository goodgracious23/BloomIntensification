#============================#
# B E F O R E  R U N N I N G #
#============================#
# Run analysis2_bloom_metrics_trend_analysis.R to start; 
# output feeds into this script for plotting and summary calculations
# Script created by GM Wilkinson, last updated 25 September, 2020

#======================================================
# Figure 2: Histogram of Standardized Trend Coefficients for each bloom indicator
#======================================================
# Set up 3 panel figure
tiff("Fig2_Historgram_BloomTrends.tiff",units="in",res=300,width=6.5,height=2.25)
par(mfrow=c(1,3), mai=c(0.3,0.3,0.15,0.1), omi=c(0.35,0.3,0.1,0.1))

#Color Palette
intensecol <- rgb(82,139,139, max = 255, alpha = 150)
intensecol1 <- rgb(82,139,139, max = 255, alpha = 250)
severitycol <- rgb(16,78,139, max = 255, alpha = 150)
severitycol1 <- rgb(16,78,139, max = 255, alpha = 220)
durationcol <- rgb(102,102,102, max = 255, alpha = 150)
durationcol1 <- rgb(102,102,102, max = 255, alpha = 240)

# Magnitude - trends in the mean
hist(res_avgchla.v.yr$b1, ylim=c(0,80), xlim=c(-0.4,0.4), main="Magnitude", cex.axis=1, las=2, col=c(intensecol,intensecol, intensecol, intensecol, intensecol,intensecol, intensecol, intensecol1, intensecol1,intensecol1,intensecol1, intensecol1, intensecol1, intensecol1), cex.main=1.5, font.main=1)
mtext(side=2, line=3, "Frequency", font=1)
text(-0.27, 70, "12.4%", col = intensecol, font=2, cex=1.5); text(0.27,70, "9.3%", col = intensecol1, font=2, cex=1.5)
lines(c(0,0), c(0,80), lty=2, lwd=2)

# Severity - trends in the 95th percentile
hist(res_p95chla.v.yr$b1, xlab="Standardized trend", ylim=c(0,80), xlim=c(-0.4,0.4), main="Severity", cex.axis=1, las=2, col=c(severitycol,severitycol, severitycol, severitycol, severitycol,severitycol, severitycol1, severitycol1, severitycol1,severitycol1,severitycol1, severitycol1, severitycol1, severitycol1), cex.main=1.5, font.main=1)
mtext(side=1, line=1, outer=T, "Standardized Trend Coefficient", font=1)
text(-0.27, 70, "10.2%", col = severitycol, font=2, cex=1.5); text(0.27,70, "5.0%", col = severitycol1, font=2, cex=1.5)
lines(c(0,0), c(0,80), lty=2, lwd=2)

#Duration - proportion of obs above 20 ug/L per season
hist(res_tIMug.v.yr$b1, main="Duration", cex.main=1.5, las=2, col=c(durationcol,durationcol, durationcol, durationcol, durationcol,durationcol, durationcol, durationcol, durationcol1,durationcol1,durationcol1, durationcol1, durationcol1, durationcol1), ylim=c(0,80), xlim=c(-0.1,0.1), font.main=1)
text(-0.07, 70, "9.0%", col = durationcol, font=2, cex=1.5); text(0.07,70, "3.7%", col = durationcol1, font=2, cex=1.5)
lines(c(0,0), c(0,80), lty=2, lwd=2)

dev.off()

#=======================================================================
# Calculating the Number of Significant Bloom Indicators for each lake
#=======================================================================
# The percentage of lakes that are significantly deteriorating based on at least one bloom indicator significantly increasing
intensity.pos = res_avgchla.v.yr[res_avgchla.v.yr$b1>0 & res_avgchla.v.yr$b1.p <0.05, "lagoslakeid"]
severity.pos = res_p95chla.v.yr[res_p95chla.v.yr$b1>0 & res_p95chla.v.yr$b1.p <0.05, "lagoslakeid"]
duration.pos = res_tIMug.v.yr[res_tIMug.v.yr$b1>0 & res_tIMug.v.yr$b1.p <0.05, "lagoslakeid"]
duration.pos<-duration.pos[!is.na(duration.pos)]
Deteriorating_Lakes = (length(unique(c(intensity.pos, severity.pos, duration.pos)))/length(lakes))*100

# The percentage of lakes that are significantly improving based on at least one bloom indicator significantly decreasing
intensity.neg = res_avgchla.v.yr[res_avgchla.v.yr$b1<0 & res_avgchla.v.yr$b1.p <0.05, "lagoslakeid"]
severity.neg = res_p95chla.v.yr[res_p95chla.v.yr$b1<0 & res_p95chla.v.yr$b1.p <0.05, "lagoslakeid"]
duration.neg = res_tIMug.v.yr[res_tIMug.v.yr$b1<0 & res_tIMug.v.yr$b1.p <0.05, "lagoslakeid"]
duration.neg<-duration.neg[!is.na(duration.neg)]
Improving_Lakes = (length(unique(c(intensity.neg, severity.neg, duration.neg)))/length(lakes))*100


Improving = unique(c(intensity.neg, severity.neg, duration.neg))
Deteriorating = unique(c(intensity.pos, severity.pos, duration.pos))

#==============================================
# COMPARISON TO HO & MICHALEK (2019) alpha=0.1 RESULTS
# The percentage of lakes that are significantly deteriorating based on severity indicator
severity.pos.ho = res_p95chla.v.yr[res_p95chla.v.yr$b1>0 & res_p95chla.v.yr$b1.p <0.1, "lagoslakeid"]

#===============================================================
# Assessing the number of indicators with sig trends per lake
#===============================================================
#Goal is to determine how many indicators are significantly changing for a given lake to determine if there is strong evidence (many indicators changing in the same direction) or weak evidence (only one indicator changing) for the trend in improvement or deterioration of a given lake

if (!require(plyr)) install.packages('plyr')
library(plyr)

# IMPROVING LAKES - Indicator Tally for significant trends
negsig_all = c(intensity.neg, severity.neg, duration.neg)
negsig_all = as.data.frame(negsig_all)
neg_indicator_count = count(negsig_all, 'negsig_all')

one_ind_neg <- length(neg_indicator_count[neg_indicator_count$freq==1, "freq"])/length(neg_indicator_count$negsig_all)*100
two_ind_neg <- length(neg_indicator_count[neg_indicator_count$freq==2, "freq"])/length(neg_indicator_count$negsig_all)*100
three_ind_neg <- length(neg_indicator_count[neg_indicator_count$freq==3, "freq"])/length(neg_indicator_count$negsig_all)*100

# DETERIORATING LAKES - Indicator Tally for significant trends
possig_all = c(intensity.pos, severity.pos, duration.pos)
possig_all = as.data.frame(possig_all)
pos_indicator_count = count(possig_all, 'possig_all')

one_ind_pos <- length(pos_indicator_count[pos_indicator_count$freq==1, "freq"])/length(pos_indicator_count$possig_all)*100
two_ind_pos <- length(pos_indicator_count[pos_indicator_count$freq==2, "freq"])/length(pos_indicator_count$possig_all)*100
three_ind_pos <- length(pos_indicator_count[pos_indicator_count$freq==3, "freq"])/length(pos_indicator_count$possig_all)*100



