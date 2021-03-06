# Figure 1 - Exemplar Time Series
# Wilkinson et al. FE&E
# Written by GM Wilkinson June 2020
# Most recent update: 25 Sept 2020


#Packages required for analysis - will install if not previously installed
if (!require(lubridate)) install.packages('lubridate')
if (!require(nlme)) install.packages('nlme')
if (!require(here)) install.packages("here")
library(lubridate)
library(nlme)
library(here)

#Read in the lake data
lakedata<-read.csv(here("hab_trend_data_filtered_20200203.csv"), stringsAsFactors=F)
lakedata$chla<-as.numeric(lakedata$chla)
lakedata<-lakedata[!is.na(lakedata$chla),]
lakedata$sampledate<- mdy(lakedata$sampledate)
lakedata$year<- year(lakedata$sampledate)
lakedata$DOY<- yday(lakedata$sampledate)
lakedata$YearFrac<- lakedata$year + (lakedata$DOY/365)

lakeinfo<-read.csv(here("lake_info.csv"), stringsAsFactors=F)
lakes<-unique(lakedata$lagoslakeid)

#Colors for the plot
durationcol <- rgb(102,102,102, max = 255, alpha = 200)
chlcol <- rgb(127, 127, 127, max = 255, alpha = 40)
severitycol <- rgb(16,78,139, max = 255, alpha = 200)
intensecol <- rgb(0,139,139, max = 255, alpha = 200)

# Begin Figure Construction
tiff("Fig1_ExemplarTimeSeries.tiff",units="in",res=300,width=6.5,height=5)
par(mfrow=c(2,1), omi=c(0.25,0.5,0.25,0.5), mai=c(0.5,0.5,0,0.5))

#===================================#
# TUCKER POND, RHODE ISLAND - #8272 #
#===================================#
#Plot the raw chlorophyll a time series, make it pretty
plot(lakedata[lakedata$lagoslakeid==8272, "YearFrac"], lakedata[lakedata$lagoslakeid==8272, "chla"], pch=16, cex=1.5, col=chlcol, ylim=c(0,50), xlab="", ylab="", las=2, xaxt="n")
axis(side=1, at=pretty(range(lakedata[lakedata$lagoslakeid==8272, "YearFrac"])))
mtext(side=2, line=3, expression(Chlorophyll-a~"("*mu*g~L^-1*")"))

#Intensity Time Series - mean chlorophyll-a by year
intensity.agg<-aggregate(lakedata[lakedata$lagoslakeid==8272, "chla"], by=list(lakedata[lakedata$lagoslakeid==8272, "year"]), FUN="mean", na.rm=T)
lines(intensity.agg, col=intensecol, lwd=3, type = "l")

#Severity Time Series - 95th percentile chlorophyll-a by year
severity.agg<-aggregate(lakedata[lakedata$lagoslakeid==8272, "chla"], by=list(lakedata[lakedata$lagoslakeid==8272, "year"]), FUN="quantile", probs=0.95, na.rm=T)
lines(severity.agg, col=severitycol, lwd=3)
legend("top", legend=c("Magnitude", "Severity", "Duration"), col=c(intensecol, severitycol, durationcol), lwd=3, ncol=3, bty="n")

#Duration Time Series- # of observations >11 ug/L per year
duration.agg<-aggregate(lakedata[lakedata$lagoslakeid==8272, "chla"]>11, by=list(lakedata[lakedata$lagoslakeid==8272, "year"]), FUN="sum", na.rm=T)
duration.sample<-aggregate(lakedata[lakedata$lagoslakeid==8272, "chla"]>0, by=list(lakedata[lakedata$lagoslakeid==8272, "year"]), FUN="sum", na.rm=T)

par(new = TRUE)
plot(duration.agg$Group.1, (duration.agg$x/duration.sample$x)*100, ylim=c(0,100), col=durationcol, lwd=3, type="l", axes = FALSE, bty = "n", xlab = "", ylab = "")
mtext(side=4, line=3, "% Observations", col=durationcol, font=2)
axis(side=4, at = c(0,20,40,60,80,100), labels=c("0", "20", "40", "60", "80", "100"), las=2)

#===============================#
# WIRTH LAKE, MINNESOTA - #2424 #
#===============================#
#Plot the raw chlorophyll a time series, make it pretty
plot(lakedata[lakedata$lagoslakeid==2424, "YearFrac"], lakedata[lakedata$lagoslakeid==2424, "chla"], pch=16, cex=1.5, col=chlcol, ylim=c(0,100), xlab="", ylab="", las=2, xaxt="n")
axis(side=1, at=pretty(range(lakedata[lakedata$lagoslakeid==2424, "YearFrac"])))
mtext(side=2, line=3, expression(Chlorophyll-a~"("*mu*g~L^-1*")"))

#Intensity Time Series - mean chlorophyll-a by year
intensity.agg<-aggregate(lakedata[lakedata$lagoslakeid==2424, "chla"], by=list(lakedata[lakedata$lagoslakeid==2424, "year"]), FUN="mean", na.rm=T)
lines(intensity.agg, col=intensecol, lwd=3, type = "l")

#Severity Time Series - 95th percentile chlorophyll-a by year
severity.agg<-aggregate(lakedata[lakedata$lagoslakeid==2424, "chla"], by=list(lakedata[lakedata$lagoslakeid==2424, "year"]), FUN="quantile", probs=0.95, na.rm=T)
lines(severity.agg, col=severitycol, lwd=3)

#Duration Time Series- # of observations >11 ug/L per year
duration.agg<-aggregate(lakedata[lakedata$lagoslakeid==2424, "chla"]>11, by=list(lakedata[lakedata$lagoslakeid==2424, "year"]), FUN="sum", na.rm=T)
duration.sample<-aggregate(lakedata[lakedata$lagoslakeid==2424, "chla"]>0, by=list(lakedata[lakedata$lagoslakeid==2424, "year"]), FUN="sum", na.rm=T)

par(new = TRUE)
plot(duration.agg$Group.1, (duration.agg$x/duration.sample$x)*100, ylim=c(0,100), col=durationcol, lwd=3, type="l", axes = FALSE, bty = "n", xlab = "", ylab = "")
mtext(side=4, line=3, "% Observations", col=durationcol, font=2)
axis(side=4, at = c(0,20,40,60,80,100), labels=c("0", "20", "40", "60", "80", "100"), las=2)

dev.off() #End Figure Construction