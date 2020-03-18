rm(list=ls())

#Packages required for analysis - will install if not previously installed
if (!require(lubridate)) install.packages('lubridate')
if (!require(nlme)) install.packages('nlme')
if (!require(here)) install.packages("here")
library(lubridate)
library(nlme)
library(here)

#Read in the lake data
lakedata<-read.csv(here("chla_data.csv"), stringsAsFactors=F)
lakedata$chla<-as.numeric(lakedata$chla)
lakedata<-lakedata[!is.na(lakedata$chla),]

lakeinfo<-read.csv(here("lake_info.csv"), stringsAsFactors=F)

##-----------------------------------------------------------------------------
## helper function for finding x-coordinate of maximum of parabola
xpmax<-function(a,b){
  return(-b/(2*a))
}

##-----------------------------------------------------------------------------
## BLOOM INTENSIFICATION - Trends in mean chlorophyll a
res_avgchla.v.yr<-NULL
recovflag.mean<-NULL

lakes<-unique(lakedata$lagoslakeid)

for(ii in 1:length(lakes)){
  tmp<-lakedata[lakedata$lagoslakeid == lakes[ii],]
  tmp$year<-year(tmp$sampledate)
  tmp.agg<-aggregate(tmp$chla, by=list(tmp$year), FUN="mean", na.rm=T)
  names(tmp.agg)<-c("year","avg.chla")
  tmp.agg$year2<-tmp.agg$year^2
  tmp.agg$avg.chla<-scale(tmp.agg$avg.chla) #standardize response variable
  #plot(tmp.agg$year,tmp.agg$avg.chla, main=ii, type="b")
  lm.tmp<-gls(avg.chla ~ year, correlation=corAR1(form = ~ year), data=tmp.agg)
  qm.tmp<-try(gls(avg.chla ~ year + year2, correlation=corAR1(form = ~ year), data=tmp.agg, 
                  control=glsControl(opt="optim")))
  
  if(class(qm.tmp)!="try-error"){
    if(AIC(qm.tmp)-AIC(lm.tmp)<(-2) &
       qm.tmp$coefficients[3]<0 &
       xpmax(qm.tmp$coefficients[3],qm.tmp$coefficients[2]) > min(tmp$year) &
       xpmax(qm.tmp$coefficients[3],qm.tmp$coefficients[2]) < max(tmp$year)){recovflag.mean<-c(recovflag.mean,ii)}
  }
  
  lm.summ<-summary(lm.tmp)$tTable
  ts.length<-max(tmp.agg$year, na.rm=TRUE) - min(tmp.agg$year, na.rm=TRUE) + 1
  res_avgchla.v.yr<-rbind(res_avgchla.v.yr,
                          c(tmp$lagoslakeid[1],nrow(tmp.agg),ts.length,c(t(lm.summ))))
}

header<-c("lagoslakeid","nyr","ts.length","b0","b0.se","b0.t","b0.p","b1","b1.se","b1.t","b1.p")
colnames(res_avgchla.v.yr)<-header
res_avgchla.v.yr<-as.data.frame(res_avgchla.v.yr)
res_avgchla.v.yr$recovery.flag<-FALSE
res_avgchla.v.yr$recovery.flag[recovflag.mean]<-TRUE

# How many lakes showing recovery after initially getting worse?
length(recovflag.mean)

# Summary of Trends in Intensification (mean chlorophyll a)
summary(res_avgchla.v.yr$b1)
sum(res_avgchla.v.yr$b1>0)/length(lakes) #Percent of all lakes with positive trends (regardless of significance)
sum(res_avgchla.v.yr$b1>0 & res_avgchla.v.yr$b1.p <0.05)/length(lakes) #Percent of lakes with significant positive trends
sum(res_avgchla.v.yr$b1<0 & res_avgchla.v.yr$b1.p <0.05)/length(lakes) #Percent of lakes with significant negative trends

##-----------------------------------------------------------------------------
## BLOOM SEVERITY - Trends in the 95th percentile of chlorophyll a

res_p95chla.v.yr<-NULL
recovflag.p95<-NULL

for(ii in 1:length(lakes)){
  tmp<-lakedata[lakedata$lagoslakeid == lakes[ii],]
  tmp$year<-year(tmp$sampledate)
  tmp.agg<-aggregate(tmp$chla, by=list(tmp$year), FUN="quantile", probs=0.95, na.rm=T)
  names(tmp.agg)<-c("year","p95.chla")
  tmp.agg$year2<-tmp.agg$year^2
  tmp.agg$p95.chla<-scale(tmp.agg$p95.chla)
  
  lm.tmp<-try(gls(p95.chla ~ year, correlation=corAR1(form = ~ year), data=tmp.agg))
  qm.tmp<-try(gls(p95.chla ~ year + year2, correlation=corAR1(form = ~ year), data=tmp.agg))
  
  if(class(lm.tmp)=="try-error"){next}
  
  if(class(qm.tmp)!="try-error"){
    if(AIC(qm.tmp)-AIC(lm.tmp)<(-2) &
       qm.tmp$coefficients[3]<0 &
       xpmax(qm.tmp$coefficients[3],qm.tmp$coefficients[2]) > min(tmp$year) &
       xpmax(qm.tmp$coefficients[3],qm.tmp$coefficients[2]) < max(tmp$year)){recovflag.mean<-c(recovflag.mean,ii)}
  }
  
  lm.summ<-summary(lm.tmp)$tTable
  ts.length<-max(tmp.agg$year, na.rm=TRUE) - min(tmp.agg$year, na.rm=TRUE) + 1
  res_p95chla.v.yr<-rbind(res_p95chla.v.yr,
                          c(tmp$lagoslakeid[1],nrow(tmp.agg),ts.length,c(t(lm.summ))))
}

header<-c("lagoslakeid","nyr","ts.length","b0","b0.se","b0.t","b0.p","b1","b1.se","b1.t","b1.p")
colnames(res_p95chla.v.yr)<-header
res_p95chla.v.yr<-as.data.frame(res_p95chla.v.yr)
res_p95chla.v.yr$recovery.flag<-FALSE
res_p95chla.v.yr$recovery.flag[recovflag.p95]<-TRUE

# How many lakes showing recovery after initially getting worse?
length(recovflag.p95) 

#Summary of Trends in Severity (95th percentile of chlorophyll a)
summary(res_p95chla.v.yr$b1)
sum(res_p95chla.v.yr$b1>0)/length(lakes) #Percent of lakes with a positive trend (regardless of significance)
sum(res_p95chla.v.yr$b1>0 & res_p95chla.v.yr$b1.p <0.05)/length(lakes) #Perecent of lakes with significant positive trends
sum(res_p95chla.v.yr$b1<0 & res_p95chla.v.yr$b1.p <0.05)/length(lakes) #Percent of lakes with significant negative trends

##-----------------------------------------------------------------------------
## BLOOM DURATION - Trends in the number of observvations per year greater than a chlorophyll a threshold
## Threshold values from Angradi et al. (2018) https://doi.org/10.1016/j.ecolind.2018.06.001

thresh1<-11#ug/L; threshold for an impaired lake in the mountains/upper mid west
thresh2<-40#ug/L; threshold for an impaired lake in the plains
res_tIMug.v.yr<-NULL
recovflag.tIMug<-NULL

for(ii in 1:length(lakes)){
  tmp<-lakedata[lakedata$lagoslakeid == lakes[ii],]
  tmp$year<-year(tmp$sampledate)
  tmp.agg<-data.frame(year=unique(tmp$year),tIMug=NA)
  for(yy in 1:length(tmp.agg$year)){
    tmp.yy<-tmp[tmp$year == tmp.agg$year[yy],]
    if(lakeinfo$class[ii]=="mountains"){
      tmp.agg$tIMug[yy]<-sum(tmp.yy$chla > thresh1)/nrow(tmp.yy)
    }
    if(lakeinfo$class[ii]=="plains"){
      tmp.agg$tIMug[yy]<-sum(tmp.yy$chla > thresh2)/nrow(tmp.yy)
    }
  }
  tmp.agg$year2<-tmp.agg$year^2
  
  if(var(tmp.agg$tIMug)==0){
    ts.length<-max(tmp.agg$year, na.rm=TRUE) - min(tmp.agg$year, na.rm=TRUE) + 1
    res_tIMug.v.yr<-rbind(res_tIMug.v.yr,
                          c(tmp$lagoslakeid[1],nrow(tmp.agg),ts.length,rep(NA,8)))
    next
  }
  
  lm.tmp<-try(gls(tIMug ~ year, correlation=corAR1(form = ~ year), data=tmp.agg))
  qm.tmp<-try(gls(tIMug ~ year + year2, correlation=corAR1(form = ~ year), data=tmp.agg))
  
  if(class(lm.tmp)=="try-error"){next}
  
  if(class(qm.tmp)!="try-error"){
    if(AIC(qm.tmp)-AIC(lm.tmp)<(-2) &
       qm.tmp$coefficients[3]<0 &
       xpmax(qm.tmp$coefficients[3],qm.tmp$coefficients[2]) > min(tmp$year) &
       xpmax(qm.tmp$coefficients[3],qm.tmp$coefficients[2]) < max(tmp$year)){recovflag.mean<-c(recovflag.mean,ii)}
  }
  lm.summ<-summary(lm.tmp)$tTable
  
  ts.length<-max(tmp.agg$year, na.rm=TRUE) - min(tmp.agg$year, na.rm=TRUE) + 1
  res_tIMug.v.yr<-rbind(res_tIMug.v.yr,
                        c(tmp$lagoslakeid[1],nrow(tmp.agg),ts.length,c(t(lm.summ))))
  
}
header<-c("lagoslakeid","nyr","ts.length","b0","b0.se","b0.t","b0.p","b1","b1.se","b1.t","b1.p")
colnames(res_tIMug.v.yr)<-header
res_tIMug.v.yr<-as.data.frame(res_tIMug.v.yr)
res_tIMug.v.yr$recovery.flag<-FALSE
res_tIMug.v.yr$recovery.flag[recovflag.tIMug]<-TRUE

# How many lakes showing recovery after initially getting worse?
length(recovflag.tIMug)

#Summary of trends in bloom duration (observations above a threshold)
summary(res_tIMug.v.yr$b1)
sum(res_tIMug.v.yr$b1>0, na.rm=T)/length(lakes) #Percent of lakes with positive trend (regardless of significance)
sum(res_tIMug.v.yr$b1>0 & res_tIMug.v.yr$b1.p <0.05, na.rm=T)/length(lakes) #Percent of lakes with significant positive trend
sum(res_tIMug.v.yr$b1<0 & res_tIMug.v.yr$b1.p <0.05, na.rm=T)/length(lakes) #Percent of lakes with significant negative trend

##========================================================
## SUMMARY OF BLOOM METRICS TRENDS

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

# FINAL ANSWER - Overall number of lakes deteriorating or improving
print(c("% Deteriorating Overall =", round(Deteriorating_Lakes, digits=1)))# The percentage of lakes that are significantly deteriorating
print(c("% Improving Overall =", round(Improving_Lakes, digits=1)))# The percentage of lakes that are significantly improving, overall


# Figure 2: Histogram of Standardized Trend Coefficients ------------------------------------------

# Set up 3 panel figure
pdf(file=here("fig1_trend_histogram.pdf"), height=2.5, width=7)
par(mfrow=c(1,3), mai=c(0.3,0.3,0.15,0.1), omi=c(0.35,0.3,0.2,0.1))

#Color Palette
intensecol <- rgb(82,139,139, max = 255, alpha = 100)
intensecol1 <- rgb(82,139,139, max = 255, alpha = 250)
severitycol <- rgb(16,78,139, max = 255, alpha = 100)
severitycol1 <- rgb(16,78,139, max = 255, alpha = 220)
durationcol <- rgb(102,102,102, max = 255, alpha = 100)
durationcol1 <- rgb(102,102,102, max = 255, alpha = 240)

# Intensity - trends in the mean
hist(res_avgchla.v.yr$b1, ylim=c(0,80), xlim=c(-0.4,0.4), main="Intensity", cex.axis=1, las=2, col=c(intensecol,intensecol, intensecol, intensecol, intensecol,intensecol, intensecol, intensecol1, intensecol1,intensecol1,intensecol1, intensecol1, intensecol1, intensecol1), cex.main=1.5, font.main=1)
mtext(side=2, line=3, "Frequency", font=1)

# Severity - trends in the 95th percentile
hist(res_p95chla.v.yr$b1, xlab="Standardized trend", ylim=c(0,80), xlim=c(-0.4,0.4), main="Severity", cex.axis=1, las=2, col=c(severitycol,severitycol, severitycol, severitycol, severitycol,severitycol, severitycol1, severitycol1, severitycol1,severitycol1,severitycol1, severitycol1, severitycol1, severitycol1), cex.main=1.5, font.main=1)
mtext(side=1, line=1, outer=T, "Standardized Trend Coefficient", font=1)

#Duration - proportion of obs above 20 ug/L per season
hist(res_tIMug.v.yr$b1, main="Duration", cex.main=1.5, las=2, col=c(durationcol,durationcol, durationcol, durationcol, durationcol,durationcol, durationcol, durationcol, durationcol1,durationcol1,durationcol1, durationcol1, durationcol1, durationcol1), ylim=c(0,80), xlim=c(-0.1,0.1), font.main=1)

dev.off()

## Export results coefficient tables --------------------------------------------------------------
write.csv(res_avgchla.v.yr, file=here("results_AvgChl_vs_Year_gls.csv"), row.names=FALSE)
write.csv(res_p95chla.v.yr, file=here("results_p95Chl_vs_Year_gls.csv"), row.names=FALSE)
write.csv(res_tIMug.v.yr, file=here("results_tIMug_vs_Year_gls.csv"), row.names=FALSE)
