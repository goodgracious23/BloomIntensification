# Analyze algal bloom trends in lakes
# script written by Jon Walter
# NOTE: Can use "hab_trend_data_filtered_20200203.csv" in repository to run script

#Packages required for analysis - will install if not previously installed
if (!require(lubridate)) install.packages('lubridate')
if (!require(nlme)) install.packages('nlme')
library(lubridate)
library(nlme)


lakedata<-read.csv("hab_trend_data_filtered_20200203.csv", stringsAsFactors=F)
lakedata$chla<-as.numeric(lakedata$chla)
lakedata<-lakedata[!is.na(lakedata$chla),]
lakedata$sampledate<-as.POSIXct(lakedata$sampledate, format="%m/%d/%Y")
lakeinfo<-read.csv("lake_info.csv", stringsAsFactors=F)

##-----------------------------------------------------------------------------
## helper function for finding x-coordinate of maximum of parabola
xpmax<-function(a,b){
  return(-b/(2*a))
}

##-----------------------------------------------------------------------------
## MAGNITUDE: are there trends in annual mean chla?
res_avgchla.v.yr<-NULL
recovflag.mean<-NULL

lakes<-unique(lakedata$lagoslakeid)
quadflag.mean<-rep(NA, length(lakes))


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
    if(AIC(qm.tmp)-AIC(lm.tmp)<(-2)){
      quadflag.mean[ii]<-AIC(qm.temp)-AIC(lm.tmp)
    }
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

length(recovflag.mean) #0 lakes show evidence of recovery after increase 

##-----------------------------------------------------------------------------
## SEVERITY: are there trends in 95th percentile of chla?
res_p95chla.v.yr<-NULL
recovflag.p95<-NULL
quadflag.p95<-rep(NA, length(lakes))

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
    if(AIC(qm.tmp)-AIC(lm.tmp)<(-2)){
      quadflag.p95[ii]<-AIC(qm.temp)-AIC(lm.tmp)
    }
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
length(recovflag.p95) #0 lakes show evidence of recovery following eutrophication


##-----------------------------------------------------------------------------
## DURATION: Are there trends in proportion of obs with impaired function?
thresh1<-11#ug/L; threshold for an impaired lake in the mountains/upper mid west
thresh2<-40#ug/L; threshold for an impaired lake in the plains
res_tIMug.v.yr<-NULL
recovflag.tIMug<-NULL
quadflag.tIMug<-rep(NA, length(lakes))

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
    if(AIC(qm.tmp)-AIC(lm.tmp)<(-2)){
      quadflag.tIMug[ii]<-AIC(qm.temp)-AIC(lm.tmp)
    }
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
length(recovflag.tIMug) 


## Export all results coef tables ---------------------------------------------
write.csv(res_avgchla.v.yr, file="results_AvgChl_vs_Year_gls.csv", row.names=FALSE)
write.csv(res_p95chla.v.yr, file="results_p95Chl_vs_Year_gls.csv", row.names=FALSE)
write.csv(res_tIMug.v.yr, file="results_tIMug_vs_Year_gls.csv", row.names=FALSE)

## Export lakes that "recover" ------------------------------------------------
recov.avg<-res_avgchla.v.yr[res_avgchla.v.yr$b1<0 & res_avgchla.v.yr$b1.p < 0.05,]
recov.p95<-res_p95chla.v.yr[res_p95chla.v.yr$b1<0 & res_p95chla.v.yr$b1.p < 0.05,]
recov.tIMug<-res_tIMug.v.yr[res_tIMug.v.yr$b1<0 & res_tIMug.v.yr$b1.p < 0.05,]
