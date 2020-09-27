# Do lake morphology, nutrients, or climate intensification explain variability in HAB trends?
# Script written by Jon Walter - 28 August 2020

#NOTE: must run 'analysis2_bloom_metrics_trend_analysis.R' for results data files


if (!require(dplyr)) install.packages('dplyr')
if (!require(ncf)) install.packages('ncf')
if (!require(lme4)) install.packages('lme4')
if (!require(car)) install.packages('car')
if (!require(lmerTest)) install.packages('lmerTest')
if (!require(RColorBrewer)) install.packages('RColorBrewer')
if (!require(reghelper)) install.packages('reghelper')

library(dplyr)
library(ncf)
library(lme4)
library(car)
library(lmerTest)
library(MuMIn)
library(RColorBrewer)
library(reghelper)

covars<-read.csv("lakecovars_20200622.csv", stringsAsFactors=F)
covars$tsi.cat[covars$tsi.cat=="hypereutrophic"]<-"eutrophic"
res.avg.chla<-read.csv("results_AvgChl_vs_Year_gls.csv", stringsAsFactors=F)
res.p95.chla<-read.csv("results_p95Chl_vs_Year_gls.csv", stringsAsFactors=F)
res.tIMug<-read.csv("results_tIMug_vs_Year_gls.csv", stringsAsFactors=F)

#Investigate missingness in covariations --------------------------------------
sum(is.na(covars$dr)) #27
sum(is.na(covars$maxdepth)) #37
sum(is.na(covars$lake_area_ha)) #24
sum(is.na(covars$tn.trend)) #141
sum(is.na(covars$tp.trend)) #130
sum(is.na(covars$np)) #145
sum(is.na(covars$wetland_pct)) #14
sum(is.na(covars$agri_pct)) #14
sum(is.na(covars$devel_pct)) #14

covars$east_west<-ifelse(covars$nhd_long > -82,"E","W")

#Scale numeric covariates ----------------------------------------------------
# covars$lake_area_ha<-scale(log10(covars$lake_area_ha))
# covars$maxdepth<-scale(log10(covars$maxdepth))
# covars$dr<-scale(log10(covars$dr))
# covars$tn.trend<-scale(covars$tn.trend)
# covars$tp.trend<-scale(covars$tp.trend)
# covars$np<-scale(covars$np)
# covars$trend.pptp95<-scale(covars$trend.pptp95)
# covars$trend.tmaxmean<-scale(covars$trend.tmaxmean)
# covars$agri_pct<-scale(covars$agri_pct)
# covars$devel_pct<-scale(covars$devel_pct)
# covars$wetland_pct<-scale(covars$wetland_pct)

covars$lake_area_ha<-log10(covars$lake_area_ha)
covars$maxdepth<-log10(covars$maxdepth)
covars$dr<-log10(covars$dr)

rsq<-function(emp,pred){
  sstot<-sum((emp-mean(emp))^2)
  ssres<-sum((pred-emp)^2)
  return(1-(ssres/sstot))
}

usecov<-c("lake_area_ha"
          ,"maxdepth"
          ,"dr"
          ,"tn.trend"
          ,"tp.trend"
          ,"np"
          ,"trend.pptp95"
          ,"trend.tmaxmean"
          ,"agri_pct"
          ,"devel_pct"
          ,"wetland_pct"
          ,"LII_ecoregion"
          ,"chla_mean"
          ,"b1"
          ,"nhd_lat"
          ,"nhd_long"
          #,"east_west"
          )

## variable importance function
varimp<-function(dredge.obj, varcols=2:18, sort=T){
  
  vars<-colnames(dredge.obj)[varcols]
  varimp<-NULL
  
  for(ii in 2:18){
    varimp <- c(varimp, sum(dredge.obj$weight[!is.na(dredge.obj[,ii])]))
  }
  out<-data.frame(varname=vars,importance=varimp)
  
  if(sort){
    out<-out[order(out$importance, decreasing=T),]
  }
  
  return(out)
}

# MODELS FOR MAGNITUDE METRIC ---------------------------------------------------------
dat.avg.chla<-inner_join(covars, res.avg.chla)
dat.avg.chla<-dat.avg.chla[!dat.avg.chla$recovery.flag,]
dat.avg.chla<-dat.avg.chla[,names(dat.avg.chla) %in% usecov]
dat.avg.chla<-dat.avg.chla[complete.cases(dat.avg.chla),]
dat.avg.chla<-dat.avg.chla[dat.avg.chla$np < 500,]

table(dat.avg.chla$LII_ecoregion)
dat.avg.chla$LII_ecoregion[dat.avg.chla$LII_ecoregion==5.2]<-5.3
dat.avg.chla$LII_ecoregion[dat.avg.chla$LII_ecoregion==8.5]<-8.4
dat.avg.chla<-dat.avg.chla[dat.avg.chla$LII_ecoregion != 9.2,]


lm.avg.full<-lmerTest::lmer(b1 ~ 
                              lake_area_ha*chla_mean + 
                              maxdepth*chla_mean + 
                              dr*chla_mean + 
                              chla_mean + 
                              tn.trend*chla_mean + 
                              tp.trend*chla_mean + 
                              np*chla_mean + 
                              trend.pptp95*chla_mean + 
                              trend.tmaxmean*chla_mean + 
                              (1|LII_ecoregion), 
                            data=dat.avg.chla, na.action="na.fail", REML=F)
summary(lm.avg.full)
AIC(lm.avg.full)
rsq(dat.avg.chla$b1, predict(lm.avg.full))
# dredge.lm.avg<-dredge(lm.avg.full, evaluate=T) ##Note: takes a long time to run
# imp.lm.avg<-varimp(dredge.lm.avg)


lm.avg.best<-lmerTest::lmer(b1 ~ 
                              chla_mean + 
                              lake_area_ha + 
                              trend.pptp95 + 
                              chla_mean:trend.pptp95 + 
                              (1|LII_ecoregion), 
                            data=dat.avg.chla, REML=F)
summary(lm.avg.best)
rsq(dat.avg.chla$b1, predict(lm.avg.best))

lm.avg.null<-lm(b1 ~ 
                  chla_mean + 
                  lake_area_ha + 
                  trend.pptp95 + 
                  chla_mean:trend.pptp95, 
                data=dat.avg.chla)
anova(lm.avg.best, lm.avg.null)

coef.avg<-coef(lm.avg.best)$LII_ecoregion
#remove effects of np for plotting against chla
pr.avg<-data.frame(b1=dat.avg.chla$b1
                         ,LII_ecoregion=dat.avg.chla$LII_ecoregion
                         ,chla_mean=dat.avg.chla$chla_mean
                         ,lake_area_ha=dat.avg.chla$lake_area_ha
                         ,trend.pptp95=dat.avg.chla$trend.pptp95
                         #,intercept=rep(NA, length(dat.avg.chla$b1))
                         ,coef.chla=rep(coef.avg[1,2], length(dat.avg.chla$b1))
                         ,coef.lake_area=rep(coef.avg[1,3], length(dat.avg.chla$b1))
                         ,coef.trend.pptp95=rep(coef.avg[1,4], length(dat.avg.chla$b1))
                         ,coef.interaction=rep(coef.avg[1,5], length(dat.avg.chla$b1))
                         )

pr.avg$b1.chla<-(pr.avg$b1 - pr.avg$lake_area_ha*pr.avg$coef.lake_area
                     - pr.avg$trend.pptp95*pr.avg$coef.trend.pptp95
                     - pr.avg$lake_area_ha*pr.avg$trend.pptp95*pr.avg$coef.interaction)

pr.avg$b1.lake_area<-(pr.avg$b1 - pr.avg$chla_mean*pr.avg$coef.chla
                 - pr.avg$trend.pptp95*pr.avg$coef.trend.pptp95
                 - pr.avg$lake_area_ha*pr.avg$trend.pptp95*pr.avg$coef.interaction)

pr.avg$b1.ppttrend<-(pr.avg$b1 - pr.avg$chla_mean*pr.avg$coef.chla
                     - pr.avg$lake_area_ha*pr.avg$coef.lake_area
                     - pr.avg$lake_area_ha*pr.avg$trend.pptp95*pr.avg$coef.interaction)

print(simple_slopes(lm.avg.best, levels=list(trend.pptp95 = c(-0.05,0,0.05))))


# MODELS FOR SEVERITY METRIC ---------------------------------------------------------------------------
dat.p95.chla<-inner_join(covars, res.p95.chla)
dat.p95.chla<-dat.p95.chla[!dat.p95.chla$recovery.flag,]
dat.p95.chla<-dat.p95.chla[,names(dat.p95.chla) %in% usecov]
dat.p95.chla<-dat.p95.chla[complete.cases(dat.p95.chla),]
dat.p95.chla<-dat.p95.chla[dat.p95.chla$np < 500,]

table(dat.p95.chla$LII_ecoregion)
dat.p95.chla$LII_ecoregion[dat.p95.chla$LII_ecoregion==5.2]<-5.3
dat.p95.chla$LII_ecoregion[dat.p95.chla$LII_ecoregion==8.5]<-8.4
dat.p95.chla<-dat.p95.chla[dat.p95.chla$LII_ecoregion != 9.2,]

lm.p95.full<-lmerTest::lmer(b1 ~ 
                              lake_area_ha*chla_mean + 
                              maxdepth*chla_mean + 
                              dr*chla_mean + 
                              chla_mean + 
                              tn.trend*chla_mean + 
                              tp.trend*chla_mean + 
                              np*chla_mean + 
                              trend.pptp95*chla_mean + 
                              trend.tmaxmean*chla_mean + 
                              (1|LII_ecoregion), 
                            data=dat.p95.chla, na.action="na.fail", REML=F)
summary(lm.p95.full)
AIC(lm.p95.full)
rsq(dat.p95.chla$b1, predict(lm.p95.full))
# dredge.lm.p95<-dredge(lm.p95.full, evaluate=T) ##Note: takes a long time to run
# imp.lm.p95<-varimp(dredge.lm.p95)

lm.p95.best<-lmerTest::lmer(b1 ~ 
                              chla_mean + 
                              lake_area_ha + 
                              (1|LII_ecoregion), 
                            data=dat.p95.chla, REML=F)
summary(lm.p95.best)
rsq(dat.p95.chla$b1, predict(lm.p95.best))

lm.p95.null<-lm(b1 ~ 
                  chla_mean + 
                  lake_area_ha, 
                data=dat.p95.chla)
anova(lm.p95.best,lm.p95.null)

coef.p95<-coef(lm.p95.best)$LII_ecoregion

pr.p95<-data.frame(b1=dat.p95.chla$b1
                   ,LII_ecoregion=dat.p95.chla$LII_ecoregion
                   ,chla_mean=dat.p95.chla$chla_mean
                   ,lake_area_ha=dat.p95.chla$lake_area_ha
                   ,coef.chla=rep(coef.p95[1,2], length(dat.p95.chla$b1))
                   ,coef.lake_area=rep(coef.p95[1,3], length(dat.p95.chla$b1))
)

pr.p95$b1.chla<-pr.p95$b1 - pr.p95$lake_area_ha*pr.p95$coef.lake_area
pr.p95$b1.lake_area<-pr.p95$b1 - pr.p95$chla_mean*pr.p95$coef.chla


# MODELS FOR DURATION BLOOM METRIC -----------------------------------------------------
dat.imp.chla<-inner_join(covars, res.tIMug)
dat.imp.chla<-dat.imp.chla[!dat.imp.chla$recovery.flag,]
dat.imp.chla<-dat.imp.chla[,names(dat.imp.chla) %in% usecov]
dat.imp.chla<-dat.imp.chla[complete.cases(dat.imp.chla),]
dat.imp.chla<-dat.imp.chla[dat.imp.chla$np < 500,]

table(dat.imp.chla$LII_ecoregion)
dat.imp.chla$LII_ecoregion[dat.imp.chla$LII_ecoregion==5.2]<-5.3
dat.imp.chla$LII_ecoregion[dat.imp.chla$LII_ecoregion==8.5]<-8.4
dat.imp.chla<-dat.imp.chla[dat.imp.chla$LII_ecoregion != 9.2,]


lm.imp.full<-lmerTest::lmer(b1 ~ lake_area_ha*chla_mean + 
                              maxdepth*chla_mean + 
                              dr*chla_mean + 
                              chla_mean + 
                              tn.trend*chla_mean + 
                              tp.trend*chla_mean + 
                              np*chla_mean + 
                              trend.pptp95*chla_mean + 
                              trend.tmaxmean*chla_mean + 
                              (1|LII_ecoregion), 
                            data=dat.imp.chla, na.action="na.fail", REML=F)
summary(lm.imp.full)
AIC(lm.imp.full)
rsq(dat.imp.chla$b1, predict(lm.imp.full))
# dredge.lm.imp<-dredge(lm.imp.full, evaluate=T) ##Note: takes a long time to run
# imp.lm.imp<-varimp(dredge.lm.imp)

lm.imp.best<-lmerTest::lmer(b1 ~ 
                              maxdepth + 
                              tn.trend + 
                              (1|LII_ecoregion), 
                            data=dat.imp.chla, REML=F)
summary(lm.imp.best)
rsq(dat.imp.chla$b1, predict(lm.imp.best))

lm.imp.null<-lm(b1 ~ maxdepth + tn.trend, data=dat.imp.chla)
anova(lm.imp.best, lm.imp.null)
