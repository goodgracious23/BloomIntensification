## Make dataset of covariates for HAB trends analysis

# - hydrologic connection type (source: LAGOS-NE)
# - mean TN (source: LAGOS-NE)
# - mean TP (source: LAGOS-NE)
# - color (source: LAGOS-NE)
# - N:P molar ratio (source: LAGOS-NE)
# - Morphometry (depth, surface area) (source: LAGOS-NE)
# - Precipitation intensitification (source: PRISM)
# - Temperature trends (source: PRISM)

## ADD: Trophic state (based on chla), HUC-4 Zones, Restoration Status


rm(list=ls())

library(rgdal)
library(raster)
library(LAGOSNE)
library(lubridate)
library(dplyr)
library(sp)
library(spatialEco)
library(nlme)

setwd("~/Box Sync/Intense HABs in Small Lakes/HAB Trends/Analyses")

lakeinfo<-read.csv("hab_trend_lakeinfo_20200203.csv", stringsAsFactors=F)
sampfreq<-read.csv("sampfreq_goodlakes_20200203.csv")

hu4zones<-readOGR("HU4.shp")
ecoregions<-readOGR("NA_CEC_Eco_Level2.shp")

dt<-lagosne_load(version="1.087.3")

## Prep Iowa Data ---------------------------------------------------------------------------------

ia_covar<-read.csv("../LAGOS Extended/Iowa/Iowa_Covariates.csv", stringsAsFactors = F)
ia_covar$lagoslakeid<-rep(NA, nrow(ia_covar))
ia_covar$lagoslakeid[ia_covar$LakeName=="Silver Lake"]<- -44
ia_covar$lagoslakeid[ia_covar$LakeName=="Center Lake"]<- -55
ia_covar$lagoslakeid[ia_covar$LakeName=="Big Spirit Lake"]<- -66
ia_covar$lagoslakeid[ia_covar$LakeName=="Little Spirit Lake"]<- -77
ia_covar$lagoslakeid[ia_covar$LakeName=="Upper Gar Lake"]<- -88
ia_covar$lagoslakeid[ia_covar$LakeName=="Minnewashta Lake"]<- -99
ia_covar$lagoslakeid[ia_covar$LakeName=="Lower Gar Lake"]<- -1010
ia_covar$lagoslakeid[ia_covar$LakeName=="West Lake Okoboji"]<- -1111
ia_covar$lagoslakeid[ia_covar$LakeName=="East Lake Okoboji"]<- -1212
ia_covar$lagoslakeid[ia_covar$LakeName=="Coralville Reservoir"]<- -11
ia_covar$lagoslakeid[ia_covar$LakeName=="Red Rock Reservoir"]<- -22
ia_covar$lagoslakeid[ia_covar$LakeName=="Saylorville Reservoir"]<- -33


clamp<-read.csv("../LAGOS Extended/Iowa/Iowa_CLAMP_Lakes.csv", stringsAsFactors = F)
clamp$Sample_Date<-as.POSIXct(clamp$Sample_Date, format="%m/%d/%Y")
clamp<-clamp[month(clamp$Sample_Date) %in% c(5,6,7,8,9),]
clamp$lagoslakeid<-rep(NA, nrow(clamp))
clamp$lagoslakeid[clamp$LakeName=="Silver Lake"]<- -44
clamp$lagoslakeid[clamp$LakeName=="Center Lake"]<- -55
clamp$lagoslakeid[clamp$LakeName=="Big Spirit Lake"]<- -66
clamp$lagoslakeid[clamp$LakeName=="Little Spirit Lake"]<- -77
clamp$lagoslakeid[clamp$LakeName=="Upper Gar Lake"]<- -88
clamp$lagoslakeid[clamp$LakeName=="Minnewashta"]<- -99
clamp$lagoslakeid[clamp$LakeName=="Lower Gar Lake"]<- -1010
clamp$lagoslakeid[clamp$LakeName=="West Okoboji"]<- -1111
clamp$lagoslakeid[clamp$LakeName=="East Okoboji"]<- -1212
clamp$TN_ugL<-clamp$TN_mgL*1000

ace_tntp<-read.csv("../LAGOS Extended/Iowa/Iowa_ACE_TNTP.csv", stringsAsFactors = F)

baddate<-simplify2array(strsplit(ace_tntp$Date,split="-"))
baddate[3,as.numeric(baddate[3,])>70]<-paste0("19",baddate[3,as.numeric(baddate[3,])>70])
baddate[3,as.numeric(baddate[3,])<70]<-paste0("20",baddate[3,as.numeric(baddate[3,])<70])
baddate[2,baddate[2,]=="Jan"]<-"01"
baddate[2,baddate[2,]=="Feb"]<-"02"
baddate[2,baddate[2,]=="Mar"]<-"03"
baddate[2,baddate[2,]=="Apr"]<-"04"
baddate[2,baddate[2,]=="May"]<-"05"
baddate[2,baddate[2,]=="Jun"]<-"06"
baddate[2,baddate[2,]=="Jul"]<-"07"
baddate[2,baddate[2,]=="Aug"]<-"08"
baddate[2,baddate[2,]=="Sep"]<-"09"
baddate[2,baddate[2,]=="Oct"]<-"10"
baddate[2,baddate[2,]=="Nov"]<-"11"
baddate[2,baddate[2,]=="Dec"]<-"12"
ace_tntp$Date<-as.POSIXct(paste(baddate[3,],baddate[2,],baddate[1,],sep="-"))
ace_tntp<-ace_tntp[month(ace_tntp$Date)>=5 & month(ace_tntp$Date)<=9,]
ace_tntp$Value<-ace_tntp$Value*1000
ace_tntp$lagoslakeid<-rep(NA, nrow(ace_tntp))
ace_tntp$lagoslakeid[ace_tntp$Site=="Saylorville"]<- -33
ace_tntp$lagoslakeid[ace_tntp$Site=="Red Rock"]<- -22
ace_tntp$lagoslakeid[ace_tntp$Site=="Coralville"]<- -11


# Hydrologic connectivity -------------------------------------------------------------------------
# Dropped.

# hydrotype<-lagosne_select(table="lakes.geo", vars=c("lagoslakeid","lakeconnection"))
# hydrotype<-hydrotype[hydrotype$lagoslakeid %in% lakeinfo$lagoslakeid,]
# hydrotype$lakeconnection<-as.character(hydrotype$lakeconnection)

# Drainage ratio ----------------------------------------------------------------------------------

dr<-lagosne_select(table="iws", vars=c("lagoslakeid","iws_ha","iws_lakeareaha"))
dr$dr<-dr$iws_ha/dr$iws_lakeareaha
dr<-dr[dr$lagoslakeid %in% lakeinfo$lagoslakeid,]
dr<-dr[,-c(2:3)]

# Depth -------------------------------------------------------------------------------------------

depth<-lagosne_select(table="lakes_limno", vars=c("lagoslakeid","meandepth","maxdepth"))
depth<-depth[depth$lagoslakeid %in% lakeinfo$lagoslakeid,]

# sum(!is.na(depth$meandepth)) #72 obs
# sum(!is.na(depth$maxdepth)) #198 obs
depth<-depth[,-2] #drop mean depth because it has more missing values
ia_depth<-cbind(ia_covar$lagoslakeid, ia_covar$max_depth_m)
colnames(ia_depth)<-c("lagoslakeid","maxdepth")
depth<-rbind(depth, ia_depth)

# TN ----------------------------------------------------------------------------------------------

tn.raw<-lagosne_select(table="epi_nutr", vars=c("lagoslakeid","sampledate","tn"))
tn.raw<-tn.raw[tn.raw$lagoslakeid %in% lakeinfo$lagoslakeid,]
tn.raw$sampledate<-as.POSIXct(tn.raw$sampledate, format="%m/%d/%Y")
tn.raw<-tn.raw[month(tn.raw$sampledate) %in% c(5,6,7,8,9),]

tn.clamp<-data.frame(lagoslakeid=clamp$lagoslakeid, sampledate=clamp$Sample_Date, tn=clamp$TN_ugL)

tn.ace<-data.frame(lagoslakeid=ace_tntp$lagoslakeid[ace_tntp$Parameter=="TN"],
                   sampledate=ace_tntp$Date[ace_tntp$Parameter=="TN"],
                   tn=ace_tntp$Value[ace_tntp$Parameter=="TN"])

tn.raw<-rbind(tn.raw, tn.clamp, tn.ace)


#find trend for suitable lakes
tn.trend<-rep(NA, nrow(lakeinfo))
tn.avg<-rep(NA, nrow(lakeinfo))

for(ii in 1:length(lakeinfo$lagoslakeid)){
  
  tmp<-tn.raw[tn.raw$lagoslakeid == lakeinfo$lagoslakeid[ii],]
  if(length(tmp)==0){next}
  
  tmp.agg<-aggregate(tmp$tn, by=list(year(tmp$sampledate)), FUN="mean", na.rm=T)
  colnames(tmp.agg)<-c("year","tn")
  yy<-min(sampfreq$year[sampfreq$lagoslakeid==lakeinfo$lagoslakeid[ii]]):
    max(sampfreq$year[sampfreq$lagoslakeid==lakeinfo$lagoslakeid[ii]])
  tmp.agg<-tmp.agg[tmp.agg$year %in% yy,]
  if(sum(!is.na(tmp.agg$tn))<5){next}
  
  tn.avg[ii]<-mean(tmp.agg$tn, na.rm=T) 
  tmp.agg$tn<-scale(tmp.agg$tn)
  tn.trend[ii]<-lm(tmp.agg$tn ~ tmp.agg$year, na.action="na.exclude")$coefficients[2]
  
}
sum(!is.na(tn.avg))#157 lakes with data if nmin = 5, 170 lakes with data if nmin = 3

tn<-data.frame(lagoslakeid=lakeinfo$lagoslakeid, tn.avg=tn.avg, tn.trend=tn.trend)

rm(tn.raw)

# TP ----------------------------------------------------------------------------------------------

tp.raw<-lagosne_select(table="epi_nutr", vars=c("lagoslakeid","sampledate","tp"))
tp.raw<-tp.raw[tp.raw$lagoslakeid %in% lakeinfo$lagoslakeid,]
tp.raw$sampledate<-as.POSIXct(tp.raw$sampledate, format="%m/%d/%Y")
tp.raw<-tp.raw[month(tp.raw$sampledate) %in% c(5,6,7,8,9),]

tp.clamp<-data.frame(lagoslakeid=clamp$lagoslakeid, sampledate=clamp$Sample_Date, tp=clamp$TP_ugL)

tp.ace<-data.frame(lagoslakeid=ace_tntp$lagoslakeid[ace_tntp$Parameter=="TP"],
                   sampledate=ace_tntp$Date[ace_tntp$Parameter=="TP"],
                   tp=ace_tntp$Value[ace_tntp$Parameter=="TP"])

tp.raw<-rbind(tp.raw, tp.clamp, tp.ace)

#find trend for suitable lakes
tp.trend<-rep(NA, nrow(lakeinfo))
tp.avg<-rep(NA, nrow(lakeinfo))

for(ii in 1:length(lakeinfo$lagoslakeid)){
  
  tmp<-tp.raw[tp.raw$lagoslakeid == lakeinfo$lagoslakeid[ii],]
  if(length(tmp)==0){next}
  
  tmp.agg<-aggregate(tmp$tp, by=list(year(tmp$sampledate)), FUN="mean", na.rm=T)
  colnames(tmp.agg)<-c("year","tp")
  yy<-min(sampfreq$year[sampfreq$lagoslakeid==lakeinfo$lagoslakeid[ii]]):
    max(sampfreq$year[sampfreq$lagoslakeid==lakeinfo$lagoslakeid[ii]])
  tmp.agg<-tmp.agg[tmp.agg$year %in% yy,]
  if(sum(!is.na(tmp.agg$tp))<5){next}
  
  tp.avg[ii]<-mean(tmp.agg$tp, na.rm=T)
  tmp.agg$tp<-scale(tmp.agg$tp)
  tp.trend[ii]<-lm(tmp.agg$tp ~ tmp.agg$year, na.action="na.exclude")$coefficients[2]
  
  
}

sum(!is.na(tp.avg))#192 lakes with data with nmin=5, 203 with nmin=3

tp<-data.frame(lagoslakeid=lakeinfo$lagoslakeid, tp.avg=tp.avg, tp.trend=tp.trend)

rm(tp.raw)

# N:P ---------------------------------------------------------------------------------------------

np.raw<-lagosne_select(table="epi_nutr", vars=c("lagoslakeid","sampledate","tn","tp"))
np.raw<-np.raw[np.raw$lagoslakeid %in% lakeinfo$lagoslakeid,]
np.raw$sampledate<-as.POSIXct(np.raw$sampledate, format="%m/%d/%Y")
np.ace<-inner_join(tn.ace,tp.ace)
np.clamp<-inner_join(tn.clamp,tp.clamp)
np.raw<-rbind(np.raw,np.ace,np.clamp)
np.raw<-np.raw[month(np.raw$sampledate) %in% c(5,6,7,8,9),]
np.raw$moln<-np.raw$tn/14.0067*1e-6
np.raw$molp<-np.raw$tp/30.973762*1e-6
np.raw$np<-np.raw$moln/np.raw$molp
np.raw$np[is.infinite(np.raw$np)]<-NA
np.raw$log.np<-log(np.raw$np)

np<-aggregate(np.raw$log.np, by=list(np.raw$lagoslakeid), FUN="mean", na.rm=T)
np$x[is.nan(np$x)]<-NA
colnames(np)<-c("lagoslakeid","np")

sum(!is.na(np$np))#166 lakes with data

rm(np.raw)

# Color -------------------------------------------------------------------------------------------

color.raw<-lagosne_select(table="epi_nutr", vars=c("lagoslakeid","sampledate","colora","colort"))
color.raw<-color.raw[color.raw$lagoslakeid %in% lakeinfo$lagoslakeid,]
color.raw$sampledate<-as.POSIXct(color.raw$sampledate, format="%m/%d/%Y")
color.raw<-color.raw[month(color.raw$sampledate) %in% c(5,6,7,8,9),]

color<-aggregate(cbind(color.raw$colora,color.raw$colort), by=list(color.raw$lagoslakeid), FUN="mean", na.rm=T)
colnames(color)<-c("lagoslakeid","colora","colort")
sum(!is.na(color$colora)) #82 lakes with data
sum(!is.na(color$colort)) #112 lakes with data

# Precipitation intensification -------------------------------------------------------------------
ppt.raw<-readRDS("lakepts_ppt_daily.RDS")

years<-1989:2018
ppt.raw<-ppt.raw[,substr(colnames(ppt.raw),1,4) %in% years & 
                   substr(colnames(ppt.raw),5,6) %in% c("05","06","07","08","09")]

# trends in summer daily mean
ppt.dmean<-matrix(NA, nrow(ppt.raw), length(years))
for(yy in 1:length(years)){
  ppt.dmean[,yy]<-apply(ppt.raw[,substr(colnames(ppt.raw),1,4)==years[yy]], 1, mean)
}

trend.pptmean<-rep(NA, nrow(ppt.dmean))
for(ii in 1:nrow(ppt.dmean)){
  y<-scale(ppt.dmean[ii,])
  lm1<-gls(y~years,correlation=corAR1(form=~years))
  trend.pptmean[ii]<-coefficients(lm1)[2]
}

trend.pptmean<-data.frame(lagoslakeid=as.numeric(rownames(ppt.raw)), trend.pptmean=trend.pptmean)


# trends in summer 95th percentile
ppt.p95<-matrix(NA, nrow(ppt.raw), length(years))
for(yy in 1:length(years)){
  ppt.p95[,yy]<-apply(ppt.raw[,substr(colnames(ppt.raw),1,4)==years[yy]], 1, quantile, 0.95)
}

trend.pptp95<-rep(NA, nrow(ppt.p95))
for(ii in 1:nrow(ppt.p95)){
  y<-scale(ppt.p95[ii,])
  lm1<-gls(y~years,correlation=corAR1(form=~years))
  trend.pptp95[ii]<-coefficients(lm1)[2]
}

trend.ppt95<-data.frame(lagoslakeid=as.numeric(rownames(ppt.raw)), trend.pptp95=trend.pptp95)

# trends in summer lag-1 autocorrelation
ppt.lag1<-matrix(NA, nrow(ppt.raw), length(years)-1)
for(yy in 2:(length(years))){
  for(ll in 1:nrow(ppt.lag1)){
    ppt.t<-ppt.raw[ll, substr(colnames(ppt.raw),1,4)==years[yy]]
    ppt.t1<-ppt.raw[ll, substr(colnames(ppt.raw),1,4)==years[yy-1]]
    ppt.lag1[ll,yy-1]<-cor(ppt.t,ppt.t1)
  }
}

trend.pptlag1<-rep(NA, nrow(ppt.lag1))
for(ii in 1:nrow(ppt.lag1)){
  lm1<-lm(scale(ppt.lag1[ii,])~years[-1])
  trend.pptlag1[ii]<-coefficients(lm1)[2]
}

trend.pptlag1<-data.frame(lagoslakeid=as.numeric(rownames(ppt.raw)), trend.pptlag1=trend.pptlag1)

# Temperature change ------------------------------------------------------------------------------
tmax.raw<-readRDS("lakepts_tmax_daily.RDS")

years<-1989:2018
tmax.raw<-tmax.raw[,substr(colnames(tmax.raw),1,4) %in% years & 
                   substr(colnames(tmax.raw),5,6) %in% c("05","06","07","08","09")]

# trends in summer daily mean
tmax.dmean<-matrix(NA, nrow(tmax.raw), length(years))
for(yy in 1:length(years)){
  tmax.dmean[,yy]<-apply(tmax.raw[,substr(colnames(tmax.raw),1,4)==years[yy]], 1, mean)
}

trend.tmaxmean<-rep(NA, nrow(tmax.dmean))
for(ii in 1:nrow(tmax.dmean)){
  y<-scale(tmax.dmean[ii,])
  lm1<-gls(y~years, correlation=corAR1(form=~years))
  trend.tmaxmean[ii]<-coefficients(lm1)[2]
}

trend.tmaxmean<-data.frame(lagoslakeid=as.numeric(rownames(tmax.raw)), trend.tmaxmean=trend.tmaxmean)


# trends in summer 95th percentile
tmax.p95<-matrix(NA, nrow(tmax.raw), length(years))
for(yy in 1:length(years)){
  tmax.p95[,yy]<-apply(tmax.raw[,substr(colnames(tmax.raw),1,4)==years[yy]], 1, quantile, 0.95)
}

trend.tmaxp95<-rep(NA, nrow(tmax.p95))
for(ii in 1:nrow(tmax.p95)){
  y<-scale(tmax.p95[ii,])
  lm1<-gls(y~years,correlation = corAR1(form = ~years))
  trend.tmaxp95[ii]<-coefficients(lm1)[2]
}

trend.tmax95<-data.frame(lagoslakeid=as.numeric(rownames(tmax.raw)), trend.tmaxp95=trend.tmaxp95)

# trends in summer lag-1 autocorrelation
tmax.lag1<-matrix(NA, nrow(tmax.raw), length(years)-1)
for(yy in 2:(length(years))){
  for(ll in 1:nrow(tmax.lag1)){
    tmax.t<-tmax.raw[ll, substr(colnames(tmax.raw),1,4)==years[yy]]
    tmax.t1<-tmax.raw[ll, substr(colnames(tmax.raw),1,4)==years[yy-1]]
    tmax.lag1[ll,yy-1]<-cor(tmax.t,tmax.t1)
  }
}

trend.tmaxlag1<-rep(NA, nrow(tmax.lag1))
for(ii in 1:nrow(tmax.lag1)){
  lm1<-lm(scale(tmax.lag1[ii,])~years[-1])
  trend.tmaxlag1[ii]<-coefficients(lm1)[2]
}

trend.tmaxlag1<-data.frame(lagoslakeid=as.numeric(rownames(tmax.raw)), trend.tmaxlag1=trend.tmaxlag1)

# Add watershed land cover ------------------------------------------------------------------------
landcover<-dt$iws.lulc
landcover<-landcover[landcover$lagoslakeid %in% lakeinfo$lagoslakeid,]
nlcd2011<-landcover[,grepl("nlcd2011_pct",colnames(landcover))]
wetland_pct<-rowSums(cbind(nlcd2011$iws_nlcd2011_pct_90,nlcd2011$iws_nlcd2011_pct_95))
devel_pct<-rowSums(cbind(nlcd2011$iws_nlcd2011_pct_21,nlcd2011$iws_nlcd2011_pct_22,
                         nlcd2011$iws_nlcd2011_pct_23,nlcd2011$iws_nlcd2011_pct_24))
agri_pct<-rowSums(cbind(nlcd2011$iws_nlcd2011_pct_81,nlcd2011$iws_nlcd2011_pct_82))
landcover<-data.frame(lagoslakeid=landcover$lagoslakeid,wetland_pct=wetland_pct,
                      devel_pct=devel_pct, agri_pct=agri_pct)

## Mean chlorophyll-a -----------------------------------------------------------------------------
chla.raw<-read.csv("hab_trend_data_filtered_20200203.csv",stringsAsFactors = F)
chla<-aggregate(chla.raw$chla,by=list(chla.raw$lagoslakeid),FUN="mean",na.rm=T)
colnames(chla)<-c("lagoslakeid","chla_mean")

## Trophic state ---------------------------------------------------------------------------------
#Chlorophyll-a TSI class
#TSI(CHL) = 9.81 ln(CHL) + 30.6
tsi.chl<-data.frame(lagoslakeid=chla$lagoslakeid, tsi=9.81 * log(chla$chla_mean) + 30.6)
tsi.chl$tsi.cat<-rep("lake",nrow(tsi.chl))

tsi.chl$tsi.cat[tsi.chl$tsi < 40]<-"oligotrophic"
tsi.chl$tsi.cat[tsi.chl$tsi >=40 & tsi.chl$tsi < 50]<-"mesotrophic"
tsi.chl$tsi.cat[tsi.chl$tsi >=50 & tsi.chl$tsi < 70]<-"eutrophic"
tsi.chl$tsi.cat[tsi.chl$tsi >= 70] <-"hypereutrophic"

## Restoration status ----------------------------------------------------------------------------
# nope.

## HUC Zone ---------------------------------------------------------------------------------------
lakepts<-SpatialPoints(coords=lakeinfo[,c("nhd_long","nhd_lat")],
                       proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
lakepts<-spTransform(lakepts,crs(hu4zones))
lakeHU4<-point.in.poly(lakepts, hu4zones)
lakeHU4<-data.frame(lagoslakeid=lakeinfo$lagoslakeid, HU4zone = lakeHU4$HUC4)
lakeHU4$HU2zone<-substr(lakeHU4$HU4zone,1,2)

## L-II Ecoregion ---------------------------------------------------------------------------------
lakepts<-spTransform(lakepts,crs(ecoregions))
lakeEcoreg<-point.in.poly(lakepts, ecoregions)
lakeEcoreg<-data.frame(lagoslakeid=lakeinfo$lagoslakeid, LII_ecoregion = lakeEcoreg$NA_L2CODE)

# Compile output ----------------------------------------------------------------------------------
lakecovars<-left_join(lakeinfo, depth)
#lakecovars<-left_join(lakecovars, hydrotype)
lakecovars<-left_join(lakecovars, dr)
lakecovars<-left_join(lakecovars, color)
lakecovars<-left_join(lakecovars, chla)
lakecovars<-left_join(lakecovars, tn)
lakecovars<-left_join(lakecovars, tp)
lakecovars<-left_join(lakecovars, np)
lakecovars<-left_join(lakecovars, trend.pptmean)
lakecovars<-left_join(lakecovars, trend.ppt95)
lakecovars<-left_join(lakecovars, trend.pptlag1)
lakecovars<-left_join(lakecovars, trend.tmaxmean)
lakecovars<-left_join(lakecovars, trend.tmax95)
lakecovars<-left_join(lakecovars, trend.tmaxlag1)
lakecovars<-left_join(lakecovars, landcover)
lakecovars<-left_join(lakecovars, lakeHU4)
lakecovars<-left_join(lakecovars, tsi.chl)
lakecovars<-left_join(lakecovars, lakeEcoreg)

write.csv(lakecovars, "lakecovars_20200810.csv", row.names=F)

