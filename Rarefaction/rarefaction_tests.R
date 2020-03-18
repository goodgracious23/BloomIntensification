## ----------------------------------------------------------------------------
## Apply rarefaction analysis to a number of lake years (n = 31 lake years)

rm(list=ls())

setwd("~/Box Sync/Intense HABs in Small Lakes/HAB Trends/Rarefaction")

if (!require(here)) install.packages("here")
if (!require(here)) install.packages("lubridate")

library(lubridate)
library(here)

source(here("Rarefaction/rarefy.R"))

## ----------------------------------------------------------------------------
## Load datasets

## NOTE: the beaver creek reservoir (bcr) data are unpublished. Contact
## Mike Pace (mlp5fy@virginia.edu) or Cal Buelo (cbuelo10@gmail.com).
## The mendota data are too large for the github repo.

squeal.raw<-read.csv("LRT_Squeal2_allYears_DailyData_sondeMARSS_manualChl_v1.csv", stringsAsFactors = F)
grnval.raw<-read.csv("GVL_EXO19.csv", stringsAsFactors = F)
sotwin.raw<-read.csv("ST EXO 051619 - 082619.csv", stringsAsFactors = F)
dortiz.raw<-read.csv("IowaLake_HighFrequency_EDI.csv", stringsAsFactors = F)
# bcr2014.raw<-read.csv("BeaverCreek_LightTempSonde_highFreq_2014.csv", stringsAsFactors = F)
# bcr2015.raw<-read.csv("BeaverCreek_Sonde_highFreq_2015.csv", stringsAsFactors = F)
# bcr2017.raw<-read.csv("BeaverCreek_LightTempSonde_highFreqData_timesMatched_2017_v1.csv", stringsAsFactors = F)
# mendota.raw<-read.csv("mendota_highFrequencySondeData.csv", stringsAsFactors = F)

## Peter Lake, 2013
peter2013 <- squeal.raw[squeal.raw$Year==2013 & squeal.raw$Lake=="R",]

rf.peter2013<-rarefy(tt=peter2013$DOYtrunc, ts=peter2013$Manual_Chl)

pdf("rarefaction_allLakes.pdf", width=6.5, height=9)
par(mfrow=c(4,1),mar=c(3.6,3.6,1.1,1.1),oma=c(0,0,1.1,0), mgp=c(1.6,0.6,0))

# pdf("rarefy_peter2013.pdf",width=6.5,height=9)
# par(mfrow=c(6,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(peter2013$DOYtrunc,peter2013$Manual_Chl,ylab= "chl",xlab="DOY")
plot(rf.peter2013$interval,rf.peter2013$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.peter2013$interval,rf.peter2013$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.peter2013$interval,rf.peter2013$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Peter Lake, 2013", 3, outer=T, line=-0.5)
#dev.off()

## Peter Lake, 2014
peter2014 <- squeal.raw[squeal.raw$Year==2014 & squeal.raw$Lake=="R",]

rf.peter2014<-rarefy(tt=peter2014$DOYtrunc, ts=peter2014$Manual_Chl)

# pdf("rarefy_peter2014.pdf",width=6.5,height=9)
# par(mfrow=c(6,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(peter2014$DOYtrunc,peter2014$Manual_Chl,ylab="chl",xlab="DOY")
plot(rf.peter2014$interval,rf.peter2014$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.peter2014$interval,rf.peter2014$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.peter2014$interval,rf.peter2014$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Peter Lake, 2014", 3, outer=T, line=-0.5)
# dev.off()

## Peter Lake, 2015
peter2015 <- squeal.raw[squeal.raw$Year==2015 & squeal.raw$Lake=="R",]

rf.peter2015<-rarefy(tt=peter2015$DOYtrunc, ts=peter2015$Manual_Chl)

# pdf("rarefy_peter2015.pdf",width=6.5,height=9)
# par(mfrow=c(6,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(peter2015$DOYtrunc,peter2015$Manual_Chl,ylab="chl",xlab="DOY")
plot(rf.peter2015$interval,rf.peter2015$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.peter2015$interval,rf.peter2015$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.peter2015$interval,rf.peter2015$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Peter Lake, 2015", 3, outer=T, line=-0.5)
# dev.off()

## Paul Lake, 2013
paul2013 <- squeal.raw[squeal.raw$Year==2013 & squeal.raw$Lake=="L",]

rf.paul2013<-rarefy(tt=paul2013$DOYtrunc, ts=paul2013$Manual_Chl)

# pdf("rarefy_paul2013.pdf",width=6.5,height=9)
# par(mfrow=c(6,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(paul2013$DOYtrunc,paul2013$Manual_Chl,ylab="chl",xlab="DOY")
plot(rf.paul2013$interval,rf.paul2013$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.paul2013$interval,rf.paul2013$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.paul2013$interval,rf.paul2013$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Paul Lake, 2013", 3, outer=T, line=-0.5)
# dev.off()

## Paul Lake, 2014
paul2014 <- squeal.raw[squeal.raw$Year==2014 & squeal.raw$Lake=="L",]

rf.paul2014<-rarefy(tt=paul2014$DOYtrunc, ts=paul2014$Manual_Chl)

# pdf("rarefy_paul2014.pdf")
# par(mfrow=c(6,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(paul2014$DOYtrunc,paul2014$Manual_Chl,ylab="chl",xlab="DOY")
plot(rf.paul2014$interval,rf.paul2014$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.paul2014$interval,rf.paul2014$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.paul2014$interval,rf.paul2014$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Paul Lake, 2014", 3, outer=T, line=-0.5)
# dev.off()

## Paul Lake, 2015
paul2015 <- squeal.raw[squeal.raw$Year==2015 & squeal.raw$Lake=="L",]

rf.paul2015<-rarefy(tt=paul2015$DOYtrunc, ts=paul2015$Manual_Chl)

# pdf("rarefy_paul2015.pdf")
# par(mfrow=c(4,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(paul2015$DOYtrunc,paul2015$Manual_Chl,ylab="chl",xlab="DOY")
plot(rf.paul2015$interval,rf.paul2015$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.paul2015$interval,rf.paul2015$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.paul2015$interval,rf.paul2015$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Paul Lake, 2015", 3, outer=T, line=-0.5)
# dev.off()

## Tuesday Lake, 2013
tuesday2013 <- squeal.raw[squeal.raw$Year==2013 & squeal.raw$Lake=="T",]

rf.tuesday2013<-rarefy(tt=tuesday2013$DOYtrunc, ts=tuesday2013$Manual_Chl)

# pdf("rarefy_tuesday2013.pdf")
# par(mfrow=c(4,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(tuesday2013$DOYtrunc,tuesday2013$Manual_Chl,ylab="chl",xlab="DOY")
plot(rf.tuesday2013$interval,rf.tuesday2013$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.tuesday2013$interval,rf.tuesday2013$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.tuesday2013$interval,rf.tuesday2013$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Tuesday Lake, 2013", 3, outer=T, line=-0.5)
# dev.off()

## Tuesday Lake, 2014
tuesday2014 <- squeal.raw[squeal.raw$Year==2014 & squeal.raw$Lake=="T",]

rf.tuesday2014<-rarefy(tt=tuesday2014$DOYtrunc, ts=tuesday2014$Manual_Chl)

# pdf("rarefy_tuesday2014.pdf")
# par(mfrow=c(4,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(tuesday2014$DOYtrunc,tuesday2014$Manual_Chl,ylab="chl",xlab="DOY")
plot(rf.tuesday2014$interval,rf.tuesday2014$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.tuesday2014$interval,rf.tuesday2014$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.tuesday2014$interval,rf.tuesday2014$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Tuesday Lake, 2014", 3, outer=T, line=-0.5)
# dev.off()

## Tuesday Lake, 2015
tuesday2015 <- squeal.raw[squeal.raw$Year==2015 & squeal.raw$Lake=="T",]

rf.tuesday2015<-rarefy(tt=tuesday2015$DOYtrunc, ts=tuesday2015$Manual_Chl)

# pdf("rarefy_tuesday2015.pdf")
# par(mfrow=c(4,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(tuesday2015$DOYtrunc,tuesday2015$Manual_Chl,ylab="chl",xlab="DOY")
plot(rf.tuesday2015$interval,rf.tuesday2015$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.tuesday2015$interval,rf.tuesday2015$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.tuesday2015$interval,rf.tuesday2015$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Tuesday Lake, 2015", 3, outer=T, line=-0.5)
# dev.off()

## Green Valley Lake, 2014
grnval2014<-dortiz.raw[dortiz.raw$Lake=="Green Valley Lake" & dortiz.raw$Year==2014,]

rf.grnval2014<-rarefy(tt=grnval2014$DOY, ts=grnval2014$Chlorophyll)

# pdf("rarefy_grnval2014.pdf")
# par(mfrow=c(4,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(grnval2014$DOY,grnval2014$Chlorophyll,ylab="chl",xlab="DOY")
plot(rf.grnval2014$interval,rf.grnval2014$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.grnval2014$interval,rf.grnval2014$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.grnval2014$interval,rf.grnval2014$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Green Valley Lake, 2014", 3, outer=T, line=-0.5)

## Green Valley Lake, 2015

grnval2015<-dortiz.raw[dortiz.raw$Lake=="Green Valley Lake" & dortiz.raw$Year==2015,]

rf.grnval2015<-rarefy(tt=grnval2015$DOY, ts=grnval2015$Chlorophyll)

# pdf("rarefy_grnval2015.pdf")
# par(mfrow=c(4,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(grnval2015$DOY,grnval2015$Chlorophyll,ylab="chl",xlab="DOY")
plot(rf.grnval2015$interval,rf.grnval2015$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.grnval2015$interval,rf.grnval2015$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.grnval2015$interval,rf.grnval2015$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Green Valley Lake, 2015", 3, outer=T, line=-0.5)

## Green Valley Lake, 2019
grnval2019<-aggregate(grnval.raw$Chlorophyll.ug.L,by=list(grnval.raw$DOY),FUN="mean")
colnames(grnval2019)<-c("DOY","chla")

rf.grnval2019<-rarefy(tt=grnval2019$DOY, ts=grnval2019$chla)

plot(grnval2019$DOY,grnval2019$chla,ylab="chl",xlab="DOY")
plot(rf.grnval2019$interval,rf.grnval2019$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.grnval2019$interval,rf.grnval2019$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.grnval2019$interval,rf.grnval2019$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Green Valley Lake, 2019", 3, outer=T, line=-0.5)

## South Twin Lake, 2018

sotwin2018<-dortiz.raw[dortiz.raw$Lake=="South Twin Lake" & dortiz.raw$Year==2018,]

rf.sotwin2018<-rarefy(tt=sotwin2018$DOY, ts=sotwin2018$Chlorophyll)

# pdf("rarefy_sotwin2018.pdf")
# par(mfrow=c(4,1),mar=c(4.1,4.1,1.1,1.1),oma=c(0,0,1.1,0))
plot(sotwin2018$DOY,sotwin2018$Chlorophyll,ylab="chl",xlab="DOY")
plot(rf.sotwin2018$interval,rf.sotwin2018$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.sotwin2018$interval,rf.sotwin2018$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.sotwin2018$interval,rf.sotwin2018$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("South Twin Lake, 2018", 3, outer=T, line=-0.5)

## South Twin Lake, 2019
sotwin.raw<-sotwin.raw[complete.cases(sotwin.raw),]
sotwin.raw$Date..MM.DD.YYYY.<-as.POSIXct(sotwin.raw$Date..MM.DD.YYYY., format="%m/%d/%Y")
sotwin.raw$DOY<-yday(sotwin.raw$Date..MM.DD.YYYY.)
sotwin.raw$Chlorophyll.ug.L<-as.numeric(sotwin.raw$Chlorophyll.ug.L)
sotwin2019<-aggregate(sotwin.raw$Chlorophyll.ug.L, by=list(sotwin.raw$DOY), FUN="mean", na.rm=T)
colnames(sotwin2019)<-c("DOY","chla")

rf.sotwin2019<-rarefy(tt=sotwin2019$DOY,ts=sotwin2019$chla)

plot(sotwin2019$DOY,sotwin2019$chla,ylab="chl",xlab="DOY")
plot(rf.sotwin2019$interval,rf.sotwin2019$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.sotwin2019$interval,rf.sotwin2019$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.sotwin2019$interval,rf.sotwin2019$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("South Twin Lake, 2019", 3, outer=T, line=-0.5)

## Blackhawk Lake, 2015
blkhwk2015<-dortiz.raw[dortiz.raw$Lake=="Blackhawk Lake" & dortiz.raw$Year==2015,]

rf.blkhwk2015<-rarefy(tt=blkhwk2015$DOY, ts=blkhwk2015$Chlorophyll)

plot(blkhwk2015$DOY,blkhwk2015$Chlorophyll,ylab="chl",xlab="DOY")
plot(rf.blkhwk2015$interval,rf.blkhwk2015$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.blkhwk2015$interval,rf.blkhwk2015$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.blkhwk2015$interval,rf.blkhwk2015$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Blackhawk Lake, 2015", 3, outer=T, line=-0.5)

## Swan Lake, 2018

swan2018<-dortiz.raw[dortiz.raw$Lake=="Swan Lake" & dortiz.raw$Year==2018,]

rf.swan2018<-rarefy(tt=swan2018$DOY, ts=swan2018$Chlorophyll)

plot(swan2018$DOY,swan2018$Chlorophyll,ylab="chl",xlab="DOY")
plot(rf.swan2018$interval,rf.swan2018$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.swan2018$interval,rf.swan2018$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.swan2018$interval,rf.swan2018$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Swan Lake, 2018", 3, outer=T, line=-0.5)

## Beaver Creek,  2014
bcr2014.raw$DOY2<-floor(bcr2014.raw$DOY)
bcr2014<-aggregate(bcr2014.raw$Chl_ugL, list(bcr2014.raw$DOY2), FUN="mean", na.rm=T)
colnames(bcr2014)<-c("DOY","chl")

rf.bcr2014<-rarefy(tt=bcr2014$DOY, ts=bcr2014$chl)

plot(bcr2014$DOY,bcr2014$chl,ylab="chl",xlab="DOY")
plot(rf.bcr2014$interval,rf.bcr2014$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.bcr2014$interval,rf.bcr2014$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.bcr2014$interval,rf.bcr2014$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Beaver Creek, 2014", 3, outer=T, line=-0.5)

## Beaver Creek, 2015
bcr2015.raw$Date<-as.POSIXct(bcr2015.raw$Date, format="%m/%d/%Y")
bcr2015.raw$DOY<-yday(bcr2015.raw$Date)
bcr2015<-aggregate(bcr2015.raw$Chl_ugL, list(bcr2015.raw$DOY), FUN="mean", na.rm=T)
colnames(bcr2015)<-c("DOY","chl")

rf.bcr2015<-rarefy(tt=bcr2015$DOY, ts=bcr2015$chl)

plot(bcr2015$DOY,bcr2015$chl,ylab="chl",xlab="DOY")
plot(rf.bcr2015$interval,rf.bcr2015$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.bcr2015$interval,rf.bcr2015$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.bcr2015$interval,rf.bcr2015$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Beaver Creek, 2015", 3, outer=T, line=-0.5)

## Beaver Creek, 2017
bcr2017.raw$Date<-as.POSIXct(bcr2017.raw$Date)
bcr2017.raw$DOY<-floor(yday(bcr2017.raw$Date))
bcr2017<-aggregate(bcr2017.raw$Chl_ugL, list(bcr2017.raw$DOY), FUN="mean", na.rm=T)
colnames(bcr2017)<-c("DOY","chl")

rf.bcr2017<-rarefy(tt=bcr2017$DOY, ts=bcr2017$chl)

plot(bcr2017$DOY,bcr2017$chl,ylab=" chl",xlab="DOY")
plot(rf.bcr2017$interval,rf.bcr2017$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.bcr2017$interval,rf.bcr2017$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.bcr2017$interval,rf.bcr2017$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Beaver Creek, 2017", 3, outer=T, line=-0.5)

## Mendota, 2006
mendota2006<-mendota.raw[mendota.raw$year4==2006,]
mendota2006<-aggregate(mendota2006$chlor, list(mendota2006$daynum), FUN="mean")
colnames(mendota2006)<-c("daynum","chlor")

rf.mendota2006<-rarefy(tt=mendota2006$daynum, ts=mendota2006$chlor)

plot(mendota2006$daynum,mendota2006$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2006$interval,rf.mendota2006$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2006$interval,rf.mendota2006$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2006$interval,rf.mendota2006$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2006", 3, outer=T, line=-0.5)

## Mendota, 2007
mendota2007<-mendota.raw[mendota.raw$year4==2007,]
mendota2007<-aggregate(mendota2007$chlor, list(mendota2007$daynum), FUN="mean")
colnames(mendota2007)<-c("daynum","chlor")

rf.mendota2007<-rarefy(tt=mendota2007$daynum, ts=mendota2007$chlor)

plot(mendota2007$daynum,mendota2007$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2007$interval,rf.mendota2007$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2007$interval,rf.mendota2007$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2007$interval,rf.mendota2007$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2007", 3, outer=T, line=-0.5)

## Mendota, 2008
mendota2008<-mendota.raw[mendota.raw$year4==2008,]
mendota2008<-aggregate(mendota2008$chlor, list(mendota2008$daynum), FUN="mean")
colnames(mendota2008)<-c("daynum","chlor")

rf.mendota2008<-rarefy(tt=mendota2008$daynum, ts=mendota2008$chlor)

plot(mendota2008$daynum,mendota2008$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2008$interval,rf.mendota2008$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2008$interval,rf.mendota2008$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2008$interval,rf.mendota2008$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2008", 3, outer=T, line=-0.5)

## Mendota, 2009
mendota2009<-mendota.raw[mendota.raw$year4==2009,]
mendota2009<-aggregate(mendota2009$chlor, list(mendota2009$daynum), FUN="mean")
colnames(mendota2009)<-c("daynum","chlor")

rf.mendota2009<-rarefy(tt=mendota2009$daynum, ts=mendota2009$chlor)

plot(mendota2009$daynum,mendota2009$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2009$interval,rf.mendota2009$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2009$interval,rf.mendota2009$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2009$interval,rf.mendota2009$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2009", 3, outer=T, line=-0.5)

## Mendota, 2010
mendota2010<-mendota.raw[mendota.raw$year4==2010,]
mendota2010<-aggregate(mendota2010$chlor, list(mendota2010$daynum), FUN="mean")
colnames(mendota2010)<-c("daynum","chlor")

rf.mendota2010<-rarefy(tt=mendota2010$daynum, ts=mendota2010$chlor)

plot(mendota2010$daynum,mendota2010$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2010$interval,rf.mendota2010$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2010$interval,rf.mendota2010$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2010$interval,rf.mendota2010$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2010", 3, outer=T, line=-0.5)

## Mendota, 2011
mendota2011<-mendota.raw[mendota.raw$year4==2011,]
mendota2011<-aggregate(mendota2011$chlor, list(mendota2011$daynum), FUN="mean")
colnames(mendota2011)<-c("daynum","chlor")

rf.mendota2011<-rarefy(tt=mendota2011$daynum, ts=mendota2011$chlor)

plot(mendota2011$daynum,mendota2011$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2011$interval,rf.mendota2011$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2011$interval,rf.mendota2011$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2011$interval,rf.mendota2011$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2011", 3, outer=T, line=-0.5)

## Mendota, 2012
mendota2012<-mendota.raw[mendota.raw$year4==2012,]
mendota2012<-aggregate(mendota2012$chlor, list(mendota2012$daynum), FUN="mean")
colnames(mendota2012)<-c("daynum","chlor")

rf.mendota2012<-rarefy(tt=mendota2012$daynum, ts=mendota2012$chlor)

plot(mendota2012$daynum,mendota2012$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2012$interval,rf.mendota2012$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2012$interval,rf.mendota2012$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2012$interval,rf.mendota2012$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2012", 3, outer=T, line=-0.5)

## Mendota, 2013
mendota2013<-mendota.raw[mendota.raw$year4==2013,]
mendota2013<-aggregate(mendota2013$chlor, list(mendota2013$daynum), FUN="mean")
colnames(mendota2013)<-c("daynum","chlor")

rf.mendota2013<-rarefy(tt=mendota2013$daynum, ts=mendota2013$chlor)

plot(mendota2013$daynum,mendota2013$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2013$interval,rf.mendota2013$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2013$interval,rf.mendota2013$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2013$interval,rf.mendota2013$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2013", 3, outer=T, line=-0.5)

## Mendota, 2014 removed due to prevalence of missing values

## Mendota, 2015
mendota2015<-mendota.raw[mendota.raw$year4==2015,]
mendota2015<-aggregate(mendota2015$chlor, list(mendota2015$daynum), FUN="mean")
colnames(mendota2015)<-c("daynum","chlor")

rf.mendota2015<-rarefy(tt=mendota2015$daynum, ts=mendota2015$chlor)

plot(mendota2015$daynum,mendota2015$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2015$interval,rf.mendota2015$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2015$interval,rf.mendota2015$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2015$interval,rf.mendota2015$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2015", 3, outer=T, line=-0.5)

## Mendota, 2016
mendota2016<-mendota.raw[mendota.raw$year4==2016,]
mendota2016<-aggregate(mendota2016$chlor, list(mendota2016$daynum), FUN="mean")
colnames(mendota2016)<-c("daynum","chlor")

rf.mendota2016<-rarefy(tt=mendota2016$daynum, ts=mendota2016$chlor)

plot(mendota2016$daynum,mendota2016$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2016$interval,rf.mendota2016$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2016$interval,rf.mendota2016$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2016$interval,rf.mendota2016$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2016", 3, outer=T, line=-0.5)

## Mendota, 2017
mendota2017<-mendota.raw[mendota.raw$year4==2017,]
mendota2017<-aggregate(mendota2017$chlor, list(mendota2017$daynum), FUN="mean")
colnames(mendota2017)<-c("daynum","chlor")

rf.mendota2017<-rarefy(tt=mendota2017$daynum, ts=mendota2017$chlor)

plot(mendota2017$daynum,mendota2017$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2017$interval,rf.mendota2017$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2017$interval,rf.mendota2017$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2017$interval,rf.mendota2017$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2017", 3, outer=T, line=-0.5)

## Mendota, 2018
mendota2018<-mendota.raw[mendota.raw$year4==2018,]
mendota2018<-aggregate(mendota2018$chlor, list(mendota2018$daynum), FUN="mean")
colnames(mendota2018)<-c("daynum","chlor")

rf.mendota2018<-rarefy(tt=mendota2018$daynum, ts=mendota2018$chlor)

plot(mendota2018$daynum,mendota2018$chlor,ylab="chl",xlab="DOY")
plot(rf.mendota2018$interval,rf.mendota2018$dat.mean,xlim=c(1,30), ylab="mean chl", xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2018$interval,rf.mendota2018$dat.95pct,ylab="95%",xlab="sampling interval")
abline(v=c(7,14), col="grey")
plot(rf.mendota2018$interval,rf.mendota2018$dat.grtr,ylab="p > 11",xlab="sampling interval")
abline(v=c(7,14), col="grey")
mtext("Mendota, 2018", 3, outer=T, line=-0.5)

dev.off()

## ----------------------------------------------------------------------------
## Root mean squared errors

lakeyrs<-c("Peter 2013","Peter 2014","Peter 2015","Paul 2013","Paul 2014","Paul 2015",
           "Tuesday 2013","Tuesday 2014","Tuesday 2015","Green Valley 2014",
           "Green Valley 2015","Green Valley 2019","South Twin 2018","South Twin 2019",
           "Blackhawk 2015","Swan 2018", "Beaver Creek 2014","Beaver Creek 2015",
           "Beaver Creek 2017",paste("Mendota",2006:2018, sep=" "))
lakeyrs<-lakeyrs[!lakeyrs=="Mendota 2014"]

rmse<-function(x1,x){
  dx<-x-x1
  return(sqrt(mean(dx^2)))
}


rmse.mean<-data.frame(lakeyr=lakeyrs,
                d1=c(rf.peter2013$dat.mean[1],rf.peter2014$dat.mean[1],
                           rf.peter2015$dat.mean[1],rf.paul2013$dat.mean[1],
                           rf.paul2014$dat.mean[1],rf.paul2015$dat.mean[1],
                           rf.tuesday2013$dat.mean[1],rf.tuesday2014$dat.mean[1],
                           rf.tuesday2015$dat.mean[1],rf.grnval2014$dat.mean[1],
                           rf.grnval2015$dat.mean[1],rf.grnval2019$dat.mean[1],
                           rf.sotwin2018$dat.mean[1],rf.sotwin2019$dat.mean[1],
                           rf.blkhwk2015$dat.mean[1],rf.swan2018$dat.mean[1],
                           rf.bcr2014$dat.mean[1],rf.bcr2015$dat.mean[1],
                           rf.bcr2017$dat.mean[1],rf.mendota2006$dat.mean[1],
                           rf.mendota2007$dat.mean[1],rf.mendota2008$dat.mean[1],
                           rf.mendota2009$dat.mean[1],rf.mendota2010$dat.mean[1],
                           rf.mendota2011$dat.mean[1],rf.mendota2012$dat.mean[1],
                           rf.mendota2013$dat.mean[1],#rf.mendota2014$dat.mean[1],
                           rf.mendota2015$dat.mean[1],rf.mendota2016$dat.mean[1],
                           rf.mendota2017$dat.mean[1],rf.mendota2018$dat.mean[1]
                           ),
          d7.rmse=c(rmse(rf.peter2013$dat.mean[1],rf.peter2013$dat.mean[rf.peter2013$interval==7]),
                    rmse(rf.peter2014$dat.mean[1],rf.peter2014$dat.mean[rf.peter2014$interval==7]),
                    rmse(rf.peter2015$dat.mean[1],rf.peter2015$dat.mean[rf.peter2015$interval==7]),
                    rmse(rf.paul2013$dat.mean[1],rf.paul2013$dat.mean[rf.paul2013$interval==7]),
                    rmse(rf.paul2014$dat.mean[1],rf.paul2014$dat.mean[rf.paul2014$interval==7]),
                    rmse(rf.paul2015$dat.mean[1],rf.paul2015$dat.mean[rf.paul2015$interval==7]),
                    rmse(rf.tuesday2013$dat.mean[1],rf.tuesday2013$dat.mean[rf.tuesday2013$interval==7]),
                    rmse(rf.tuesday2014$dat.mean[1],rf.tuesday2014$dat.mean[rf.tuesday2014$interval==7]),
                    rmse(rf.tuesday2015$dat.mean[1],rf.tuesday2015$dat.mean[rf.tuesday2015$interval==7]),
                    rmse(rf.grnval2014$dat.mean[1],rf.grnval2014$dat.mean[rf.grnval2014$interval==7]),
                    rmse(rf.grnval2015$dat.mean[1],rf.grnval2015$dat.mean[rf.grnval2015$interval==7]),
                                rmse(rf.grnval2019$dat.mean[1],rf.grnval2019$dat.mean[rf.grnval2019$interval==7]),
                                rmse(rf.sotwin2018$dat.mean[1],rf.sotwin2018$dat.mean[rf.sotwin2018$interval==7]),
                                rmse(rf.sotwin2019$dat.mean[1],rf.sotwin2019$dat.mean[rf.sotwin2019$interval==7]),
                                rmse(rf.blkhwk2015$dat.mean[1],rf.blkhwk2015$dat.mean[rf.blkhwk2015$interval==7]),
                                rmse(rf.swan2018$dat.mean[1],rf.swan2018$dat.mean[rf.swan2018$interval==7]),
                                rmse(rf.bcr2014$dat.mean[1],rf.bcr2014$dat.mean[rf.bcr2014$interval==7]),
                                rmse(rf.bcr2015$dat.mean[1],rf.bcr2015$dat.mean[rf.bcr2015$interval==7]),
                                rmse(rf.bcr2017$dat.mean[1],rf.bcr2017$dat.mean[rf.bcr2017$interval==7]),
                                rmse(rf.mendota2006$dat.mean[1],rf.mendota2006$dat.mean[rf.mendota2006$interval==7]),
                                rmse(rf.mendota2007$dat.mean[1],rf.mendota2007$dat.mean[rf.mendota2007$interval==7]),
                                rmse(rf.mendota2008$dat.mean[1],rf.mendota2008$dat.mean[rf.mendota2008$interval==7]),
                                rmse(rf.mendota2009$dat.mean[1],rf.mendota2009$dat.mean[rf.mendota2009$interval==7]),
                                rmse(rf.mendota2010$dat.mean[1],rf.mendota2010$dat.mean[rf.mendota2010$interval==7]),
                                rmse(rf.mendota2011$dat.mean[1],rf.mendota2011$dat.mean[rf.mendota2011$interval==7]),
                                rmse(rf.mendota2012$dat.mean[1],rf.mendota2012$dat.mean[rf.mendota2012$interval==7]),
                                rmse(rf.mendota2013$dat.mean[1],rf.mendota2013$dat.mean[rf.mendota2013$interval==7]),
                                #rmse(rf.mendota2014$dat.mean[1],rf.mendota2014$dat.mean[rf.mendota2014$interval==7]),
                                rmse(rf.mendota2015$dat.mean[1],rf.mendota2015$dat.mean[rf.mendota2015$interval==7]),
                                rmse(rf.mendota2016$dat.mean[1],rf.mendota2016$dat.mean[rf.mendota2016$interval==7]),
                                rmse(rf.mendota2017$dat.mean[1],rf.mendota2017$dat.mean[rf.mendota2017$interval==7]),
                                rmse(rf.mendota2018$dat.mean[1],rf.mendota2018$dat.mean[rf.mendota2018$interval==7])
                      ),
                      d14.rmse=c(rmse(rf.peter2013$dat.mean[1],rf.peter2013$dat.mean[rf.peter2013$interval==14]),
                                rmse(rf.peter2014$dat.mean[1],rf.peter2014$dat.mean[rf.peter2014$interval==14]),
                                rmse(rf.peter2015$dat.mean[1],rf.peter2015$dat.mean[rf.peter2015$interval==14]),
                                rmse(rf.paul2013$dat.mean[1],rf.paul2013$dat.mean[rf.paul2013$interval==14]),
                                rmse(rf.paul2014$dat.mean[1],rf.paul2014$dat.mean[rf.paul2014$interval==14]),
                                rmse(rf.paul2015$dat.mean[1],rf.paul2015$dat.mean[rf.paul2015$interval==14]),
                                rmse(rf.tuesday2013$dat.mean[1],rf.tuesday2013$dat.mean[rf.tuesday2013$interval==14]),
                                rmse(rf.tuesday2014$dat.mean[1],rf.tuesday2014$dat.mean[rf.tuesday2014$interval==14]),
                                rmse(rf.tuesday2015$dat.mean[1],rf.tuesday2015$dat.mean[rf.tuesday2015$interval==14]),
                                rmse(rf.grnval2014$dat.mean[1],rf.grnval2014$dat.mean[rf.grnval2014$interval==14]),
                                rmse(rf.grnval2015$dat.mean[1],rf.grnval2015$dat.mean[rf.grnval2015$interval==14]),
                                rmse(rf.grnval2019$dat.mean[1],rf.grnval2019$dat.mean[rf.grnval2019$interval==14]),
                                rmse(rf.sotwin2018$dat.mean[1],rf.sotwin2018$dat.mean[rf.sotwin2018$interval==14]),
                                rmse(rf.sotwin2019$dat.mean[1],rf.sotwin2019$dat.mean[rf.sotwin2019$interval==14]),
                                rmse(rf.blkhwk2015$dat.mean[1],rf.blkhwk2015$dat.mean[rf.blkhwk2015$interval==14]),
                                rmse(rf.swan2018$dat.mean[1],rf.swan2018$dat.mean[rf.swan2018$interval==14]),
                                rmse(rf.bcr2014$dat.mean[1],rf.bcr2014$dat.mean[rf.bcr2014$interval==14]),
                                rmse(rf.bcr2015$dat.mean[1],rf.bcr2015$dat.mean[rf.bcr2015$interval==14]),
                                rmse(rf.bcr2014$dat.mean[1],rf.bcr2017$dat.mean[rf.bcr2017$interval==14]),
                                rmse(rf.mendota2006$dat.mean[1],rf.mendota2006$dat.mean[rf.mendota2006$interval==14]),
                                rmse(rf.mendota2007$dat.mean[1],rf.mendota2007$dat.mean[rf.mendota2007$interval==14]),
                                rmse(rf.mendota2008$dat.mean[1],rf.mendota2008$dat.mean[rf.mendota2008$interval==14]),
                                rmse(rf.mendota2009$dat.mean[1],rf.mendota2009$dat.mean[rf.mendota2009$interval==14]),
                                rmse(rf.mendota2010$dat.mean[1],rf.mendota2010$dat.mean[rf.mendota2010$interval==14]),
                                rmse(rf.mendota2011$dat.mean[1],rf.mendota2011$dat.mean[rf.mendota2011$interval==14]),
                                rmse(rf.mendota2012$dat.mean[1],rf.mendota2012$dat.mean[rf.mendota2012$interval==14]),
                                rmse(rf.mendota2013$dat.mean[1],rf.mendota2013$dat.mean[rf.mendota2013$interval==14]),
                                #rmse(rf.mendota2014$dat.mean[1],rf.mendota2014$dat.mean[rf.mendota2014$interval==14]),
                                rmse(rf.mendota2015$dat.mean[1],rf.mendota2015$dat.mean[rf.mendota2015$interval==14]),
                                rmse(rf.mendota2016$dat.mean[1],rf.mendota2016$dat.mean[rf.mendota2016$interval==14]),
                                rmse(rf.mendota2017$dat.mean[1],rf.mendota2017$dat.mean[rf.mendota2017$interval==14]),
                                rmse(rf.mendota2018$dat.mean[1],rf.mendota2018$dat.mean[rf.mendota2018$interval==14])
                      )
)

rmse.95pct<-data.frame(lakeyr=lakeyrs,
                      d1=c(rf.peter2013$dat.95pct[1],rf.peter2014$dat.95pct[1],
                           rf.peter2015$dat.95pct[1],rf.paul2013$dat.95pct[1],
                           rf.paul2014$dat.95pct[1],rf.paul2015$dat.95pct[1],
                           rf.tuesday2013$dat.95pct[1],rf.tuesday2014$dat.95pct[1],
                           rf.tuesday2015$dat.95pct[1],rf.grnval2014$dat.95pct[1],
                           rf.grnval2015$dat.95pct[1],rf.grnval2019$dat.95pct[1],
                           rf.sotwin2018$dat.95pct[1],rf.sotwin2019$dat.95pct[1],
                           rf.blkhwk2015$dat.95pct[1],rf.swan2018$dat.95pct[1],
                           rf.bcr2014$dat.95pct[1],rf.bcr2015$dat.95pct[1],
                           rf.bcr2017$dat.95pct[1],rf.mendota2006$dat.95pct[1],
                           rf.mendota2007$dat.95pct[1],rf.mendota2008$dat.95pct[1],
                           rf.mendota2009$dat.95pct[1],rf.mendota2010$dat.95pct[1],
                           rf.mendota2011$dat.95pct[1],rf.mendota2012$dat.95pct[1],
                           rf.mendota2013$dat.95pct[1],#rf.mendota2014$dat.95pct[1],
                           rf.mendota2015$dat.95pct[1],rf.mendota2016$dat.95pct[1],
                           rf.mendota2017$dat.95pct[1],rf.mendota2018$dat.95pct[1]
                      ),
                      d7.rmse=c(rmse(rf.peter2013$dat.95pct[1],rf.peter2013$dat.95pct[rf.peter2013$interval==7]),
                                rmse(rf.peter2014$dat.95pct[1],rf.peter2014$dat.95pct[rf.peter2014$interval==7]),
                                rmse(rf.peter2015$dat.95pct[1],rf.peter2015$dat.95pct[rf.peter2015$interval==7]),
                                rmse(rf.paul2013$dat.95pct[1],rf.paul2013$dat.95pct[rf.paul2013$interval==7]),
                                rmse(rf.paul2014$dat.95pct[1],rf.paul2014$dat.95pct[rf.paul2014$interval==7]),
                                rmse(rf.paul2015$dat.95pct[1],rf.paul2015$dat.95pct[rf.paul2015$interval==7]),
                                rmse(rf.tuesday2013$dat.95pct[1],rf.tuesday2013$dat.95pct[rf.tuesday2013$interval==7]),
                                rmse(rf.tuesday2014$dat.95pct[1],rf.tuesday2014$dat.95pct[rf.tuesday2014$interval==7]),
                                rmse(rf.tuesday2015$dat.95pct[1],rf.tuesday2015$dat.95pct[rf.tuesday2015$interval==7]),
                                rmse(rf.grnval2014$dat.95pct[1],rf.grnval2014$dat.95pct[rf.grnval2014$interval==7]),
                                rmse(rf.grnval2015$dat.95pct[1],rf.grnval2015$dat.95pct[rf.grnval2015$interval==7]),
                                rmse(rf.grnval2019$dat.95pct[1],rf.grnval2019$dat.95pct[rf.grnval2019$interval==7]),
                                rmse(rf.sotwin2018$dat.95pct[1],rf.sotwin2018$dat.95pct[rf.sotwin2018$interval==7]),
                                rmse(rf.sotwin2019$dat.95pct[1],rf.sotwin2019$dat.95pct[rf.sotwin2019$interval==7]),
                                rmse(rf.blkhwk2015$dat.95pct[1],rf.blkhwk2015$dat.95pct[rf.blkhwk2015$interval==7]),
                                rmse(rf.swan2018$dat.95pct[1],rf.swan2018$dat.95pct[rf.swan2018$interval==7]),
                                rmse(rf.bcr2014$dat.95pct[1],rf.bcr2014$dat.95pct[rf.bcr2014$interval==7]),
                                rmse(rf.bcr2015$dat.95pct[1],rf.bcr2015$dat.95pct[rf.bcr2015$interval==7]),
                                rmse(rf.bcr2017$dat.95pct[1],rf.bcr2017$dat.95pct[rf.bcr2017$interval==7]),
                                rmse(rf.mendota2006$dat.95pct[1],rf.mendota2006$dat.95pct[rf.mendota2006$interval==7]),
                                rmse(rf.mendota2007$dat.95pct[1],rf.mendota2007$dat.95pct[rf.mendota2007$interval==7]),
                                rmse(rf.mendota2008$dat.95pct[1],rf.mendota2008$dat.95pct[rf.mendota2008$interval==7]),
                                rmse(rf.mendota2009$dat.95pct[1],rf.mendota2009$dat.95pct[rf.mendota2009$interval==7]),
                                rmse(rf.mendota2010$dat.95pct[1],rf.mendota2010$dat.95pct[rf.mendota2010$interval==7]),
                                rmse(rf.mendota2011$dat.95pct[1],rf.mendota2011$dat.95pct[rf.mendota2011$interval==7]),
                                rmse(rf.mendota2012$dat.95pct[1],rf.mendota2012$dat.95pct[rf.mendota2012$interval==7]),
                                rmse(rf.mendota2013$dat.95pct[1],rf.mendota2013$dat.95pct[rf.mendota2013$interval==7]),
                                #rmse(rf.mendota2014$dat.95pct[1],rf.mendota2014$dat.95pct[rf.mendota2014$interval==7]),
                                rmse(rf.mendota2015$dat.95pct[1],rf.mendota2015$dat.95pct[rf.mendota2015$interval==7]),
                                rmse(rf.mendota2016$dat.95pct[1],rf.mendota2016$dat.95pct[rf.mendota2016$interval==7]),
                                rmse(rf.mendota2017$dat.95pct[1],rf.mendota2017$dat.95pct[rf.mendota2017$interval==7]),
                                rmse(rf.mendota2018$dat.95pct[1],rf.mendota2018$dat.95pct[rf.mendota2018$interval==7])
                      ),
                      d14.rmse=c(rmse(rf.peter2013$dat.95pct[1],rf.peter2013$dat.95pct[rf.peter2013$interval==14]),
                                 rmse(rf.peter2014$dat.95pct[1],rf.peter2014$dat.95pct[rf.peter2014$interval==14]),
                                 rmse(rf.peter2015$dat.95pct[1],rf.peter2015$dat.95pct[rf.peter2015$interval==14]),
                                 rmse(rf.paul2013$dat.95pct[1],rf.paul2013$dat.95pct[rf.paul2013$interval==14]),
                                 rmse(rf.paul2014$dat.95pct[1],rf.paul2014$dat.95pct[rf.paul2014$interval==14]),
                                 rmse(rf.paul2015$dat.95pct[1],rf.paul2015$dat.95pct[rf.paul2015$interval==14]),
                                 rmse(rf.tuesday2013$dat.95pct[1],rf.tuesday2013$dat.95pct[rf.tuesday2013$interval==14]),
                                 rmse(rf.tuesday2014$dat.95pct[1],rf.tuesday2014$dat.95pct[rf.tuesday2014$interval==14]),
                                 rmse(rf.tuesday2015$dat.95pct[1],rf.tuesday2015$dat.95pct[rf.tuesday2015$interval==14]),
                                 rmse(rf.grnval2014$dat.95pct[1],rf.grnval2014$dat.95pct[rf.grnval2014$interval==14]),
                                 rmse(rf.grnval2015$dat.95pct[1],rf.grnval2015$dat.95pct[rf.grnval2015$interval==14]),
                                 rmse(rf.grnval2019$dat.95pct[1],rf.grnval2019$dat.95pct[rf.grnval2019$interval==14]),
                                 rmse(rf.sotwin2018$dat.95pct[1],rf.sotwin2018$dat.95pct[rf.sotwin2018$interval==14]),
                                 rmse(rf.sotwin2019$dat.95pct[1],rf.sotwin2019$dat.95pct[rf.sotwin2019$interval==14]),
                                 rmse(rf.blkhwk2015$dat.95pct[1],rf.blkhwk2015$dat.95pct[rf.blkhwk2015$interval==14]),
                                 rmse(rf.swan2018$dat.95pct[1],rf.swan2018$dat.95pct[rf.swan2018$interval==14]),
                                 rmse(rf.bcr2014$dat.95pct[1],rf.bcr2014$dat.95pct[rf.bcr2014$interval==14]),
                                 rmse(rf.bcr2015$dat.95pct[1],rf.bcr2015$dat.95pct[rf.bcr2015$interval==14]),
                                 rmse(rf.bcr2014$dat.95pct[1],rf.bcr2017$dat.95pct[rf.bcr2017$interval==14]),
                                 rmse(rf.mendota2006$dat.95pct[1],rf.mendota2006$dat.95pct[rf.mendota2006$interval==14]),
                                 rmse(rf.mendota2007$dat.95pct[1],rf.mendota2007$dat.95pct[rf.mendota2007$interval==14]),
                                 rmse(rf.mendota2008$dat.95pct[1],rf.mendota2008$dat.95pct[rf.mendota2008$interval==14]),
                                 rmse(rf.mendota2009$dat.95pct[1],rf.mendota2009$dat.95pct[rf.mendota2009$interval==14]),
                                 rmse(rf.mendota2010$dat.95pct[1],rf.mendota2010$dat.95pct[rf.mendota2010$interval==14]),
                                 rmse(rf.mendota2011$dat.95pct[1],rf.mendota2011$dat.95pct[rf.mendota2011$interval==14]),
                                 rmse(rf.mendota2012$dat.95pct[1],rf.mendota2012$dat.95pct[rf.mendota2012$interval==14]),
                                 rmse(rf.mendota2013$dat.95pct[1],rf.mendota2013$dat.95pct[rf.mendota2013$interval==14]),
                                 #rmse(rf.mendota2014$dat.95pct[1],rf.mendota2014$dat.95pct[rf.mendota2014$interval==14]),
                                 rmse(rf.mendota2015$dat.95pct[1],rf.mendota2015$dat.95pct[rf.mendota2015$interval==14]),
                                 rmse(rf.mendota2016$dat.95pct[1],rf.mendota2016$dat.95pct[rf.mendota2016$interval==14]),
                                 rmse(rf.mendota2017$dat.95pct[1],rf.mendota2017$dat.95pct[rf.mendota2017$interval==14]),
                                 rmse(rf.mendota2018$dat.95pct[1],rf.mendota2018$dat.95pct[rf.mendota2018$interval==14])
                      )
)

rmse.grtr<-data.frame(lakeyr=lakeyrs,
                      d1=c(rf.peter2013$dat.grtr[1],rf.peter2014$dat.grtr[1],
                           rf.peter2015$dat.grtr[1],rf.paul2013$dat.grtr[1],
                           rf.paul2014$dat.grtr[1],rf.paul2015$dat.grtr[1],
                           rf.tuesday2013$dat.grtr[1],rf.tuesday2014$dat.grtr[1],
                           rf.tuesday2015$dat.grtr[1],rf.grnval2014$dat.grtr[1],
                           rf.grnval2015$dat.grtr[1],rf.grnval2019$dat.grtr[1],
                           rf.sotwin2018$dat.grtr[1],rf.sotwin2019$dat.grtr[1],
                           rf.blkhwk2015$dat.grtr[1],rf.swan2018$dat.grtr[1],
                           rf.bcr2014$dat.grtr[1],rf.bcr2015$dat.grtr[1],
                           rf.bcr2017$dat.grtr[1],rf.mendota2006$dat.grtr[1],
                           rf.mendota2007$dat.grtr[1],rf.mendota2008$dat.grtr[1],
                           rf.mendota2009$dat.grtr[1],rf.mendota2010$dat.grtr[1],
                           rf.mendota2011$dat.grtr[1],rf.mendota2012$dat.grtr[1],
                           rf.mendota2013$dat.grtr[1],#rf.mendota2014$dat.grtr[1],
                           rf.mendota2015$dat.grtr[1],rf.mendota2016$dat.grtr[1],
                           rf.mendota2017$dat.grtr[1],rf.mendota2018$dat.grtr[1]
                      ),
                      d7.rmse=c(rmse(rf.peter2013$dat.grtr[1],rf.peter2013$dat.grtr[rf.peter2013$interval==7]),
                                rmse(rf.peter2014$dat.grtr[1],rf.peter2014$dat.grtr[rf.peter2014$interval==7]),
                                rmse(rf.peter2015$dat.grtr[1],rf.peter2015$dat.grtr[rf.peter2015$interval==7]),
                                rmse(rf.paul2013$dat.grtr[1],rf.paul2013$dat.grtr[rf.paul2013$interval==7]),
                                rmse(rf.paul2014$dat.grtr[1],rf.paul2014$dat.grtr[rf.paul2014$interval==7]),
                                rmse(rf.paul2015$dat.grtr[1],rf.paul2015$dat.grtr[rf.paul2015$interval==7]),
                                rmse(rf.tuesday2013$dat.grtr[1],rf.tuesday2013$dat.grtr[rf.tuesday2013$interval==7]),
                                rmse(rf.tuesday2014$dat.grtr[1],rf.tuesday2014$dat.grtr[rf.tuesday2014$interval==7]),
                                rmse(rf.tuesday2015$dat.grtr[1],rf.tuesday2015$dat.grtr[rf.tuesday2015$interval==7]),
                                rmse(rf.grnval2014$dat.grtr[1],rf.grnval2014$dat.grtr[rf.grnval2014$interval==7]),
                                rmse(rf.grnval2015$dat.grtr[1],rf.grnval2015$dat.grtr[rf.grnval2015$interval==7]),
                                rmse(rf.grnval2019$dat.grtr[1],rf.grnval2019$dat.grtr[rf.grnval2019$interval==7]),
                                rmse(rf.sotwin2018$dat.grtr[1],rf.sotwin2018$dat.grtr[rf.sotwin2018$interval==7]),
                                rmse(rf.sotwin2019$dat.grtr[1],rf.sotwin2019$dat.grtr[rf.sotwin2019$interval==7]),
                                rmse(rf.blkhwk2015$dat.grtr[1],rf.blkhwk2015$dat.grtr[rf.blkhwk2015$interval==7]),
                                rmse(rf.swan2018$dat.grtr[1],rf.swan2018$dat.grtr[rf.swan2018$interval==7]),
                                rmse(rf.bcr2014$dat.grtr[1],rf.bcr2014$dat.grtr[rf.bcr2014$interval==7]),
                                rmse(rf.bcr2015$dat.grtr[1],rf.bcr2015$dat.grtr[rf.bcr2015$interval==7]),
                                rmse(rf.bcr2017$dat.grtr[1],rf.bcr2017$dat.grtr[rf.bcr2017$interval==7]),
                                rmse(rf.mendota2006$dat.grtr[1],rf.mendota2006$dat.grtr[rf.mendota2006$interval==7]),
                                rmse(rf.mendota2007$dat.grtr[1],rf.mendota2007$dat.grtr[rf.mendota2007$interval==7]),
                                rmse(rf.mendota2008$dat.grtr[1],rf.mendota2008$dat.grtr[rf.mendota2008$interval==7]),
                                rmse(rf.mendota2009$dat.grtr[1],rf.mendota2009$dat.grtr[rf.mendota2009$interval==7]),
                                rmse(rf.mendota2010$dat.grtr[1],rf.mendota2010$dat.grtr[rf.mendota2010$interval==7]),
                                rmse(rf.mendota2011$dat.grtr[1],rf.mendota2011$dat.grtr[rf.mendota2011$interval==7]),
                                rmse(rf.mendota2012$dat.grtr[1],rf.mendota2012$dat.grtr[rf.mendota2012$interval==7]),
                                rmse(rf.mendota2013$dat.grtr[1],rf.mendota2013$dat.grtr[rf.mendota2013$interval==7]),
                                #rmse(rf.mendota2014$dat.grtr[1],rf.mendota2014$dat.grtr[rf.mendota2014$interval==7]),
                                rmse(rf.mendota2015$dat.grtr[1],rf.mendota2015$dat.grtr[rf.mendota2015$interval==7]),
                                rmse(rf.mendota2016$dat.grtr[1],rf.mendota2016$dat.grtr[rf.mendota2016$interval==7]),
                                rmse(rf.mendota2017$dat.grtr[1],rf.mendota2017$dat.grtr[rf.mendota2017$interval==7]),
                                rmse(rf.mendota2018$dat.grtr[1],rf.mendota2018$dat.grtr[rf.mendota2018$interval==7])
                      ),
                      d14.rmse=c(rmse(rf.peter2013$dat.grtr[1],rf.peter2013$dat.grtr[rf.peter2013$interval==14]),
                                 rmse(rf.peter2014$dat.grtr[1],rf.peter2014$dat.grtr[rf.peter2014$interval==14]),
                                 rmse(rf.peter2015$dat.grtr[1],rf.peter2015$dat.grtr[rf.peter2015$interval==14]),
                                 rmse(rf.paul2013$dat.grtr[1],rf.paul2013$dat.grtr[rf.paul2013$interval==14]),
                                 rmse(rf.paul2014$dat.grtr[1],rf.paul2014$dat.grtr[rf.paul2014$interval==14]),
                                 rmse(rf.paul2015$dat.grtr[1],rf.paul2015$dat.grtr[rf.paul2015$interval==14]),
                                 rmse(rf.tuesday2013$dat.grtr[1],rf.tuesday2013$dat.grtr[rf.tuesday2013$interval==14]),
                                 rmse(rf.tuesday2014$dat.grtr[1],rf.tuesday2014$dat.grtr[rf.tuesday2014$interval==14]),
                                 rmse(rf.tuesday2015$dat.grtr[1],rf.tuesday2015$dat.grtr[rf.tuesday2015$interval==14]),
                                 rmse(rf.grnval2014$dat.grtr[1],rf.grnval2014$dat.grtr[rf.grnval2014$interval==14]),
                                 rmse(rf.grnval2015$dat.grtr[1],rf.grnval2015$dat.grtr[rf.grnval2015$interval==14]),
                                 rmse(rf.grnval2019$dat.grtr[1],rf.grnval2019$dat.grtr[rf.grnval2019$interval==14]),
                                 rmse(rf.sotwin2018$dat.grtr[1],rf.sotwin2018$dat.grtr[rf.sotwin2018$interval==14]),
                                 rmse(rf.sotwin2019$dat.grtr[1],rf.sotwin2019$dat.grtr[rf.sotwin2019$interval==14]),
                                 rmse(rf.blkhwk2015$dat.grtr[1],rf.blkhwk2015$dat.grtr[rf.blkhwk2015$interval==14]),
                                 rmse(rf.swan2018$dat.grtr[1],rf.swan2018$dat.grtr[rf.swan2018$interval==14]),
                                 rmse(rf.bcr2014$dat.grtr[1],rf.bcr2014$dat.grtr[rf.bcr2014$interval==14]),
                                 rmse(rf.bcr2015$dat.grtr[1],rf.bcr2015$dat.grtr[rf.bcr2015$interval==14]),
                                 rmse(rf.bcr2014$dat.grtr[1],rf.bcr2017$dat.grtr[rf.bcr2017$interval==14]),
                                 rmse(rf.mendota2006$dat.grtr[1],rf.mendota2006$dat.grtr[rf.mendota2006$interval==14]),
                                 rmse(rf.mendota2007$dat.grtr[1],rf.mendota2007$dat.grtr[rf.mendota2007$interval==14]),
                                 rmse(rf.mendota2008$dat.grtr[1],rf.mendota2008$dat.grtr[rf.mendota2008$interval==14]),
                                 rmse(rf.mendota2009$dat.grtr[1],rf.mendota2009$dat.grtr[rf.mendota2009$interval==14]),
                                 rmse(rf.mendota2010$dat.grtr[1],rf.mendota2010$dat.grtr[rf.mendota2010$interval==14]),
                                 rmse(rf.mendota2011$dat.grtr[1],rf.mendota2011$dat.grtr[rf.mendota2011$interval==14]),
                                 rmse(rf.mendota2012$dat.grtr[1],rf.mendota2012$dat.grtr[rf.mendota2012$interval==14]),
                                 rmse(rf.mendota2013$dat.grtr[1],rf.mendota2013$dat.grtr[rf.mendota2013$interval==14]),
                                 #rmse(rf.mendota2014$dat.grtr[1],rf.mendota2014$dat.grtr[rf.mendota2014$interval==14]),
                                 rmse(rf.mendota2015$dat.grtr[1],rf.mendota2015$dat.grtr[rf.mendota2015$interval==14]),
                                 rmse(rf.mendota2016$dat.grtr[1],rf.mendota2016$dat.grtr[rf.mendota2016$interval==14]),
                                 rmse(rf.mendota2017$dat.grtr[1],rf.mendota2017$dat.grtr[rf.mendota2017$interval==14]),
                                 rmse(rf.mendota2018$dat.grtr[1],rf.mendota2018$dat.grtr[rf.mendota2018$interval==14])
                      )
)



write.csv(rmse.mean, "rmse_mean.csv", row.names = F)
write.csv(rmse.95pct, "rmse_95pct.csv", row.names = F)
write.csv(rmse.grtr, "rmse_grtr.csv", row.names = F)

## rmse/true value

loop.rmse<-function(rf,freq){
  out<-0
  true<-rf[1]
  for(ii in 2:max(freq)){
    out<-c(out, rmse(true,rf[freq==ii])/true)
  }
  return(out)
}

rmseratio.mean<-rbind(loop.rmse(rf.peter2013$dat.mean,rf.peter2013$interval),
                      loop.rmse(rf.peter2014$dat.mean,rf.peter2014$interval),
                      loop.rmse(rf.peter2015$dat.mean,rf.peter2015$interval),
                      loop.rmse(rf.paul2013$dat.mean,rf.paul2013$interval),
                      loop.rmse(rf.paul2014$dat.mean,rf.paul2014$interval),
                      loop.rmse(rf.paul2015$dat.mean,rf.paul2015$interval),
                      loop.rmse(rf.tuesday2013$dat.mean,rf.tuesday2013$interval),
                      loop.rmse(rf.tuesday2014$dat.mean,rf.tuesday2014$interval),
                      loop.rmse(rf.tuesday2015$dat.mean,rf.tuesday2015$interval),
                      loop.rmse(rf.grnval2014$dat.mean,rf.grnval2014$interval),
                      loop.rmse(rf.grnval2015$dat.mean,rf.grnval2015$interval),
                      loop.rmse(rf.grnval2019$dat.mean,rf.grnval2019$interval),
                      loop.rmse(rf.sotwin2018$dat.mean,rf.sotwin2018$interval),
                      loop.rmse(rf.sotwin2019$dat.mean,rf.sotwin2019$interval),
                      loop.rmse(rf.blkhwk2015$dat.mean,rf.blkhwk2015$interval),
                      loop.rmse(rf.swan2018$dat.mean,rf.swan2018$interval),
                      loop.rmse(rf.bcr2014$dat.mean,rf.bcr2014$interval),
                      loop.rmse(rf.bcr2015$dat.mean,rf.bcr2015$interval),
                      loop.rmse(rf.bcr2017$dat.mean,rf.bcr2017$interval),
                      loop.rmse(rf.mendota2006$dat.mean,rf.mendota2006$interval),
                      loop.rmse(rf.mendota2007$dat.mean,rf.mendota2007$interval),
                      loop.rmse(rf.mendota2008$dat.mean,rf.mendota2008$interval),
                      loop.rmse(rf.mendota2009$dat.mean,rf.mendota2009$interval),
                      loop.rmse(rf.mendota2010$dat.mean,rf.mendota2010$interval),
                      loop.rmse(rf.mendota2011$dat.mean,rf.mendota2011$interval),
                      loop.rmse(rf.mendota2012$dat.mean,rf.mendota2012$interval),
                      loop.rmse(rf.mendota2013$dat.mean,rf.mendota2013$interval),
                      loop.rmse(rf.mendota2015$dat.mean,rf.mendota2015$interval),
                      loop.rmse(rf.mendota2016$dat.mean,rf.mendota2016$interval),
                      loop.rmse(rf.mendota2017$dat.mean,rf.mendota2017$interval),
                      loop.rmse(rf.mendota2018$dat.mean,rf.mendota2018$interval))

rmseratio.95pct<-rbind(loop.rmse(rf.peter2013$dat.95pct,rf.peter2013$interval),
                      loop.rmse(rf.peter2014$dat.95pct,rf.peter2014$interval),
                      loop.rmse(rf.peter2015$dat.95pct,rf.peter2015$interval),
                      loop.rmse(rf.paul2013$dat.95pct,rf.paul2013$interval),
                      loop.rmse(rf.paul2014$dat.95pct,rf.paul2014$interval),
                      loop.rmse(rf.paul2015$dat.95pct,rf.paul2015$interval),
                      loop.rmse(rf.tuesday2013$dat.95pct,rf.tuesday2013$interval),
                      loop.rmse(rf.tuesday2014$dat.95pct,rf.tuesday2014$interval),
                      loop.rmse(rf.tuesday2015$dat.95pct,rf.tuesday2015$interval),
                      loop.rmse(rf.grnval2014$dat.95pct,rf.grnval2014$interval),
                      loop.rmse(rf.grnval2015$dat.95pct,rf.grnval2015$interval),
                      loop.rmse(rf.grnval2019$dat.95pct,rf.grnval2019$interval),
                      loop.rmse(rf.sotwin2018$dat.95pct,rf.sotwin2018$interval),
                      loop.rmse(rf.sotwin2019$dat.95pct,rf.sotwin2019$interval),
                      loop.rmse(rf.blkhwk2015$dat.95pct,rf.blkhwk2015$interval),
                      loop.rmse(rf.swan2018$dat.95pct,rf.swan2018$interval),
                      loop.rmse(rf.bcr2014$dat.95pct,rf.bcr2014$interval),
                      loop.rmse(rf.bcr2015$dat.95pct,rf.bcr2015$interval),
                      loop.rmse(rf.bcr2017$dat.95pct,rf.bcr2017$interval),
                      loop.rmse(rf.mendota2006$dat.95pct,rf.mendota2006$interval),
                      loop.rmse(rf.mendota2007$dat.95pct,rf.mendota2007$interval),
                      loop.rmse(rf.mendota2008$dat.95pct,rf.mendota2008$interval),
                      loop.rmse(rf.mendota2009$dat.95pct,rf.mendota2009$interval),
                      loop.rmse(rf.mendota2010$dat.95pct,rf.mendota2010$interval),
                      loop.rmse(rf.mendota2011$dat.95pct,rf.mendota2011$interval),
                      loop.rmse(rf.mendota2012$dat.95pct,rf.mendota2012$interval),
                      loop.rmse(rf.mendota2013$dat.95pct,rf.mendota2013$interval),
                      loop.rmse(rf.mendota2015$dat.95pct,rf.mendota2015$interval),
                      loop.rmse(rf.mendota2016$dat.95pct,rf.mendota2016$interval),
                      loop.rmse(rf.mendota2017$dat.95pct,rf.mendota2017$interval),
                      loop.rmse(rf.mendota2018$dat.95pct,rf.mendota2018$interval))

rmseratio.grtr<-rbind(loop.rmse(rf.peter2013$dat.grtr,rf.peter2013$interval),
                      loop.rmse(rf.peter2014$dat.grtr,rf.peter2014$interval),
                      loop.rmse(rf.peter2015$dat.grtr,rf.peter2015$interval),
                      loop.rmse(rf.paul2013$dat.grtr,rf.paul2013$interval),
                      loop.rmse(rf.paul2014$dat.grtr,rf.paul2014$interval),
                      loop.rmse(rf.paul2015$dat.grtr,rf.paul2015$interval),
                      loop.rmse(rf.tuesday2013$dat.grtr,rf.tuesday2013$interval),
                      loop.rmse(rf.tuesday2014$dat.grtr,rf.tuesday2014$interval),
                      loop.rmse(rf.tuesday2015$dat.grtr,rf.tuesday2015$interval),
                      loop.rmse(rf.grnval2014$dat.grtr,rf.grnval2014$interval),
                      loop.rmse(rf.grnval2015$dat.grtr,rf.grnval2015$interval),
                      loop.rmse(rf.grnval2019$dat.grtr,rf.grnval2019$interval),
                      loop.rmse(rf.sotwin2018$dat.grtr,rf.sotwin2018$interval),
                      loop.rmse(rf.sotwin2019$dat.grtr,rf.sotwin2019$interval),
                      loop.rmse(rf.blkhwk2015$dat.grtr,rf.blkhwk2015$interval),
                      loop.rmse(rf.swan2018$dat.grtr,rf.swan2018$interval),
                      loop.rmse(rf.bcr2014$dat.grtr,rf.bcr2014$interval),
                      loop.rmse(rf.bcr2015$dat.grtr,rf.bcr2015$interval),
                      loop.rmse(rf.bcr2017$dat.grtr,rf.bcr2017$interval),
                      loop.rmse(rf.mendota2006$dat.grtr,rf.mendota2006$interval),
                      loop.rmse(rf.mendota2007$dat.grtr,rf.mendota2007$interval),
                      loop.rmse(rf.mendota2008$dat.grtr,rf.mendota2008$interval),
                      loop.rmse(rf.mendota2009$dat.grtr,rf.mendota2009$interval),
                      loop.rmse(rf.mendota2010$dat.grtr,rf.mendota2010$interval),
                      loop.rmse(rf.mendota2011$dat.grtr,rf.mendota2011$interval),
                      loop.rmse(rf.mendota2012$dat.grtr,rf.mendota2012$interval),
                      loop.rmse(rf.mendota2013$dat.grtr,rf.mendota2013$interval),
                      loop.rmse(rf.mendota2015$dat.grtr,rf.mendota2015$interval),
                      loop.rmse(rf.mendota2016$dat.grtr,rf.mendota2016$interval),
                      loop.rmse(rf.mendota2017$dat.grtr,rf.mendota2017$interval),
                      loop.rmse(rf.mendota2018$dat.grtr,rf.mendota2018$interval)
)

#plotting
tiff("rmse_over_true.tif",units="in",width=3.5,height=4.5,res=300)

durationcol <- rgb(102,102,102, max = 255, alpha = 70)
severitycol <- rgb(16,78,139, max = 255, alpha = 70)
intensecol <- rgb(0,139,139, max = 255, alpha = 70)

par(mfrow=c(3,1), mar=c(3.1,3.1,1.1,1.1),mgp=c(1.6,0.5,0),tcl=-0.3)

#Intensity
plot(1:30,apply(rmseratio.mean,2,median,na.rm=T),type="l",ylim=range(rmseratio.mean,na.rm=T),
     xlab="Sampling interval (days)", ylab="RMSE/true value")
for(ii in 1:nrow(rmseratio.mean)){
  lines(1:30,rmseratio.mean[ii,],col=intensecol)
}
lines(1:30,apply(rmseratio.mean,2,median,na.rm=T),lwd=3, col="darkcyan")
mtext("Intensity (Mean Annual Chlorophyll)",3,cex=0.7)

# Severity
plot(1:30,apply(rmseratio.95pct,2,median,na.rm=T),type="l",ylim=range(rmseratio.95pct,na.rm=T),
     xlab="Sampling interval (days)", ylab="RMSE/true value")
for(ii in 1:nrow(rmseratio.95pct)){
  lines(1:30,rmseratio.95pct[ii,],col=severitycol)
}
lines(1:30,apply(rmseratio.95pct,2,median,na.rm=T),lwd=3, col="dodgerblue4")
mtext("Severity (95th percentile Chlorophyll)",3,cex=0.7)

# Duration
plot(1:30,apply(rmseratio.grtr,2,median,na.rm=T),type="l",ylim=range(rmseratio.grtr,na.rm=T),
     xlab="Sampling interval (days)", ylab="RMSE/true value")
for(ii in 1:nrow(rmseratio.grtr)){
  lines(1:30,rmseratio.grtr[ii,],col=durationcol)
}
lines(1:30,apply(rmseratio.grtr,2,median,na.rm=T),lwd=3, col="grey30")
mtext("Duration (Proportion of values > threshold)",3,cex=0.7)

dev.off()
