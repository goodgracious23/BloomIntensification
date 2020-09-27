## Wilkinson et al. FE&E
## Create and export dataset based on length criteria
## Written by Jon Walter - last updated 8 March 2020
## NOTE: file "hab_trend_lakeinfo.csv" created from this script which feeds into the next analysis step

if (!require(lubridate)) install.packages('lubridate')
library(lubridate)
if (!require(rgdal)) install.packages('rgdal')
library(rgdal)
if (!require(LAGOSNE)) install.packages('LAGOSNE')
library(LAGOSNE)
if (!require(readr)) install.packages('readr')
library(readr)
if (!require(stringr)) install.packages('stringr')
library(stringr)
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
if (!require(raster)) install.packages('raster')
library(raster)


## LAGOS-NE Lakes -------------------------------------------------------------
dt<-lagosne_load(version = "1.087.3")

lakeinfo<-lagosne_select(table="locus", vars=c("lagoslakeid","gnis_name","nhd_lat","nhd_long",
                                               "lake_area_ha","lake_perim_meters"))


vars.raw<-lagosne_select(table="epi_nutr", vars=c("lagoslakeid","sampledate","chla"))
vars.raw<-vars.raw[complete.cases(vars.raw),]


vars<-vars.raw #make a copy to manipulate
#vars<-vars[complete.cases(vars),] #remove obs with missing data
vars$sampledate<-as.POSIXct(vars$sampledate, format="%m/%d/%Y")
vars<-vars[month(vars$sampledate)>=5 & month(vars$sampledate)<=9,] #focus on May through October
vars$source<-rep("LAGOS-NE", nrow(vars))

## MN Lakes -------------------------------------------------------------------
MNfiles<-list.files("./LAGOS Extended/MN_MPCA")
MNids<-parse_number(MNfiles)

#options(warn=1)

for(file in MNfiles){
  dat.ff<-read.csv(paste0("./LAGOS Extended/MN_MPCA/",file), stringsAsFactors = F, na.strings = "(null)")
  dat.ff<-dat.ff[dat.ff$parameter=="Chlorophyll a, corrected for pheophytin", colnames(dat.ff) %in% c("result","sampleDate")]
  daf.ff<-dat.ff[complete.cases(dat.ff),]
  if(nrow(dat.ff)==0){next}
  if(sum(vars$lagoslakeid==parse_number(file))==0){next}
  #fix date format and convert to POSICXct
  date.tmp<-simplify2array(strsplit(as.character(dat.ff$sampleDate),"/")) #reformat the sampledate columnn
  dat.ff$sampleDate<-paste(str_pad(date.tmp[1,],width=2,pad=0),
                           str_pad(date.tmp[2,],width=2,pad=0),
                           date.tmp[3,],sep="/")
  dat.ff$sampleDate<-as.POSIXct(dat.ff$sampleDate,format="%m/%d/%Y")
  dat.ff<-dat.ff[order(dat.ff$sampleDate),]
  names(dat.ff)<-c("chla","sampledate")
  dat.ff<-dat.ff[month(dat.ff$sampledate)>=5 & month(dat.ff$sampledate)<=9,]
  #check if this would extend the dataset
  lagos.ff<-vars[vars$lagoslakeid == parse_number(file),]
  if(max(dat.ff$sampledate)>max(lagos.ff$sampledate)){
    dappend<-dat.ff[dat.ff$sampledate>max(lagos.ff$sampledate),]
    dappend$lagoslakeid<-rep(parse_number(file),nrow(dappend))
    dappend<-dappend[c("lagoslakeid","sampledate","chla")]
    dappend$source<-rep("MN_MPCA",nrow(dappend))
    vars<-rbind(vars,dappend)
  }
}

## WI Lakes -------------------------------------------------------------------
WIfiles<-list.files("./LAGOS Extended/WI_DNR")
WIids<-parse_number(WIfiles)

for(file in WIfiles){
  dat.ff<-read.csv(paste0("./LAGOS Extended/WI_DNR/",file))
  dat.ff<-dat.ff[,colnames(dat.ff) %in% c("Start.Date","Chlorophyll.ug.l.")]
  dat.ff<-dat.ff[complete.cases(dat.ff),]
  #fix date format and convert to POSICXct
  date.tmp<-simplify2array(strsplit(as.character(dat.ff$Start.Date),"/")) #reformat the sampledate columnn
  date.tmp[3,as.numeric(date.tmp[3,])>50]<-paste0("19",date.tmp[3,as.numeric(date.tmp[3,])>50])
  date.tmp[3,as.numeric(date.tmp[3,])<50]<-paste0("20",date.tmp[3,as.numeric(date.tmp[3,])<50])
  dat.ff$Start.Date<-paste(str_pad(date.tmp[1,],width=2,pad=0),
                           str_pad(date.tmp[2,],width=2,pad=0),
                           date.tmp[3,],sep="/")
  dat.ff$Start.Date<-as.POSIXct(dat.ff$Start.Date,format="%m/%d/%Y")
  dat.ff<-dat.ff[order(dat.ff$Start.Date),]
  names(dat.ff)<-c("sampledate","chla")
  dat.ff<-dat.ff[month(dat.ff$sampledate)>=5 & month(dat.ff$sampledate)<=9,]
  #check if this would extend the dataset
  lagos.ff<-vars[vars$lagoslakeid == parse_number(file),]
  if(max(dat.ff$sampledate)>max(lagos.ff$sampledate)){
    dappend<-dat.ff[dat.ff$sampledate>max(lagos.ff$sampledate),]
    dappend$lagoslakeid<-rep(parse_number(file),nrow(dappend))
    dappend<-dappend[c("lagoslakeid","sampledate","chla")]
    dappend$source<-rep("WI_DNR",nrow(dappend))
    vars<-rbind(vars,dappend)
  }
}

## VT Lakes ---------------------------------------------------------------------------------------
VTfiles<-list.files("./LAGOS Extended/Vermont")
VTids<-parse_number(VTfiles)

for(file in VTfiles){
  dat.ff<-read.csv(paste0("./LAGOS Extended/Vermont/",file), stringsAsFactors = F, na.strings = c("(null)","<NA>"))
  dat.ff<-dat.ff[dat.ff$CharacteristicName=="Chlorophyll-a", colnames(dat.ff) %in% c("Result","VisitDate")]
  dat.ff<-dat.ff[complete.cases(dat.ff),]
  if(nrow(dat.ff)==0){next}
  if(sum(vars$lagoslakeid==parse_number(file))==0){next}
  #fix date format and convert to POSICXct
  date.tmp<-simplify2array(strsplit(as.character(dat.ff$VisitDate),"/")) #reformat the sampledate columnn
  dat.ff$VisitDate<-paste(str_pad(date.tmp[1,],width=2,pad=0),
                           str_pad(date.tmp[2,],width=2,pad=0),
                           date.tmp[3,],sep="/")
  dat.ff$VisitDate<-as.POSIXct(dat.ff$VisitDate,format="%m/%d/%Y")
  dat.ff<-dat.ff[order(dat.ff$VisitDate),]
  dat.ff<-aggregate(dat.ff$Result, list(dat.ff$VisitDate),FUN="mean")
  names(dat.ff)<-c("sampledate","chla")
  dat.ff<-dat.ff[month(dat.ff$sampledate)>=5 & month(dat.ff$sampledate)<=9,]
  #check if this would extend the dataset
  lagos.ff<-vars[vars$lagoslakeid == parse_number(file),]
  if(max(dat.ff$sampledate)>max(lagos.ff$sampledate)){
    dappend<-dat.ff[dat.ff$sampledate>max(lagos.ff$sampledate),]
    dappend$lagoslakeid<-rep(parse_number(file),nrow(dappend))
    dappend<-dappend[c("lagoslakeid","sampledate","chla")]
    dappend$source<-rep("Vermont",nrow(dappend))
    vars<-rbind(vars,dappend)
  }
}

## Maine ------------------------------------------------------------------------------------------
maine.raw<-read.csv("./LAGOS Extended/Maine/MaineLakes_Chlorophyll_ByDate.csv", stringsAsFactors=F)
maine.crosswalk<-read.csv("./LAGOS Extended/Maine/Maine_Crosswalk.csv", stringsAsFactors=F)
maine.crosswalk<-maine.crosswalk[-4,]
maine.crosswalk<-maine.crosswalk[colnames(maine.crosswalk) %in% c("lagoslakeid","name_dataset")]

maine.lagos<-left_join(maine.crosswalk, maine.raw, by=c("name_dataset"="Lake"))

for(lake in unique(maine.lagos$lagoslakeid)){
  dat.ff<-maine.lagos[maine.lagos$lagoslakeid==lake,]
  dat.ff<-dat.ff[,colnames(dat.ff) %in% c("Date","CHLA")]
  dat.ff<-dat.ff[complete.cases(dat.ff),]
  #fix date format and convert to POSICXct
  date.tmp<-simplify2array(strsplit(as.character(dat.ff$Date),"/")) #reformat the sampledate columnn
  date.tmp[3,as.numeric(date.tmp[3,])>50]<-paste0("19",date.tmp[3,as.numeric(date.tmp[3,])>50])
  date.tmp[3,as.numeric(date.tmp[3,])<50]<-paste0("20",date.tmp[3,as.numeric(date.tmp[3,])<50])
  dat.ff$Date<-paste(str_pad(date.tmp[1,],width=2,pad=0),
                           str_pad(date.tmp[2,],width=2,pad=0),
                           date.tmp[3,],sep="/")
  dat.ff$Date<-as.POSIXct(dat.ff$Date,format="%m/%d/%Y")
  dat.ff<-dat.ff[order(dat.ff$Date),]
  names(dat.ff)<-c("sampledate","chla")
  dat.ff<-dat.ff[month(dat.ff$sampledate)>=5 & month(dat.ff$sampledate)<=9,]
  #check if this would extend the dataset
  lagos.ff<-vars[vars$lagoslakeid == parse_number(file),]
  if(max(dat.ff$sampledate)>max(lagos.ff$sampledate)){
    dappend<-dat.ff[dat.ff$sampledate>max(lagos.ff$sampledate),]
    dappend$lagoslakeid<-rep(parse_number(file),nrow(dappend))
    dappend<-dappend[c("lagoslakeid","sampledate","chla")]
    dappend$source<-rep("Maine",nrow(dappend))
    vars<-rbind(vars,dappend)
  }
}

## Rhode Island -----------------------------------------------------------------------------------
RI.crosswalk<-read.csv("./LAGOS Extended/Rhode Island/RI_crosswalk.csv", stringsAsFactors=F,na.strings="")
RI.crosswalk<-RI.crosswalk[,colnames(RI.crosswalk) %in% c("lagoslakeid","RI_name")]
RI.crosswalk<-RI.crosswalk[complete.cases(RI.crosswalk),]

RI.2012<-read.csv("./LAGOS Extended/Rhode Island/URIWW-2012.csv", stringsAsFactors=F)
RI.2013<-read.csv("./LAGOS Extended/Rhode Island/URIWW-2013.csv", stringsAsFactors=F)
RI.2014<-read.csv("./LAGOS Extended/Rhode Island/URIWW-2014.csv", stringsAsFactors=F)
RI.2015<-read.csv("./LAGOS Extended/Rhode Island/URIWW-2015.csv", stringsAsFactors=F)
RI.2016<-read.csv("./LAGOS Extended/Rhode Island/URIWW-2016.csv", stringsAsFactors=F)
RI.2017<-read.csv("./LAGOS Extended/Rhode Island/URIWW-2017.csv", stringsAsFactors=F)

RI.2012<-RI.2012[grepl("Chlorophyll a", RI.2012$Parameter), 
                 colnames(RI.2012) %in% c("Location","Date","Concentration")]
RI.2013<-RI.2013[grepl("Chlorophyll a", RI.2013$Parameter), 
                 colnames(RI.2013) %in% c("Location","Date","Concentration")]
RI.2014<-RI.2014[grepl("Chlorophyll a", RI.2014$Parameter), 
                 colnames(RI.2014) %in% c("Location","Date","Concentration")]
RI.2015<-RI.2015[grepl("Chlorophyll a", RI.2015$Parameter), 
                 colnames(RI.2015) %in% c("Location","Date","Concentration")]
RI.2016<-RI.2016[grepl("Chlorophyll a", RI.2016$Parameter), 
                 colnames(RI.2016) %in% c("Location","Date","Concentration")]
RI.2017<-RI.2017[grepl("Chlorophyll a", RI.2017$Parameter..), 
                 colnames(RI.2017) %in% c("Monitoring.location","Date.of.Sample","Concentration")]
colnames(RI.2017)<-c("Date","Concentration","Location")

RI.raw<-rbind(RI.2012,RI.2013,RI.2014,RI.2015,RI.2016,RI.2017)
RI.lagos<-left_join(RI.crosswalk,RI.raw,by=c("RI_name"="Location"))

for(lake in unique(RI.lagos$lagoslakeid)){
  dat.ff<-RI.lagos[RI.lagos$lagoslakeid==lake,]
  dat.ff<-dat.ff[,colnames(dat.ff) %in% c("Date","Concentration")]
  dat.ff<-dat.ff[complete.cases(dat.ff),]
  if(nrow(dat.ff)==0){next}
  #fix date format and convert to POSICXct
  date.tmp<-simplify2array(strsplit(as.character(dat.ff$Date),"/")) #reformat the sampledate columnn
  dat.ff$Date<-paste(str_pad(date.tmp[1,],width=2,pad=0),
                     str_pad(date.tmp[2,],width=2,pad=0),
                     date.tmp[3,],sep="/")
  dat.ff$Date<-as.POSIXct(dat.ff$Date,format="%m/%d/%Y")
  dat.ff<-dat.ff[order(dat.ff$Date),]
  names(dat.ff)<-c("sampledate","chla")
  dat.ff<-dat.ff[month(dat.ff$sampledate)>=5 & month(dat.ff$sampledate)<=9,]
  #check if this would extend the dataset
  lagos.ff<-vars[vars$lagoslakeid == parse_number(file),]
  if(max(dat.ff$sampledate)>max(lagos.ff$sampledate)){
    dappend<-dat.ff[dat.ff$sampledate>max(lagos.ff$sampledate),]
    dappend$lagoslakeid<-rep(parse_number(file),nrow(dappend))
    dappend<-dappend[c("lagoslakeid","sampledate","chla")]
    dappend$source<-rep("Rhode Island",nrow(dappend))
    vars<-rbind(vars,dappend)
  }
}

## New York CSLAP ---------------------------------------------------------------------------------
ny_raw<-read.csv("./LAGOS Extended/NY_CSLAP/NY_CSLAP_2013_2018_20200116.csv", stringsAsFactors=F)
ny<-ny_raw[,-2]
ny$sampledate<-as.POSIXct(ny$sampledate,format="%Y-%m-%d")
ny$source<-rep("NY_CSLAP",nrow(ny))

vars<-rbind(vars,ny)

## Iowa ALM ---------------------------------------------------------------------------------------
coralville.raw<-read.csv("./LAGOS Extended/Iowa/Coralville_Chlorophyll.csv", stringsAsFactors=F)
redrock.raw<-read.csv("./LAGOS Extended/Iowa/RedRock_Chlorophyll.csv", stringsAsFactors=F)
saylorville.raw<-read.csv("./LAGOS Extended/Iowa/Saylorville_Chlorophyll.csv", stringsAsFactors=F)

coralville<-coralville.raw[coralville.raw$Parameter=="Chlorophyll a",]
coralville<-coralville[coralville$Duplicate==0,]
baddate<-simplify2array(strsplit(coralville$SampleDate,split="-"))
baddate[3,as.numeric(baddate[3,])>80]<-paste0("19",baddate[3,as.numeric(baddate[3,])>80])
baddate[3,as.numeric(baddate[3,])<80]<-paste0("20",baddate[3,as.numeric(baddate[3,])<80])
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
coralville$SampleDate<-as.POSIXct(paste(baddate[3,],baddate[2,],baddate[1,],sep="-"))
coralville<-data.frame(lagoslakeid=rep(-11, nrow(coralville)),
                       sampledate=coralville$SampleDate,
                       chla=coralville$Result)
coralville<-coralville[month(coralville$sampledate)>=5 & month(coralville$sampledate)<=9,]
coralville<-aggregate(coralville$chla,by=list(coralville$sampledate),FUN="mean",na.rm=T)
coralville<-data.frame(lagoslakeid=rep(-11, nrow(coralville)),
                       sampledate=coralville$Group.1,
                       chla=coralville$x, source=rep("IA_ALM",nrow(coralville)))

redrock<-redrock.raw
baddate<-simplify2array(strsplit(redrock$DATE_OF_SAMPLE,split="-"))
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
redrock$SampleDate<-as.POSIXct(paste(baddate[3,],baddate[2,],baddate[1,],sep="-"))
redrock<-data.frame(lagoslakeid=rep(-22, nrow(redrock)),
                       sampledate=redrock$SampleDate,
                       chla=redrock$SAMPLE_VALUE,
                    source=rep("IA_ALM",nrow(redrock)))
redrock<-redrock[month(redrock$sampledate)>=5 & month(redrock$sampledate)<=9,] 

saylorville<-saylorville.raw
baddate<-simplify2array(strsplit(saylorville$DATE_OF_SAMPLE,split="-"))
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
saylorville$SampleDate<-as.POSIXct(paste(baddate[3,],baddate[2,],baddate[1,],sep="-"))
saylorville<-data.frame(lagoslakeid=rep(-33, nrow(saylorville)),
                    sampledate=saylorville$SampleDate,
                    chla=saylorville$SAMPLE_VALUE,
                    source=rep("IA_ALM",nrow(saylorville)))
saylorville<-saylorville[month(saylorville$sampledate)>=5 & month(saylorville$sampledate)<=9,] 

vars<-rbind(vars,coralville,redrock,saylorville)

## Iowa CLAMP -------------------------------------------------------------------------------------
IA_CLAMP.raw<-read.csv("./LAGOS Extended/Iowa/Iowa_CLAMP_Lakes.csv", stringsAsFactors = F)

IA_CLAMP<-IA_CLAMP.raw[,1:3]
IA_CLAMP$lagoslakeid<-rep(NA, nrow(IA_CLAMP))

IA_CLAMP$lagoslakeid[IA_CLAMP$LakeName=="Silver Lake"]<- -44
IA_CLAMP$lagoslakeid[IA_CLAMP$LakeName=="Center Lake"]<- -55
IA_CLAMP$lagoslakeid[IA_CLAMP$LakeName=="Big Spirit Lake"]<- -66
IA_CLAMP$lagoslakeid[IA_CLAMP$LakeName=="Little Spirit Lake"]<- -77
IA_CLAMP$lagoslakeid[IA_CLAMP$LakeName=="Upper Gar Lake"]<- -88
IA_CLAMP$lagoslakeid[IA_CLAMP$LakeName=="Minnewashta"]<- -99
IA_CLAMP$lagoslakeid[IA_CLAMP$LakeName=="Lower Gar Lake"]<- -1010
IA_CLAMP$lagoslakeid[IA_CLAMP$LakeName=="West Okoboji"]<- -1111
IA_CLAMP$lagoslakeid[IA_CLAMP$LakeName=="East Okoboji"]<- -1212

IA_CLAMP$Sample_Date<-as.POSIXct(IA_CLAMP$Sample_Date, format="%m/%d/%Y")
IA_CLAMP<-data.frame(lagoslakeid=IA_CLAMP$lagoslakeid,
                     sampledate=IA_CLAMP$Sample_Date,
                     chla=IA_CLAMP$Chlorophyll_ugL)
IA_CLAMP<-IA_CLAMP[month(IA_CLAMP$sampledate)>=5 & month(IA_CLAMP$sampledate)<=9,] 
IA_CLAMP$source<-rep("IA_CLAMP",nrow(IA_CLAMP))

vars<-rbind(vars,IA_CLAMP)

## NTL LTER ---------------------------------------------------------------------------------------
ntl.raw<-read.csv("./LAGOS Extended/ntl38_v5.csv", stringsAsFactors=F)
ntl<-ntl.raw[ntl.raw$depth_range_m=="0-2",]
ntl<-ntl[ntl$year4 >= 2005,]
ntl$sampledate<-as.POSIXct(ntl$sampledate)
ntl<-ntl[month(ntl$sampledate)>=5 & month(ntl$sampledate)<=9,] 
ntl<-ntl[,colnames(ntl) %in% c("lakeid","sampledate","correct_chl_fluor")]
ntl<-aggregate(ntl$correct_chl_fluor,by=list(ntl$lakeid,ntl$sampledate),FUN=mean,na.rm=T)
colnames(ntl)<-c("lakeid","sampledate","correct_chl_fluor")
ntl$lagoslakeid<-rep(NA, nrow(ntl))
ntl$lagoslakeid[ntl$lakeid=="ME"]<-5371
ntl$lagoslakeid[ntl$lakeid=="MO"]<-4559
ntl$lagoslakeid[ntl$lakeid=="WI"]<-827
ntl$lagoslakeid[ntl$lakeid=="FI"]<-2746

ntl<-data.frame(lagoslakeid=ntl$lagoslakeid,
                sampledate=ntl$sampledate,
                chla=ntl$correct_chl_fluor,
                source=rep("NTL_LTER",nrow(ntl)))

vars<-vars[!vars$lagoslakeid %in% c(5371,4559,827,2746),]

vars<-rbind(vars, ntl)

## Sort -------------------------------------------------------------------------------------------  
vars<-vars[order(vars$lagoslakeid,vars$sampledate),]

## Compute sampling frequency by lake and year ----------------------------------------------------
ids<-unique(vars$lagoslakeid)
sampfreq<-NULL

for(ii in 1:length(ids)){
  lakedat<-vars[vars$lagoslakeid==ids[ii],]
  lakeyears=unique(year(lakedat$sampledate))
  
  tmpdiff<-NULL
  tmpn<-NULL
  tmp.minmo<-NULL
  tmp.maxmo<-NULL
  tmp.nmo<-NULL
  for(yy in 1:length(lakeyears)){
    tmpdiff<-c(tmpdiff, mean(abs(diff(lakedat$sampledate[year(lakedat$sampledat)==lakeyears[yy]],units="days"))))
    tmpn<-c(tmpn,length(lakedat$sampledate[year(lakedat$sampledat)==lakeyears[yy]]))
    tmp.minmo<-c(tmp.minmo, min(month(lakedat$sampledate[year(lakedat$sampledat)==lakeyears[yy]])))
    tmp.maxmo<-c(tmp.maxmo, max(month(lakedat$sampledate[year(lakedat$sampledat)==lakeyears[yy]])))
    tmp.nmo<-c(tmp.nmo, length(unique(month(lakedat$sampledate[year(lakedat$sampledat)==lakeyears[yy]]))))
  }
  
  sampfreq<-rbind(sampfreq, 
                  cbind(rep(ids[ii],length(lakeyears)),
                        lakeyears,
                        tmpdiff,
                        tmpn,
                        tmp.minmo,
                        tmp.maxmo,
                        tmp.nmo)
  )
}
colnames(sampfreq)<-c("lagoslakeid","year","mean_interval","nobs","min.mo","max.mo","n.mo")
sampfreq<-as.data.frame(sampfreq)
sampfreq<-sampfreq[complete.cases(sampfreq),]

## Identify good lake years -----------------------------------------------------------------------
min.mos=3 #require data in three months
max.interval=17.5 #require a 17.5-day max interval. This is what you get with 3 months of 14-day sampling with 1 missing.

sampfreq$lake_year<-paste(sampfreq$lagoslakeid,sampfreq$year,sep="_")
vars$lake_year<-paste(vars$lagoslakeid,year(vars$sampledate),sep="_")

sampfreq$good<-sampfreq$mean_interval<=max.interval & sampfreq$n.mo>=min.mos

# ## Select good lake years -------------------------------------------------------------------------
vars.sel<-vars[vars$lake_year %in% sampfreq$lake_year[sampfreq$good],]

length(unique(vars.sel$lagoslakeid)) #1690 lakes have at least one good year

## Select lakes with sufficient coverage ----------------------------------------------------------

ymin=10 #must span 10 years of data
diffmax=4 #maximum gap between years with good data

ids<-unique(vars.sel$lagoslakeid)
get<-NULL
check<-NULL
for(ii in 1:length(ids)){
  tmp<-vars.sel[vars.sel$lagoslakeid==ids[ii],]
  tmp<-tmp[order(tmp$sampledate),]
  ydiff<-diff(year(tmp$sampledate))
  if(max(ydiff)==0){next}
  ydiff<-ydiff[ydiff>0]
  if(max(year(tmp$sampledate))-min(year(tmp$sampledate)) >= ymin &
     max(ydiff) <= diffmax &
     ydiff[1]<=2 &
     ydiff[length(ydiff)]<=2){
    get<-c(get,ids[ii])
  }
  if(max(year(tmp$sampledate))-min(year(tmp$sampledate)) >= ymin+diffmax &
     max(ydiff) > diffmax &
     length(unique(year(tmp$sampledate))) >=6){
     #ydiff[1]<=2 &
     #ydiff[length(ydiff)]<=2){
    check<-c(check,ids[ii])
  }
}

length(get)
length(check)

#lakeinfo_sel<-lakeinfo[lakeinfo$lagoslakeid %in% get,]
#plot(lakeinfo_sel$nhd_long, lakeinfo_sel$nhd_lat)

# Manually check selected lakes and add to dataset as needed --------------------------------------

check.add<-NULL

# ii=92
print(sampfreq[sampfreq$lagoslakeid==85738,])

 check.add<-rbind(check.add, 
                 c(-11,1991,Inf),
                 c(-44,1999,2011),
                 c(-55,2002,Inf),
                 c(249,1996,Inf),
                 c(307,1999,Inf),
                 c(418,1995,Inf),
                 c(1143,1999,Inf),
                 c(1572,1999,Inf),
                 c(1627,1990,Inf),
                 c(2329,1999,Inf),
                 c(2604,1999,Inf),
                 c(2703,2000,Inf),
                 c(3006,2001,Inf),
                 c(3092,2001,Inf),
                 c(3140,2001,Inf),
                 c(3523,1990,2000),
                 c(3863,2006,Inf),
                 c(4099,2001,2011),
                 c(4317,2008,Inf),
                 c(4339,2000,2010),
                 c(4356,2001,Inf),
                 c(4388,1980,1989),
                 c(4432,2000,2012),
                 c(4484,1994,Inf),
                 c(4584,1997,Inf),
                 c(4626,1997,Inf),
                 c(4655,1994,Inf),
                 c(4742,1999,Inf),
                 c(4787,2010,Inf),
                 c(4845,1994,Inf),
                 c(4892,2006,Inf),
                 c(4972,1985,1997),
                 c(4995,2000,Inf),
                 c(5052,2002,Inf),
                 c(5135,2001,Inf),
                 c(5227,2005,Inf),
                 c(5238,1996,Inf),
                 c(5382,1995,Inf),
                 c(5284,2004,2013),
                 c(5435,1994,Inf),
                 c(5491,1997,Inf),
                 c(5542,2007,Inf),
                 c(5564,2008,Inf),
                 c(5567,1996,Inf),
                 c(6065,1992,2002),
                 c(6208,1994,2010),
                 c(6227,1998,Inf),
                 c(6301,1994,2010),
                 c(6397,2004,Inf),
                 c(6404,1998,Inf),
                 c(6405,2004,Inf),
                 c(6409,2008,Inf),
                 c(6412,2000,Inf),
                 c(6413,2006,Inf),
                 c(6419,2004,Inf),
                 c(6436,2004,Inf),
                 c(6480,2002,Inf),
                 c(6492,2003,Inf),
                 c(6493,2008,Inf),
                 c(6494,1997,Inf),
                 c(6519,1988,1998),
                 c(6521,1992,2010),
                 c(6529,1988,2001),
                 c(6534,1986,2005),
                 c(6543,2005,Inf),
                 c(6546,2002,Inf),
                 c(6550,1998,Inf),
                 c(6560,2001,Inf),
                 c(6562,2005,Inf),
                 c(6922,2003,Inf),
                 c(6944,2007,Inf),
                 c(6953,2000,Inf),
                 c(6973,1997,2009),
                 c(7128,1996,Inf),
                 c(7135,2002,Inf),
                 c(7151,2002,Inf),
                 c(7156,2001,Inf),
                 c(7160,1997,2009),
                 c(7168,1998,Inf),
                 c(7171,1988,2001),
                 c(7382,2006,Inf),
                 c(7402,2002,Inf),
                 c(7461,1999,Inf),
                 c(7521,2002,Inf),
                 c(7540,1994,Inf),
                 c(7886,1997,Inf),
                 c(7905,2002,Inf),
                 c(8037,1999,Inf),
                 c(8075,1999,Inf),
                 c(8179,1999,2010),
                 c(8213,2002,Inf),
                 c(8313,2000,Inf),
                 c(8325,1999,Inf),
                 c(8469,1999,Inf),
                 c(72641,1991,Inf)
                 )

check.add<-as.data.frame(check.add)
colnames(check.add)<-c("lagoslakeid","ystart","yend")

## Select out the good lake years------------------------------------------------------------------

sampfreq.get<-sampfreq[sampfreq$lagoslakeid %in% get,]

for(ii in 1:nrow(check.add)){
  
  tmp<-sampfreq[sampfreq$lagoslakeid==check.add$lagoslakeid[ii],]
  tmp<-tmp[tmp$year >= check.add$ystart[ii] & tmp$year <= check.add$yend[ii],]
  sampfreq.get<-rbind(sampfreq.get,tmp)
}

sampfreq.get<-sampfreq.get[sampfreq.get$good,]
length(unique(sampfreq.get$lagoslakeid))

write.csv(sampfreq.get,"./Analyses/sampfreq_goodlakes_20200203.csv", row.names=F)

trend.dat<-vars[vars$lake_year %in% sampfreq.get$lake_year,]

## Add region variable ----------------------------------------------------------------------------
region.shp<-readOGR("./Analyses/region_class.shp",stringsAsFactors = F)
region.shp@data<-region.shp@data[,colnames(region.shp@data) %in% c("id","class")]

#plot(region.shp, col=region.shp@data$color)
lakeinfo_out<-lakeinfo[lakeinfo$lagoslakeid %in% trend.dat$lagoslakeid,]

#Add info about iowa lakes
ia_lakeinfo.raw<-read.csv("./LAGOS Extended/Iowa/Iowa_Covariates.csv", stringsAsFactors=F)
ia_lakeinfo<-data.frame(lagoslakeid=rep(NA, nrow(ia_lakeinfo.raw)),
                        gnis_name=ia_lakeinfo.raw$LakeName,
                        nhd_lat=ia_lakeinfo.raw$nhd_lat,
                        nhd_long=ia_lakeinfo.raw$nhd_long,
                        lake_area_ha=ia_lakeinfo.raw$lake_area_ha,
                        lake_perim_meters=rep(NA, nrow(ia_lakeinfo.raw)))
ia_lakeinfo$lagoslakeid<-c(-44, -55, -1111, -1212, -1010, -88, -99, -66, -77, -33, -22, -11)

lakeinfo_out<-rbind(lakeinfo_out, ia_lakeinfo[ia_lakeinfo$lagoslakeid %in% sampfreq.get$lagoslakeid,])

lakepts<-SpatialPointsDataFrame(coords=cbind(lakeinfo_out$nhd_long,lakeinfo_out$nhd_lat),
                                             data=lakeinfo_out, 
                                             proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
lakepts<-spTransform(lakepts,crs(region.shp))
lakepts_intersect<-raster::intersect(lakepts,region.shp)

lakepts$lagoslakeid[!lakepts$lagoslakeid %in% lakepts_intersect$lagoslakeid] #this is a mountain lake

lakeregions<-lakepts_intersect@data
lakeregions<-lakeregions[,colnames(lakeregions) %in% c("lagoslakeid","class")]
lakeregions<-rbind(lakeregions, c("6302","mountains"))

table(lakeregions$class) # there are only two "plains" lakes here

lakeinfo_out$lagoslakeid<-as.character(lakeinfo_out$lagoslakeid)

lakeinfo_out<-left_join(lakeinfo_out, lakeregions)

write.csv(trend.dat, "./Analyses/hab_trend_data_filtered_20200308.csv", row.names=F)
write.csv(lakeinfo_out, "./Analyses/hab_trend_lakeinfo_20200308.csv", row.names=F)


