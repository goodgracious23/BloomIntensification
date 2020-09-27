## Figure 3 - Bloom Trends Mixed Model output
## Written by Jon Walter
## Last updated by Grace Wilkinson on 25 Sept 2020

## NOTE: Run 'step4_mixed_model.R' first to create this figure

if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)

#Mix that color palette up and get ready for arts and crafts times
pttrans=0.4
ptpal=brewer.pal(8,"Set2")[c(1,3,4)]
ptrgb<-col2rgb(ptpal)
ptpal<-rgb(red=ptrgb[1,],green=ptrgb[2,],blue=ptrgb[3,],alpha=pttrans*255, maxColorValue = 255)
lnpal=brewer.pal(8,"Set2")[c(1,3,4)]
yrange = c(-0.35,0.35)


tiff("Fig3_MixedModelResults.tiff",
     units="in",
     res=300,
     width=4.5,
     height=4.5)

# windows(height=4.5, width=4.5)
par(mfrow=c(3,2), 
    mar=c(3,3.3,1,1), 
    mgp=c(1.75,0.75,0))

plot(NA, NA, xlim=range(dat.avg.chla$chla_mean), ylim=yrange, 
     xlab="Mean chlorophyll-a", ylab="Trend in magnitude", cex.lab=1.1, cex.axis=0.9)
rect(xleft=-1,ybottom=-1,xright=60,ytop=0,col="grey95",border=NA)
points(pr.avg$chla_mean[pr.avg$LII_ecoregion==5.3], pr.avg$b1.chla[pr.avg$LII_ecoregion==5.3],pch=16, col=ptpal[3], cex=1.25)
points(pr.avg$chla_mean[pr.avg$LII_ecoregion==8.1], pr.avg$b1.chla[pr.avg$LII_ecoregion==8.1],pch=16, col=ptpal[2], cex=1.25)
points(pr.avg$chla_mean[pr.avg$LII_ecoregion==8.4], pr.avg$b1.chla[pr.avg$LII_ecoregion==8.4],pch=16, col=ptpal[1], cex=1.25)
abline(a=coef(lm.avg.best)$LII_ecoregion[1,1],b=coef(lm.avg.best)$LII_ecoregion[1,2], col=lnpal[3], lwd=2)
abline(a=coef(lm.avg.best)$LII_ecoregion[2,1],b=coef(lm.avg.best)$LII_ecoregion[2,2], col=lnpal[2], lwd=2)
abline(a=coef(lm.avg.best)$LII_ecoregion[3,1],b=coef(lm.avg.best)$LII_ecoregion[3,2], col=lnpal[1], lwd=2)
text(par("usr")[1]+0.07*abs(diff(par("usr")[1:2])), par("usr")[4]-0.1*abs(diff(par("usr")[3:4])),"a)",cex=1)
legend("topright",lwd=3,legend=c("Region 5.3","Region 8.1","Region 8.4"),
       col=lnpal[1:3], cex=0.8)
box()

plot(NA, NA, xlim=range(dat.avg.chla$lake_area_ha), ylim=yrange, 
     xlab=expression('log'[10]*'(Lake area) (ha)'), ylab="Trend in magnitude", cex.lab=1.1, cex.axis=0.9)
rect(xleft=-1,ybottom=-1,xright=60,ytop=0,col="grey95",border=NA)
points(pr.avg$lake_area_ha[pr.avg$LII_ecoregion==5.3], pr.avg$b1.lake_area[pr.avg$LII_ecoregion==5.3],pch=16, col=ptpal[3], cex=1.25)
points(pr.avg$lake_area_ha[pr.avg$LII_ecoregion==8.1], pr.avg$b1.lake_area[pr.avg$LII_ecoregion==8.1],pch=16, col=ptpal[2], cex=1.25)
points(pr.avg$lake_area_ha[pr.avg$LII_ecoregion==8.4], pr.avg$b1.lake_area[pr.avg$LII_ecoregion==8.4],pch=16, col=ptpal[1], cex=1.25)
abline(a=coef(lm.avg.best)$LII_ecoregion[1,1],b=coef(lm.avg.best)$LII_ecoregion[1,3], col=lnpal[3], lwd=2)
abline(a=coef(lm.avg.best)$LII_ecoregion[2,1],b=coef(lm.avg.best)$LII_ecoregion[2,3], col=lnpal[2], lwd=2)
abline(a=coef(lm.avg.best)$LII_ecoregion[3,1],b=coef(lm.avg.best)$LII_ecoregion[3,3], col=lnpal[1], lwd=2)
text(par("usr")[1]+0.07*abs(diff(par("usr")[1:2])), par("usr")[4]-0.1*abs(diff(par("usr")[3:4])),"b)",cex=1)

box()
#axis(1,at=seq(from=1/8,by=1/4,length.out=4),labels=10^(1:4))

plot(NA, NA, xlim=range(dat.avg.chla$trend.pptp95), ylim=yrange, 
     xlab="Precipitation trend", ylab="Trend in magnitude", cex.lab=1.1, cex.axis=0.9)
rect(xleft=-1,ybottom=-1,xright=60,ytop=0,col="grey95",border=NA)
points(pr.avg$trend.pptp95[pr.avg$LII_ecoregion==5.3], pr.avg$b1.ppttrend[pr.avg$LII_ecoregion==5.3],pch=16, col=ptpal[3], cex=1.25)
points(pr.avg$trend.pptp95[pr.avg$LII_ecoregion==8.1], pr.avg$b1.ppttrend[pr.avg$LII_ecoregion==8.1],pch=16, col=ptpal[2], cex=1.25)
points(pr.avg$trend.pptp95[pr.avg$LII_ecoregion==8.4], pr.avg$b1.ppttrend[pr.avg$LII_ecoregion==8.4],pch=16, col=ptpal[1], cex=1.25)
abline(a=coef(lm.avg.best)$LII_ecoregion[1,1],b=coef(lm.avg.best)$LII_ecoregion[1,4], col=lnpal[3], lwd=2)
abline(a=coef(lm.avg.best)$LII_ecoregion[2,1],b=coef(lm.avg.best)$LII_ecoregion[2,4], col=lnpal[2], lwd=2)
abline(a=coef(lm.avg.best)$LII_ecoregion[3,1],b=coef(lm.avg.best)$LII_ecoregion[3,4], col=lnpal[1], lwd=2)
text(par("usr")[1]+0.07*abs(diff(par("usr")[1:2])), par("usr")[4]-0.1*abs(diff(par("usr")[3:4])),"c)",cex=1)
box()

plot(NA, NA, xlim=range(dat.avg.chla$chla_mean), ylim=yrange, 
     xlab="Mean chlorophyll-a", ylab="Trend in magnitude", cex.lab=1.1, cex.axis=0.9)
rect(xleft=-1,ybottom=-1,xright=60,ytop=0,col="grey95",border=NA)
abline(a=-0.044235,b=-0.0069,lty=1, lwd=2)
abline(a=-0.04423, b=-0.0027,lty=2, lwd=2)
abline(a=-0.044235,b=0.0015,lty=3, lwd=2)
legend("topright", legend=c("precipitation declining","precipitation steady","precipitation increasing"),lty=1:3,bty="n", cex=0.8)
text(par("usr")[1]+0.07*abs(diff(par("usr")[1:2])), par("usr")[4]-0.1*abs(diff(par("usr")[3:4])),"d)",cex=1)
box()

plot(NA, NA, xlim=range(dat.p95.chla$chla_mean), ylim=yrange, 
     xlab="Mean chlorophyll-a", ylab="Trend in severity", cex.lab=1.1, cex.axis=0.9)
rect(xleft=-1,ybottom=-1,xright=60,ytop=0,col="grey95",border=NA)
points(pr.p95$chla_mean[pr.p95$LII_ecoregion==5.3], pr.p95$b1.chla[pr.p95$LII_ecoregion==5.3],pch=16, col=ptpal[3], cex=1.25)
points(pr.p95$chla_mean[pr.p95$LII_ecoregion==8.1], pr.p95$b1.chla[pr.p95$LII_ecoregion==8.1],pch=16, col=ptpal[2], cex=1.25)
points(pr.p95$chla_mean[pr.p95$LII_ecoregion==8.4], pr.p95$b1.chla[pr.p95$LII_ecoregion==8.4],pch=16, col=ptpal[1], cex=1.25)
abline(a=coef(lm.p95.best)$LII_ecoregion[1,1],b=coef(lm.p95.best)$LII_ecoregion[1,2], col=lnpal[3], lwd=2)
abline(a=coef(lm.p95.best)$LII_ecoregion[2,1],b=coef(lm.p95.best)$LII_ecoregion[2,2], col=lnpal[2], lwd=2)
abline(a=coef(lm.p95.best)$LII_ecoregion[3,1],b=coef(lm.p95.best)$LII_ecoregion[3,2], col=lnpal[1], lwd=2)
text(par("usr")[1]+0.07*abs(diff(par("usr")[1:2])), par("usr")[4]-0.1*abs(diff(par("usr")[3:4])),"e)",cex=1)
box()

plot(NA, NA, xlim=range(dat.p95.chla$lake_area_ha), ylim=yrange, 
     xlab=expression('log'[10]*'(Lake area) (ha)'), ylab="Trend in severity", cex.lab=1.1, cex.axis=0.9)
rect(xleft=-1,ybottom=-1,xright=60,ytop=0,col="grey95",border=NA)
points(pr.p95$lake_area_ha[pr.p95$LII_ecoregion==5.3], pr.p95$b1.lake_area[pr.p95$LII_ecoregion==5.3],pch=16, col=ptpal[3], cex=1.25)
points(pr.p95$lake_area_ha[pr.p95$LII_ecoregion==8.1], pr.p95$b1.lake_area[pr.p95$LII_ecoregion==8.1],pch=16, col=ptpal[2], cex=1.25)
points(pr.p95$lake_area_ha[pr.p95$LII_ecoregion==8.4], pr.p95$b1.lake_area[pr.p95$LII_ecoregion==8.4],pch=16, col=ptpal[1], cex=1.25)
abline(a=coef(lm.p95.best)$LII_ecoregion[1,1],b=coef(lm.p95.best)$LII_ecoregion[1,3], col=lnpal[3], lwd=2)
abline(a=coef(lm.p95.best)$LII_ecoregion[2,1],b=coef(lm.p95.best)$LII_ecoregion[2,3], col=lnpal[2], lwd=2)
abline(a=coef(lm.p95.best)$LII_ecoregion[3,1],b=coef(lm.p95.best)$LII_ecoregion[3,3], col=lnpal[1], lwd=2)
text(par("usr")[1]+0.07*abs(diff(par("usr")[1:2])), par("usr")[4]-0.1*abs(diff(par("usr")[3:4])),"f)",cex=1)
box()

dev.off()

