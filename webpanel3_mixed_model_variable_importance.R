## WebPanel 3: Variable importances -------------------------------------------------------
## Written by Jon Walter

## NOTE: Run 'step4_mixed_model.R' first to create this figure

colnames(imp.lm.avg)[2]<-"importance.avg"
colnames(imp.lm.p95)[2]<-"importance.p95"
colnames(imp.lm.imp)[2]<-"importance.imp"

imp.all<-left_join(imp.lm.avg,imp.lm.p95)
imp.all<-imp.all[order(rowMeans(imp.all[,2:3])),]

labels_cln<-c("Mean chlorophyll-a"
              ,"Lake area"
              ,"Lake maximum depth"
              ,"Precipitation trend"
              ,"Drainage ratio"
              ,"TP trend"
              ,"TN trend"
              ,"Temperature trend"
              ,"mean N:P"
              ,"Mean chl.-a x Precip. trend"
              ,"Mean chl.-a x Drainage ratio"
              ,"Mean chl.-a x Lake area"
              ,"Mean chl.-a x Max depth"
              ,"Mean chl.-a x mean N:P"
              ,"Mean chl.-a x TN trend"
              ,"Mean chl.-a x Temp. trend"
              ,"Mean chl.-a x TP trend")

intensecol <- rgb(82,139,139, max = 255, alpha = 250)
#intensecol1 <- rgb(82,139,139, max = 255, alpha = 250)
severitycol <- rgb(16,78,139, max = 255, alpha = 220)
#severitycol1 <- rgb(16,78,139, max = 255, alpha = 220)


tiff("variable_importances.tif", units="in",res=150,width=5,height=5)
par(mfrow=c(1,1), mar=c(5.1,12.1,1.1,1.1))
barplot(rbind(imp.all$importance.avg,imp.all$importance.p95), names.arg=rev(labels_cln), horiz=T,las=2, xlim=c(0,1),
        xlab="Variable importance", beside=T, legend.text = c("magnitude","severity"), 
        args.legend=list(x="bottomright", inset=0.1), col=c(intensecol,severitycol))
dev.off()

