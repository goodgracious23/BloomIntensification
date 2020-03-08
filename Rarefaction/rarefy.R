## Rarefaction test for high frequency data

## finds the error in the mean, maximum, and timing of peak

## ts is a time series of values
## tt are days of the year corresponding to entries in ts

rarefy<-function(tt,ts,downsample=2:30,thresh=11){
  
  out<-NULL#this will contain n+1 dataframes, where n is the length of downsample
  
  out<-rbind(out, data.frame(dat.mean=mean(ts, na.rm=T),
                             dat.95pct=quantile(ts,0.95,na.rm=T),
                             dat.grtr=sum(ts>thresh, na.rm=T)/length(ts)
                  )
  )
  
  for(ii in 1:length(downsample)){
    
    dat.mean<-NULL
    dat.95pct<-NULL
    dat.grtr<-NULL
    
    for(jj in 1:downsample[ii]){
      
      tmp.ts<-ts[seq(from=jj,to=length(ts),by=downsample[ii])]
      
      dat.mean<-c(dat.mean, mean(tmp.ts, na.rm=T))
      dat.95pct<-c(dat.95pct, quantile(tmp.ts,0.95,na.rm=T))
      dat.grtr<-c(dat.grtr, sum(tmp.ts>thresh, na.rm=T)/length(tmp.ts))
      
    }
    
    out<-rbind(out, data.frame(dat.mean=dat.mean,
                               dat.95pct=dat.95pct,
                               dat.grtr=dat.grtr
                               )
               )
  }
  
  out$interval<-c(1,rep(downsample,downsample))
  
  return(out)
}