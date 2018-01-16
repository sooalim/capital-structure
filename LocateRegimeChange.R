LocateRegimeChange<-function(data, initialWindowSize, incrementalWindowSize, plotScore=F, pvalue_thr=0.01){
  
  ## input: 
  ## a: data has in the form of [X, Y] X, Y are vectors
  ## initialWindowSize: size of the first window to fit the model
  ## incrementalWindowSize: the size of sliding window (test window)
  ## plotScore: if TRUE it will plot the t-score value
  ## pvalue_thr: p-value threshold
  ##
  ## output: the index number at which the linear model changes
  
  len<-length(data[,1])
  tscore<-c()
  tscore_date<-c()
  
  #assum trend does not change within first window
  x0<-data[1:initialWindowSize,1]
  y0<-data[1:initialWindowSize,2]
  c0<-lm(y0~x0)
  
  regimeChangePoint = NULL
  
  while(len>initialWindowSize+incrementalWindowSize){

    x1<-data[(initialWindowSize+1):min(initialWindowSize+incrementalWindowSize, len),1]
    y1<-data[(initialWindowSize+1):min(initialWindowSize+incrementalWindowSize, len),2]
    tscore_date<-rbind(tscore_date,initialWindowSize)
    c<-lm(y1~x1)
    
    cat(paste("@ Point ", data[initialWindowSize,1], "\n"))
    cat(paste("slope up to now: ", c0$coefficients[2], "\n"))
    cat(paste("new slope: ", c$coefficients[2], "\n"))
    cat(paste("regime point", (regimeChangePoint), "\n"))
    
    x1<-as.numeric(x1)
    n=length(x1)
    SSR<-sum(c$residuals^2)
    SSRx <- sum((x1-mean(x1))^2)
    beta0 <- c0$coefficients[2]
    beta <-c$coefficients[2]
    tscore_beta <- (beta-beta0)*sqrt(n-2)/sqrt(SSR/SSRx)
    tscorec<-pt(tscore_beta, df=n-2)
    
    cat(paste("t-score: ", tscorec, "\n"))
    tscore<-rbind(tscore, tscorec)

    if(ifelse(is.na(tscorec),FALSE, tscorec<pvalue_thr)){
      regimeChangePoint <-initialWindowSize
      
      print("\n------------------------------------------------------------")
      print(regimeChangePoint)
      initialWindowSize <-initialWindowSize + incrementalWindowSize
      x0<-data[1:initialWindowSize,1]
      y0<-data[1:initialWindowSize,2]
      c0<-lm(y0~x0)
      print("\n-------------------------------------------------------Break")
      #break
    }
    else{
      print(paste("intial:", data[initialWindowSize,1]," - end: ",data[incrementalWindowSize+initialWindowSize,1]))
      initialWindowSize <-initialWindowSize + incrementalWindowSize
      
      x0<-data[1:initialWindowSize,1]
      y0<-data[1:initialWindowSize,2]
      c0<-lm(y0~x0)
      
    }
  }
    
  if(plotScore==T){
    #par(mfrow=c(2,1))
    
    vec<-ifelse(tscore_date==regimeChangePoint,tscore[tscore_date==regimeChangePoint], NA)
    xx<-c(as.Date(data[tscore_date,1]), rev(as.Date(data[tscore_date,1])))
    yy<-c(rep(1e-9,length(tscore_date)), rev(rep(pvalue_thr, length(tscore_date))))
    
    plot(tscore~as.Date(data[tscore_date,1]), type='b',cex=0.5,log='y',las=2,xlab="Date", ylab="T-Score of new slope = slope up to now")
    polygon(xx, yy, col=rgb(1, 0, 0,0.3), border=NA)
    points(vec~as.Date(data[tscore_date,1]), cex=2, pch="*",col='red')
    #lines(rep(pvalue_thr, length(tscore_date))~as.Date(data[tscore_date,1]), col="red", lty=2)
    
    
    vec<-ifelse(tscore_date==regimeChangePoint,data[regimeChangePoint,2], NA)
    plot(as.Date(data[p$tscore_date,1]), 
         data[p$tscore_date,2],
          type='b', las=2, cex=0.5, axes=T,
         xlab="Date", ylab="Aggregate Debt/Capital Ratio",
         ylim=c(0,1))
    lines(as.Date(data[tscore_date,1]), 
          data[p$tscore_date,2],
          axes=T, type='l', lty=2,
          xlab="Date", ylab="Aggregate Debt/Capital Ratio",
          ylim=c(0,1))
    points(as.Date(data[tscore_date,1]),vec, col="red", pch="*", cex=3)
    print(data[regimeChangePoint,1])
    print("\n------------------------------------------------------------")
    print(regimeChangePoint)
    
  
      }
  
  info<-c()
  info$regimeChangePoint<-regimeChangePoint
  info$tscore<-tscore
  info$tscore_date<-tscore_date
  return(info)
   }
 
incrementalWindowSize<-3
p<-LocateRegimeChange(data=QuarterAgg, initialWindowSize=5, incrementalWindowSize=incrementalWindowSize, T, 0.05)
