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
  SSR<-c()
  SSRx <- c()
  beta0 <- c()
  beta <- c()
  tscore <- c()
  tdate<-c()  
  #assum trend does not change within first window
  x0<-data[1:initialWindowSize,1]
  y0<-data[1:initialWindowSize,2]
  c0<-lm(y0~x0)
  
  regimeChangePoint = NULL
  
  while(len>initialWindowSize){
    
    x1<-data[(initialWindowSize+1):min(initialWindowSize+incrementalWindowSize, len),1]
    y1<-data[(initialWindowSize+1):min(initialWindowSize+incrementalWindowSize, len),2]
    
    c<-lm(y1~x1)
    
    cat(paste("@ Point ", initialWindowSize, "\n"))
    cat(paste("slope up to now: ", c0$coefficients[2], "\n"))
    cat(paste("new slope: ", c$coefficients[2], "\n"))
    
    x1<-as.numeric(x1)
    n=length(x1)
    SSR<-sum(c$residuals^2)
    SSRx <- sum((x1-mean(x1))^2)
    beta0 <- c0$coefficients[2]
    beta <-c$coefficients[2]
    tscore_beta <- (beta-beta0)*sqrt(n-2)/sqrt(SSR/SSRx)
    tscorec<-pt(tscore_beta, df=n-2)
    tdate<-rbind(tdate,data[initialWindowSize, 1])
    tscore<-rbind(tscore, tscorec)
    
    if(tscorec<pvalue_thr){
      regimeChangePoint <- initialWindowSize
      break
    }
    else{
      print(paste("intial:", initialWindowSize," - end: ",incrementalWindowSize+initialWindowSize))
      initialWindowSize <-initialWindowSize + 1
      
      x0<-data[1:initialWindowSize,1]
      y0<-data[1:initialWindowSize,2]
      c0<-lm(y0~x0)
    }
  }
  if(plotScore==T){plot((tscore), type='b')}
  p$tscore<-tscore
  p$rcp<-regimeChangePoint
  p$tdate<-tdate
    return(p)
}

dev.new()

incrementalWindowSize<-5
p<-LocateRegimeChange(data=QuarterAgg, initialWindowSize=10, incrementalWindowSize=incrementalWindowSize, T, 0.01)

Newlen<-length(QuarterAgg$Group.1[((1:NROW(QuarterAgg$Group.1))%%incrementalWindowSize)==0])
ax<-QuarterAgg$Group.1[((1:NROW(QuarterAgg$Group.1))%%incrementalWindowSize)==0]


plot(p$tscore~as.Date(p$tdate),type='b')
pvector<-c(rep(NA, p/incrementalWindowSize-1) ,QuarterAgg$x[p], rep(NA, Newlen-(p/incrementalWindowSize)))
points(QuarterAgg$Group.1[((1:NROW(QuarterAgg$Group.1))%%incrementalWindowSize)==0], pvector,col="red", pch="*", cex=3)

print(QuarterAgg$Group.1[p$rcp])

