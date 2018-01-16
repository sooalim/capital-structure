#------------------------------------------------------------------#
# Debt to Capital Portfolio                                        #
#------------------------------------------------------------------#

portfolio_compute<-function(data=Annual_Ratio,item="DCMarket", # Any item that exist in the data table
                            criteria="DCMarket",               # Any item that exist in the data table
                            start=1979, 
                            end=2016, eventtime=21, 
                            survivor = c("s","ns"),            #s: Survivors ns: non-survivors
                            scope = c("all", "pure", "active"),#pure: only US Regional banks that are active and inactive. 
                            #all: Reginoal banks + Bulge Brackets that are active and inactive 
                            #active: all banks that are still in business
                            plot.all=c(TRUE, FALSE),           #Plot average leverage graphs of each portfolios formulated every year 
                            main="Debt to Market Capital Portfolio (Pure US Regional Active Banks)", 
                            ylab="Market Leverage", loc="topright",
                            reverse=FALSE)
{
  Annual_Ratio<-data
  #portfolio.hold - Held for 1
  par(mfrow=c(2,2))
  
  #length of event period
  eventtime=eventtime
  start = start
  if(survivor=="s"){end=2016-eventtime+1}
  else {end = end}
  
  if (reverse==TRUE){start1=start
  start=end
  end=start1}
  
  for (i in seq(start,end)){
    
    #start year
    sy=i 
    #end year
    if(reverse==FALSE) {ey=sy+eventtime} 
    else {ey=sy-eventtime}
    #Criteria for dividing quartiles
    criteria = criteria
    #item of interest
    item = item
    
    if(scope=="all"){
      #for all companies that existed at year sy, allows for active/inactive banks/bigbanks in the set
      sy.exists<-subset(Annual_Ratio, Annual_Ratio$year==sy,"Ticker")
    }
    else if (scope=="pure"){
      #for all companies that existed at year sy, Pure US Regionals option. 
      sy.exists<-subset(Annual_Ratio, Annual_Ratio$year==sy& !(Annual_Ratio$Ticker %in% BigBanks),"Ticker")
    }
    else if (scope=="active"){
      #for all companies that existed at year sy, Pure US Regionals, Active Banks option. 
      sy.exists<-subset(Annual_Ratio, Annual_Ratio$year==sy& Annual_Ratio$Ticker %in% setdiff(Active, BigBanks),"Ticker")
    }
    
    a <- count(subset(Annual_Ratio, 
                      Annual_Ratio$Ticker %in% sy.exists$Ticker&
                        Annual_Ratio$year>=sy&Annual_Ratio$year<ey&
                        !is.na(Annual_Ratio[,item]),
                      "Ticker")) 
    
    if(survivor=="s"){
      
      # Requires the firm to have survived over the whole event time period
      a<- a[a[,2]>=eventtime,1] #(Survivors options)
    } else if(survivor =="ns"){
      
      #use this option to include portfolios with less than event time period in the average calculations.
      a<-a[,1] #(All inclusive options)
    }
    
    x<-subset(Annual_Ratio[(Annual_Ratio$Ticker %in% a)&(Annual_Ratio$year>=sy)&(Annual_Ratio$year<ey),])
    y<-data.frame(cbind(Ticker=as.character(x[x$year==sy,c("Ticker")]),quantile= cut(x[x$year==sy,criteria], breaks = quantile(x[x$year==sy,criteria], probs = seq(0, 1, 0.25), na.rm=TRUE, names=TRUE), include.lowest = TRUE, labels = 1:4)))
    
    x.sub<-x[,c("Ticker", "year", item)]
    quantile.hold<-left_join(x.sub, y)
    quantile.hold$eventyear<-quantile.hold$year-sy
    
    mean_<-function(x){mean(x, na.rm=TRUE)}
    median_<-function(x){median(x, na.rm=TRUE)}
    pf.hold<-aggregate(quantile.hold[,item],by=quantile.hold[,c("eventyear", "quantile")], FUN="mean_")
    
    if(plot.all){
      plot(x~eventyear, data=pf.hold[pf.hold$quantile==1,], type="l", ylim=c(min(pf.hold$x, na.rm=TRUE), max(pf.hold$x, na.rm=TRUE)), xlim=c(0, eventtime-1), lty=2, main=paste("Portfolio formed at year:", i))
      abline(v=eventtime-1, col="red", lty=2)
      lines(x~eventyear, data=pf.hold[pf.hold$quantile==2,], type="l")
      lines(x~eventyear, data=pf.hold[pf.hold$quantile==3,],col="red", type="l")
      lines(x~eventyear, data=pf.hold[pf.hold$quantile==4,], col="blue",type="l")
      points(x~eventyear, data=pf.hold[pf.hold$quantile==1,],pch=21, bg="grey", cex=0.8)
      points(x~eventyear, data=pf.hold[pf.hold$quantile==2,],pch=21, bg="grey", cex=0.8)
      points(x~eventyear, data=pf.hold[pf.hold$quantile==3,], col="red", pch=21, bg="salmon", cex=0.8)
      points(x~eventyear, data=pf.hold[pf.hold$quantile==4,], col="blue",pch=21, bg="lightblue", cex=0.8)
    }
    
    #change names to corresponding year
    colnames(pf.hold)<-gsub("x",i, colnames(pf.hold))
    
    if (sy==start){event.portfolio<-pf.hold[,c("eventyear", "quantile", i)]}
    else {event.portfolio<-left_join(event.portfolio,pf.hold[,c("eventyear", "quantile", i)], by=c("quantile", "eventyear"))}
    
    # reports the number of companies that were included in the interquantile calculation for each year portfolio 
    
    if (sy==start){freq<-count(quantile.hold, vars=c("quantile","eventyear")) }
    else {
      freq_i<-count(quantile.hold, vars=c("quantile","eventyear"))
      colnames(freq_i)<-gsub("freq", i, colnames(freq_i))
      freq<-left_join(freq, freq_i)}
  }
  
  long.event.portfolio<-melt(event.portfolio, id.vars=c("quantile", "eventyear"))
  avg.leverage<-aggregate(long.event.portfolio$value,
                          by=long.event.portfolio[,c("eventyear", "quantile")],
                          FUN="mean_")
  
  confint_<-function(x){mean(x, na.rm=TRUE)+sd(x, na.rm=TRUE)/sqrt(NROW(x))*qt(c(0.05, 0.95), df=NROW(x))}
  var.leverage<-aggregate(long.event.portfolio$value,
                          by=long.event.portfolio[,c("eventyear", "quantile")],
                          FUN="confint_")
  par(mfrow=c(1,1))
  
  #Average leverage - Lines
  plot(x~eventyear, data=avg.leverage[avg.leverage$quantile==1,], type="l",lty=1,col = "#eb8c00",
       ylim=c(min(avg.leverage$x),max(avg.leverage$x)), 
       main=main, 
       xlab="Event Time (years)",ylab=ylab)
  lines(x~eventyear, data=avg.leverage[avg.leverage$quantile==2,], col="#e0301e")
  lines(x~eventyear, data=avg.leverage[avg.leverage$quantile==3,], col="#a32020")
  lines(x~eventyear, data=avg.leverage[avg.leverage$quantile==4,], col="#602320")
  
  #Average leverage - Points
  points(x~eventyear, data=avg.leverage[avg.leverage$quantile==1,], col="#eb8c00",pch=19)
  points(x~eventyear, data=avg.leverage[avg.leverage$quantile==2,], col="#e0301e",pch=19)
  points(x~eventyear, data=avg.leverage[avg.leverage$quantile==3,], col="#a32020", pch=19)
  points(x~eventyear, data=avg.leverage[avg.leverage$quantile==4,], col="#602320", pch=19)
  
  #confint 5%
  for (i in 1:4){
    lines((x[,1])~eventyear, data=var.leverage[var.leverage$quantile==i,], col="lightgrey", lty=2)
    lines((x[,2])~eventyear, data=var.leverage[var.leverage$quantile==i,], col="lightgrey", lty=2)
  }
  legend(loc, c("Low", "Medium", "High", "Very High", "95% conf.int."), col=c("#eb8c00", "#e0301e","#a32020","#602320", "lightgrey"), pch=c(rep(19, 4),NA) , lty=c(1,1,1,1,2), bty="n" )
  
  return(avg.leverage) 
}

