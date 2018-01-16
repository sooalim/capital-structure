#Detecting Shifts in mean/median 
library(ggplot2)
t<-ddply(Quarter, "Date", summarise, quantile(DCBook_Q, probs=c(0.25, 0.40, 0.5, 0.6, 0.75), na.rm=TRUE))
t$label<-c(1:765)%%5
t<-reshape(t, direction = "wide", idvar="Date", v.names="..1", timevar="label")
colnames(t)<-c("Date","25%", "40%", "50%", "60%", "75%")
for (i in colnames(t)[2:6]){t[, paste("MA",i, sep="")]<-SMA(t[,i], n=4)}

#plot +/- 10% range
plot(cbind(SMA(t$`40%`, n=4), t$`50%`)[,1], type="l",col="red", ylim=c(0.35, 0.75))
lines(cbind(SMA(t$`40%`, n=4), t$`50%`)[,2], type="l")
lines(cbind(SMA(t$`40%`, n=4), t$`50%`, t$`60%`)[,3], type="l", col="red")


#plot +/- 10 range and IQR
plot(t$Date, t$`25%`, type="l", ylim=c(min(t[,2:6]),max(t[,2:6])))
for( i in 2:6){lines(t$Date, t[,i], type="l",col=i)}


# Graph - Moving Average +/-10% of Median vs. Median value of Debt to Capital at point of time
t$flag<-ifelse(((t$`50%`>t$`MA60%`)|(t$`50%`<t$`MA40%`))&!is.na(t$`50%`), t$`50%`,NA)
t$flagDate<-ifelse(((t$`50%`>t$`MA60%`)|(t$`50%`<t$`MA40%`))&!is.na(t$`50%`), format(t$Date, "%B '%y"),NA)
t$period<-cumsum(ifelse(((t$`50%`>t$`MA60%`)|(t$`50%`<t$`MA40%`))&!is.na(t$`MA50%`),1,0))

p1<-ggplot(t,aes(x=Date, y=`50%`))+
  geom_line()+
  geom_ribbon(aes(x=Date, ymin=`MA40%`,ymax=`MA60%`),alpha=0.40,fill="indianred3")+
  geom_ribbon(aes(x=Date, ymin=`MA25%`,ymax=`MA75%`),alpha=0.20,fill="indianred1")+
  ylab("Debt to Capital")+
  xlab("Year")+
  ggtitle("Debt to Book Capital Ratio")+
  scale_x_date(date_breaks = "5 year", date_labels="%Y")

p1<-p1+geom_point(data=t, aes(x=Date, y=flag), color="red", shape=19, size=3)+
  geom_label(data=t, aes(label=flagDate), vjust=2)

plot(p1)


t<-left_join(t, Index)

