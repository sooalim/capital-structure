#aggregating number of records by each ticker and finding legnth of time series data
library(plyr)
#t<-ddply(Quarter, "Ticker", summarize, Duration = (max(Date.EOQ)-min(Date.EOQ))/365, nratings = length(unique(Rating_ST_T)),Status = mean(Status)) 
#t<-ddply(Quarter, "Date", summarise, unique(Ticker) )

t<-ddply(Quarter, "Date", summarise, quantile(DCBook_Q, probs=c(0.25, 0.4, 0.5, 0.6, 0.75), na.rm=TRUE))
t$label<-c(1:765)%%5
t<-reshape(t, direction = "wide", idvar="Date", v.names="..1", timevar="label")
colnames(t)<-c("Date","25%", "40%", "50%", "60%", "75%")
plot(t$Date, t$`25%`, type="l", ylim=c(min(t[,2:6]),max(t[,2:6])))
for( i in 2:6){lines(t$Date, t[,i], type="l",col=i)}

t<-ddply(Quarter, "Date", summarise, mean(DCBook_Q, na.rm=TRUE)); colnames(t)<-c("Date", "DCBook_Q_mean")
tmed<-ddply(Quarter, "Date", summarise, median(DCBook_Q, na.rm=TRUE)); colnames(tmed)<-c("Date", "DCBook_Q_med")

tlog<-ddply(Quarter, "Date", summarise, mean(logb(DCBook_Q), na.rm=TRUE)); colnames(tlog)<-c("Date", "DCBook_Q_mean_log")
tmedlog<-ddply(Quarter, "Date", summarise, median(logb(DCBook_Q), na.rm=TRUE)); colnames(tmedlog)<-c("Date", "DCBook_Q_med_log")

number<-ddply(Quarter, "Date", summarise, length(unique(Ticker))); colnames(number)<-c("Date", "n")
DCRatio_tbl<-left_join(left_join(left_join(left_join(t, tmed),tlog), tmedlog), number)

dev.new()
par(mfrow=c(2,1))

plot(DCRatio_tbl$Date, DCRatio_tbl$DCBook_Q_mean, col="red", type="l")
lines(DCRatio_tbl$Date, DCRatio_tbl$DCBook_Q_med, col="blue")

plot(DCRatio_tbl$Date, DCRatio_tbl$DCBook_Q_mean_log, col="red", type="l")
lines(DCRatio_tbl$Date, DCRatio_tbl$DCBook_Q_med_log, col="blue")

plot(aggregate(t, list(t$Date), median, na.rm=TRUE)[,1],aggregate(t, list(t$Date), median, na.rm=TRUE)[,3], type="l", col="red")


plot(cbind(SMA(t$`40%`, n=4), t$`50%`)[,1], type="l",col="red", ylim=c(0.35, 0.75))
lines(cbind(SMA(t$`40%`, n=4), t$`50%`)[,2], type="l")
lines(cbind(SMA(t$`40%`, n=4), t$`50%`, t$`60%`)[,3], type="l", col="red")

t<-ddply(Qtr, "Ticker", summarize, Duration = (max(Date.EOQ)-min(Date.EOQ))/365, max(Date.EOQ), min(Date.EOQ),nratings = length(unique(Rating_ST_T)),Status = mean(Status)) 
ddply(t, "Status",summarize, round(mean(Duration),1), round(median(Duration),1), round(sd(Duration), 1))
ddply(Qtr, "Status", summarize, Duration=max(Date.EOQ)-min(Date.EOQ))


# for final report
# Debt ratio for top companies
# top10 in terms of Debt issuance Sum: "PNC"  "KEY"  "BK"   "STI"  "STT"  "BBT"  "FITB" "HBAN" "RF"   "MTB" 

QuarterAgg_m<-aggregate(Quarter$DCMarket_Q, by=list(Quarter$Date), med_q<-function(x){median(x, na.rm=T)})
colnames(QuarterAgg_m)<-c("Date", "Ratio")

Top10<-c("PNC", "KEY", "BK", "STI", "STT", "BBT", "FITB", "HBAN", "RF", "MTB" )

dev.new()
par(mfrow=c(3,4))
plot(QuarterAgg$Date, QuarterAgg$Ratio, col="blue", lwd=2, main="Aggregate",type="l",
     ylim =c(min(QuarterAgg$Ratio, QuarterAgg_m$Ratio), max(QuarterAgg$Ratio, QuarterAgg_m$Ratio)) ,ylab="Debt/Capital Ratio", xlab="Year")
lines(QuarterAgg$Date, QuarterAgg_m$Ratio, col="red", lwd=2, main="Aggregate",type="l", ylab="Debt/Book Capital Ratio", xlab="Year")
legend("topright", c("BV", "Market"),lwd=c(2), col=c("blue", "red"), bty="n", seg.len=0.5)

for(i in Top10){
  plot(Qtr[Qtr$Ticker==i,"Date"], Qtr[Qtr$Ticker==i,"DCBook_Q"], type="l", 
       ylim=c(min(Qtr[Qtr$Ticker==i,"DCMarket_Q"], Qtr[Qtr$Ticker==i,"DCBook_Q"])*0.9,max(Qtr[Qtr$Ticker==i,"DCMarket_Q"], Qtr[Qtr$Ticker==i,"DCBook_Q"])*1.1 ), 
       col="blue",  main=i, lwd=1.5,ylab="Debt/Capital Ratio", xlab="Year")
  lines(Qtr[Qtr$Ticker==i,"Date"], Qtr[Qtr$Ticker==i,"DCMarket_Q"], type="l", col="red", lwd=1.5)
  par(new=T)
  plot(Qtr[Qtr$Ticker==i,"Date"], 20-Qtr[Qtr$Ticker==i,"Rating_LT"], type="o",pch=10, col="black", lwd=1.5, xlab=NA, ylab=NA,axes=F , ylim=c(6,20))
  #axis(side=4)
}

# Debt, Longterm debt, MKVALT
dev.new()
par(mfrow=c(3,4))
for(i in Top10){
  plot(Qtr[Qtr$Ticker==i,"Date"], Qtr[Qtr$Ticker==i,"DTQ"], type="l", 
       #ylim=c(min(Qtr[Qtr$Ticker==i,"DCMarket_Q"], Qtr[Qtr$Ticker==i,"DCBook_Q"])*0.9,max(Qtr[Qtr$Ticker==i,"DCMarket_Q"], Qtr[Qtr$Ticker==i,"DCBook_Q"])*1.1 ), 
       col="black",  main=i, lwd=1.5,ylab="Debt/Capital Ratio", xlab="Year")
  lines(Qtr[Qtr$Ticker==i,"Date"], Qtr[Qtr$Ticker==i,"LTDQ"], type="l", col="red", lwd=1.5)
  lines(Qtr[Qtr$Ticker==i,"Date"], Qtr[Qtr$Ticker==i,"MKVALTQ"], type="l", col="blue", lwd=1.5)
}



plot(Qtr[Qtr$Ticker=="PNC","Date"], Qtr[Qtr$Ticker=="PNC","DCBook_Q"], type="l", ylim=c(0.4, 1), col="#ABB065", lwd=1.5)
lines(QuarterAgg$Date, QuarterAgg$Ratio, col="tomato", lwd=2.5)
lines(Qtr[Qtr$Ticker=="KEY","Date"], Qtr[Qtr$Ticker=="KEY","DCBook_Q"], type="l", col="#39BEB1", lwd=1.5)
lines(Qtr[Qtr$Ticker=="BK","Date"], Qtr[Qtr$Ticker=="BK","DCBook_Q"], type="l", col="#ACA4E2", lwd=1.5)
lines(Qtr[Qtr$Ticker=="STI","Date"], Qtr[Qtr$Ticker=="STI","DCBook_Q"], type="l", col="maroon1", lwd=1.5)
lines(Qtr[Qtr$Ticker=="BBT","Date"], Qtr[Qtr$Ticker=="BBT","DCBook_Q"], type="l", col="plum", lwd=1.5)
legend("topright",c("Aggregate", "PNC", "KEY", "BK","STI", "BBT"), lwd=c(2.5), col=c("tomato", "#ABB065", "#39BEB1", "#ACA4E2", "maroon1", "plum"))

#Debt to Capital Ratio
colnames(QuarterAgg)<-c("Date", "Ratio")
QuarterAgg$ln_Ratio<-logb(QuarterAgg$Ratio)
QuarterAgg$ln_diff_Ratio<-c(NA,diff(QuarterAgg$ln_Ratio))



#Document name: Identifying order of integration.docx
#Unit root Tests

X = QuarterAgg[, "ln_Ratio"]
require(urca)
adf.test<-ur.df(X,type="none", lags=0)
summary(adf.test)

adf.test<-ur.df(X,type="none", lags=1)
summary(adf.test)

adf.test<-ur.df(X,type="drift", lags=1)
summary(adf.test)

adf.test<-ur.df(X,type="trend", lags=1)
summary(adf.test)

z<-diff(X)
adf.test.diff<-ur.df(z,type="none", lags=0)
summary(adf.test.diff)

adf.test.diff<-ur.df(z,type="drift", lags=0)
summary(adf.test.diff)

adf.test.diff<-ur.df(z,type="drift", lags=1)
summary(adf.test.diff)

adf.test.diff<-ur.df(z,type="trend", lags=1)
summary(adf.test.diff)

#KPSS unit root test NULL: Stationary
summary(ur.kpss(X,type="mu"))
summary(ur.kpss(X,type="tau"))
summary(ur.kpss(z,type="mu"))
summary(ur.kpss(z,type="tau"))

#Phillips Perron NULL: non-stationary
pp.test(X, type="Z(alpha)")
pp.test(X, type="Z(t_alpha)")
pp.test(z, type="Z(alpha)")
pp.test(z, type="Z(t_alpha)")


#breakpoints with trend for first differenced series, breaks=4

m<-4;plot(X, ylim=c(-0.9, 0.15))
lines(z)
lines(breakpoints(z~tt), col="red", breaks=m)
lines(fitted(breakpoints(z~tt),  breaks=m), col="red")
title(paste("First differenced time series breakpoints with trend (n=",m,")", sep=""))
legend("bottomleft", c("log + differenced","log transformed"))
legend("bottomleft", c("log + differenced","log transformed"), lty=1)
legend("bottomleft", c("log + differenced (above)","log transformed", "fitted with trend"), lty=1, col=c("black", "black", "red"))
coef(breakpoints(z~tt), breaks=m)

#boxplot for each quarter
boxplot(sapply(seq(0, 0.75, by=0.25),
  function(p) exp(X[(index(X)-floor(index(X)))==p])), xlab="Quarter", ylab="Debt to Capital Ratio")

#sarima model with seasonal trend every 4 quarters
model<-arima(window(z, start=1994, end=2008), order= c(3,0,2), seasonal=list(order=c(1,0,0), period=20))

#period between 1994~2008 works well  - break points for n = 3 for z

#AAA yield and breakpoints
AAA<-ts(Index$AAA, start=year(min(Index$Date)), end=year(max(Index$Date)), frequency=12)
plot(AAA)
lines(breakpoints(AAA~ttsub))

#plotting with predicted values of z and original 
plot(ts((c(z, predict(model, n.ahead=60, se.fit=TRUE)$pred)), start=1979, end=2031, frequency=4), type="l")
plot(ts(exp(cumsum(c(z, predict(model, n.ahead=60, se.fit=TRUE)$pred))), start=1979, end=2031, frequency=4), type="l")

#volatility of debt/capacity 
plot((ma((z-ma(z,4))^2, 4)*4/3)^0.5*2, ylim=c(0, 1))
lines(exp(X))
lines(z+0.4, col="red")

#garch?
plot(garch(z)$fitted.values[,1], type="l")

#VAR model - econometric model to capiture the linear interdependencies among multiple time series.
#VAR allows more than one evolving variable. 
