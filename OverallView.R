setwd("C:/Users/slim236/Desktop/TAX/Transfer Pricing/Liquidity Modeling/data/alteryx")
source("./Load_All.R")
library(fpp)
comp="BBT"
par(mfrow=c(3,2))


#plots overview of Ratings, leverage, Yield, Profitability
plot(DCBook_Q~Date, data=Quarter[Quarter$Ticker==comp,], type='l', ylim=c(0,1))
lines(DCMarket_Q~Date, data=Quarter[Quarter$Ticker==comp,], col="red")
plot(Rating_LT~Date, data=Quarter[Quarter$Ticker==comp,], ylim=c(0, 15), type='b', cex=0.2,main="Size and CR trend")
lines(log.SALEQ~Date, data=Quarter[Quarter$Ticker==comp,])
lines(log.ATQ~Date, data=Quarter[Quarter$Ticker==comp,], col='red')
par(new=T);plot(Rating_LT~Date, data=Quarter[Quarter$Ticker==comp,], ylim=c(0, 15), type='b', cex=0.2, xlab="", axes=F);axis(4)

plot(AAA~Date, data=Quarter[Quarter$Ticker==comp,], type='l', ylim=c(0, 16), main="Yield AAA,BAA,Spread")
lines(BAA~Date, data=Quarter[Quarter$Ticker==comp,], type='l', col='blue')
par(new=T);plot(AAA_BAA~Date, data=Quarter[Quarter$Ticker==comp,], type='l', col='purple', axes=F, ylab="")
axis(4, ylab="BAA-AAA spread")

plot(ROEQ~Date, data=Quarter[Quarter$Ticker==comp,], type='l', lty=2, col='blue', main="Profitability ROE, Op/Book, NIM")
par(new=T);plot(logb(EBDAQ/Capital_Q_Book)~Date, data=Quarter[Quarter$Ticker==comp,], type='l',col="red", axes=F, ylab="")
axis(4, ylab="Profitability/Capital_Book")
par(new=T);plot(logb(NIMQ)~Date, data=Quarter[Quarter$Ticker==comp,], type='l',col="red", axes=F, ylab="")

plot(SPSIRBK~Date, data=Quarter[Quarter$Ticker==comp,], type='l',col="red")
#plots leverage ratio at aggregate level and Autocorrelation

QuarterAgg<-aggregate(Quarter$DCBook_Q, by=list(Quarter$Date), med_q<-function(x){median(x, na.rm=T)})
colnames(QuarterAgg)<-c("Date", "Ratio")

AnnualAgg<-aggregate(Annual$DCBook, by=list(Annual$Date), med_q<-function(x){median(x, na.rm=T)})
QuarterAgg_m<-aggregate(Quarter$DCMarket_Q, by=list(Quarter$Date), med_q<-function(x){median(x, na.rm=T)})
colnames(QuarterAgg_m)<-c("Date", "Ratio")

par(mfrow=c(3,2))
acf(QuarterAgg$Ratio, lag.max=30, na.action=na.omit, main ="Auto correlation function estimation", ylab="autocorrelation value")
pacf(QuarterAgg$Ratio, lag.max=20, na.action=na.omit, main="Partial covariance function estimation plot", ylab = "partial autocorrelation value")
acf(AnnualAgg$x, lag.max=40, na.action=na.omit)

acf(diff(QuarterAgg$Ratio,1), lag.max=40, na.action=na.omit)
pacf(diff(QuarterAgg$Ratio,1), lag.max=40, na.action=na.omit)
acf(diff(AnnualAgg$x,1), lag.max=40, na.action=na.omit)

#decomposes the seasonal trend to separate from seasonality, noise and trend
plot(QuarterAgg$Ratio, type='l')
k<-decompose(ts(QuarterAgg$Ratio, frequency=20))
plot(k)

par(mfrow=c(3,2))

plot(QuarterAgg$Date, k$x, type='l')
plot(QuarterAgg$Date, k$trend, type='l', tck=1)
plot(QuarterAgg$Date, k$seasonal, type='l')
plot(QuarterAgg$Date, k$random, type='h')
hist(k$random, n=50)

dev.new()
period = 20
trend_book=ma(Quarter$DCBook_Q, order=period, centre=T)
par(mfrow=c(4,2))

plot(as.ts(Quarter$DCBook_Q)[1:800],type='l')
lines(trend_book[1:800])

plot(as.ts(trend_book[1:800]))
detrend_book<-as.ts(Quarter$DCBook_Q-trend_book)

m_book=t(matrix(detrend_book, nrow=period))
seasonal = colMeans(m_book, na.rm=T)

plot(as.ts(Quarter$DCBook_Q),type='l', main="original leverage")
noise = Quarter$DCBook_Q - trend_book - seasonal

plot(as.ts(rep(seasonal, 40)), main="seasonality")
plot(noise, main="random_noise")
plot(trend_book, main="trend_book")
hist(noise, n=40)

#detects anomaly



