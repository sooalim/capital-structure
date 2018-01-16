#t<-ts(t, start=year(min(t$Date)), end=year(max(t$Date)), frequency=4)

#Bank Profile
dev.new()
par(mfrow=c(4,4))
#k=0
for(i in Active){
  assign(i, unique(Quarter[Quarter$Ticker==i, ]))
  temp<-eval(as.symbol(i))
  n<-NROW(unique(na.omit(temp$Revolving_Rating)))
  #if (((which(Tickers == i, arr.ind=TRUE))%%16)==0){dev.new();par(mfrow=c(4,4))}
  #if ((k%%16)==0){dev.new();par(mfrow=c(6,4))}
if (n>=1){
#  plot(temp$Date, temp$Revolving_Rating, ylim=c(0, 20), type="l", main=eval(i)) 
  print(paste(eval(i), mean(temp$Revolving_Rating, na.rm=T), sep=": "))
  #k=+1
  if (NROW(unique(temp$Rating_LT_T))>1)
    {print(paste(unique(temp$Rating_LT_T), sep=", " ))}
  }
}

tfed<-index(ts(t$Fed_Rate, start=1979, end=2017, frequency=4))

plot(ts(t$`50%`, start=1979, end=2017, frequency=4), type="l")
lines(breakpoints(ts(t$Fed_Rate, start=1979, end=2017, frequency=4)~1))
lines(breakpoints(ts(t$`50%`, start=1979, end=2017, frequency=4)~1), col="red", breaks=4)

plot(ts(t$Fed_Rate, start=1979, end=2017, frequency=4), type="l")
lines(breakpoints(ts(t$`50%`, start=1979, end=2017, frequency=4)~1), col="red", breaks=4)
lines(breakpoints(ts(t$Fed_Rate, start=1979, end=2017, frequency=4)~tfed))

lines(ts(t$AAA, start=1979, end=2017, frequency=4), type="l", col="blue")
lines(breakpoints(ts(t$AAA, start=1979, end=2017, frequency=4)~1), col="blue", breaks=4)
lines(fitted(breakpoints(ts(t$AAA, start=1979, end=2017, frequency=4)~1),  breaks=4), col="blue")

lines(confint(breakpoints(ts(t$`50%`, start=1979, end=2017, frequency=4)~1),  breaks=4), col="red")
lines(confint(breakpoints(ts(t$Fed_Rate, start=1979, end=2017, frequency=4)~1), breaks=4), col="black")
lines(confint(breakpoints(ts(t$AAA, start=1979, end=2017, frequency=4)~1),breaks=4), col="blue")

lines(fitted(breakpoints(ts(t$Fed_Rate, start=1979, end=2017, frequency=4)~tfed)))
coef(breakpoints(ts(t$Fed_Rate, start=1979, end=2017, frequency=4)~tfed))