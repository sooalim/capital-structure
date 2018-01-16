library(plyr)

setwd("C:/Users/slim236/Desktop/TAX/Transfer Pricing/Liquidity Modeling/data/alteryx")
source("Bond Data.R")
source("multiplot.R")
# plot_trend <-function(combined){
#   require(ggplot2)
#   ggplot(combined, aes(Date,Sum))+geom_line(data=combined,aes(Date,Sum), col="red")+geom_ribbon(data=combined, fill=rgb(1,0,0,0.2),aes(ymin=0.8*Sum,ymax=1.2*Sum, colour=rgb(1, 0,0,0.3)))
# }
# plot_trend(c)

#plot(Sum~Date, data=Aggtrend(Bonds), type='l', xlim=c(today()-years(30), today()+years(10)))

#monthly
Priority<-run_trend("Priority")
Type<-run_trend("Type")
Type2<-run_trend("Type2")
Total<-run_trend(NA)


#used to be joined to Bank_Debt data table  - Quarter


#Quarterly Adjusted - Total by ticker, Seniority, time
P.Sum<-aggregate(Priority[,c("Issued","Matured")], by=list(as.Date(Priority$Date.EOQ),Priority$Priority, Priority$Ticker), sum_q<-function(x){sum(x,na.rm=T)})
P.Sum$Diff<-P.Sum$Issued-P.Sum$Matured
P.Sum$Sum<-cumsum(P.Sum$Diff)
colnames(P.Sum)<-c("Date.EOQ", "Priority", "Ticker", "Issued", "Matured", "Diff", "Sum")
P.Sum<-reshape(P.Sum, timevar="Priority", idvar=c("Date.EOQ", "Ticker"), direction="wide", drop="Diff")


#Quarterly Adjusted - Total by ticker, Type, time
T.Sum<-aggregate(Type[,c("Issued","Matured")], by=list(as.Date(Type$Date.EOQ),Type$Type, Type$Ticker), sum_q<-function(x){sum(x,na.rm=T)})
T.Sum$Diff<-T.Sum$Issued-T.Sum$Matured
T.Sum$Sum<-cumsum(T.Sum$Diff)
colnames(T.Sum)<-c("Date.EOQ", "Type", "Ticker", "Issued", "Matured", "Diff", "Sum")
T.Sum<-reshape(T.Sum, timevar="Type", idvar=c("Date.EOQ", "Ticker"), direction="wide", drop="Diff")

#Quarterly Adjusted - Total by ticker, Type2, time
T2.Sum<-aggregate(Type2[,c("Issued","Matured")], by=list(as.Date(Type2$Date.EOQ),Type2$Type2, Type2$Ticker), sum_q<-function(x){sum(x,na.rm=T)})
T2.Sum$Diff<-T2.Sum$Issued-T2.Sum$Matured
T2.Sum$Sum<-cumsum(T2.Sum$Diff)
colnames(T2.Sum)<-c("Date.EOQ", "Type2", "Ticker", "Issued", "Matured", "Diff", "Sum")
T2.Sum<-reshape(T2.Sum, timevar="Type2", idvar=c("Date.EOQ", "Ticker"), direction="wide", drop="Diff")


#Quarterly Adjusted - Total by ticker, time
TO.Sum<-aggregate(Total[,c("Issued","Matured")], by=list(as.Date(Total$Date.EOQ), Total$Ticker), sum_q<-function(x){sum(x,na.rm=T)})
TO.Sum$Diff<-TO.Sum$Issued-TO.Sum$Matured
TO.Sum$Sum<-cumsum(TO.Sum$Diff)
colnames(TO.Sum)<-c("Date.EOQ", "Ticker", "Issued", "Matured", "Diff", "Sum")

#Join tables
Quarter$Date.EOQ<-as.Date(Quarter$Date.EOQ)
test<-left_join(Quarter, P.Sum)
test<-left_join(test, T.Sum)
test<-left_join(test, T2.Sum)
test<-left_join(test, TO.Sum)


#Annualy Adjusted - Total by ticker, Seniority, time
A.P.Sum<-aggregate(Priority[,c("Issued","Matured")], by=list(year(as.Date(Priority$Date.EOQ)),Priority$Priority, Priority$Ticker), sum_q<-function(x){sum(x,na.rm=T)})
A.P.Sum$Diff<-A.P.Sum$Issued-A.P.Sum$Matured
A.P.Sum$Sum<-cumsum(A.P.Sum$Diff)
colnames(A.P.Sum)<-c("Date", "Priority", "Ticker", "Issued", "Matured", "Diff", "Sum")
A.P.Sum$Date<-as.Date(paste(A.P.Sum$Date+1, 1, 1, sep="-"))-1
A.P.Sum<-reshape(A.P.Sum, timevar="Priority", idvar=c("Date", "Ticker"), direction="wide", drop="Diff")

#Annualy Adjusted - Total by ticker, Type, time
A.T.Sum<-aggregate(Type[,c("Issued","Matured")], by=list(year(as.Date(Type$Date.EOQ)),Type$Type, Type$Ticker), sum_q<-function(x){sum(x,na.rm=T)})
A.T.Sum$Diff<-A.T.Sum$Issued-A.T.Sum$Matured
A.T.Sum$Sum<-cumsum(A.T.Sum$Diff)
colnames(A.T.Sum)<-c("Date.EOQ", "Type", "Ticker", "Issued", "Matured", "Diff", "Sum")
A.T.Sum$Date<-as.Date(paste(A.T.Sum$Date+1, 1, 1, sep="-"))-1
A.T.Sum<-reshape(A.T.Sum, timevar="Type", idvar=c("Date", "Ticker"), direction="wide", drop="Diff")

#Annualy Adjusted - Total by ticker, Type, time
A.T2.Sum<-aggregate(Type2[,c("Issued","Matured")], by=list(year(as.Date(Type2$Date.EOQ)),Type2$Type2, Type2$Ticker), sum_q<-function(x){sum(x,na.rm=T)})
A.T2.Sum$Diff<-A.T2.Sum$Issued-A.T2.Sum$Matured
A.T2.Sum$Sum<-cumsum(A.T2.Sum$Diff)
colnames(A.T2.Sum)<-c("Date.EOQ", "Type2", "Ticker", "Issued", "Matured", "Diff", "Sum")
A.T2.Sum$Date<-as.Date(paste(A.T2.Sum$Date+1, 1, 1, sep="-"))-1
A.T2.Sum<-reshape(A.T2.Sum, timevar="Type2", idvar=c("Date", "Ticker"), direction="wide", drop="Diff")

#Annualy Adjusted - Total by ticker, time
A.TO.Sum<-aggregate(Total[,c("Issued","Matured")], by=list(year(as.Date(Total$Date.EOQ)), Total$Ticker), sum_q<-function(x){sum(x,na.rm=T)})
A.TO.Sum$Diff<-A.TO.Sum$Issued-A.TO.Sum$Matured
A.TO.Sum$Sum<-cumsum(A.TO.Sum$Diff)
colnames(A.TO.Sum)<-c("Date", "Ticker", "Issued", "Matured", "Diff", "Sum")
A.TO.Sum$Date<-as.Date(paste(A.TO.Sum$Date+1, 1, 1, sep="-"))-1

#Join tables
A.test<-left_join(Annual, A.P.Sum)
A.test<-left_join(A.test, A.T.Sum)
A.test<-left_join(A.test, A.T2.Sum)
A.test<-left_join(A.test, A.TO.Sum)

Yrly<-A.test
rm(A.test)

Qtr<-test
rm(test)



t<-ddply(Quarter, "Date", summarise, mean(DCBook_Q, na.rm=TRUE)); colnames(t)<-c("Date", "DCBook_Q_mean")
tmed<-ddply(Quarter, "Date", summarise, median(DCBook_Q, na.rm=TRUE)); colnames(tmed)<-c("Date", "DCBook_Q_med")

tlog<-ddply(Quarter, "Date", summarise, mean(logb(DCBook_Q), na.rm=TRUE)); colnames(tlog)<-c("Date", "DCBook_Q_mean_log")
tmedlog<-ddply(Quarter, "Date", summarise, median(logb(DCBook_Q), na.rm=TRUE)); colnames(tmedlog)<-c("Date", "DCBook_Q_med_log")

number<-ddply(Quarter, "Date", summarise, length(unique(Ticker))); colnames(number)<-c("Date", "n")
DCRatio_tbl<-left_join(left_join(left_join(left_join(t, tmed),tlog), tmedlog), number)


#Used for plot generation in Word document (Total by time)

#Quarterly Adjusted  - Total by Time, Seniority
m<-aggregate(Priority$Sum, by=list(as.Date(Priority$Date.EOQ),Priority$Priority),sum)
colnames(m)<-c("Date.EOQ", "Priority", "Sum")

#Quarterly Adjusted  - Total by Time, Type
n<-aggregate(Type2$Sum, by=list(as.Date(Type2$Date.EOQ),Type2$Type2),sum)
colnames(n)<-c("Date.EOQ", "Type", "Sum")

#Quarterly Adjusted  - Total by Time
mTot<-aggregate(Total$Sum, by=list(as.Date(Total$Date.EOQ)),sum)
colnames(mTot)<-c("Date.EOQ", "Sum")


#Plot composition of Amounts for different Priorities and Types of Bonds
#Sum

dev.new()
Priority1<-Priority[Priority$Date==Priority$Date.EOQ,]
temp<-left_join(Priority1, DCRatio_tbl, by="Date")

p0<-ggplot(Priority1[Priority1$Date<today()&Priority1$Date.EOQ>today()-years(30),], aes(x=Date, y=Sum, fill=Priority))+
  geom_bar(alpha=1, stat="identity")+scale_fill_brewer(palette="Spectral")+
  geom_line(stat="identity", data=temp[temp$Priority=="Senior",], aes(x=Date, y=(1-DCBook_Q_mean)*2e+11), colour="red4", linetype=1, size=1)+
  scale_y_continuous(expand=c(0,0),sec.axis = sec_axis(~./2e+11, name="Equity to Capital Ratio [%]"))+
  xlim(min(DCRatio_tbl$Date), max(DCRatio_tbl$Date)); plot(p0)

Type1<-Type2[Type2$Date==Type2$Date.EOQ,]
temp2<-left_join(Type1, DCRatio_tbl, by="Date")
p0.2<-ggplot(Type2[Type2$Date.EOQ<today()&Type2$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Sum, fill=Type2))+
  geom_bar(alpha=1, stat="identity")+
  scale_fill_brewer(palette="Spectral")+
  geom_line(stat="identity", data=temp2[temp2$Type2=="Senior Bonds",], aes(x=Date, y=((1-DCBook_Q_mean))*5.5e+11), colour="red4", linetype=1, size=1)+
  scale_y_continuous(sec.axis = sec_axis(~./5.5e+11, name="Equity to Capital Ratio [%]"))+
  xlim(min(DCRatio_tbl$Date), max(DCRatio_tbl$Date)); plot(p0.2)

p1<-ggplot(Priority[Priority$Date.EOQ<today()&Priority$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Sum, fill=Priority))+geom_bar(alpha=1, stat="identity")+scale_fill_brewer(palette="Spectral")

p2<-ggplot(Type2[Type2$Date.EOQ<today()+years(20)&Type2$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Sum, fill=Type2))+geom_bar(alpha=1, stat="identity")
    +scale_fill_brewer(palette="Spectral")

p3<-ggplot(m[m$Date.EOQ<today()&m$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Sum, fill=Priority))+
  geom_bar(alpha=1, stat="identity",position="fill")+
  scale_fill_brewer(palette="Accent")

p5<-ggplot(n[n$Date.EOQ<today()&n$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Sum, fill=Type))+
  geom_bar(alpha=1, stat="identity",position="fill")+
  scale_fill_brewer(palette="Spectral")+ylim(0,NA)

  
p1<-p1+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p2<-p2+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p3<-p3+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p5<-p5+scale_x_date(date_breaks = "5 year", date_labels="%Y")

#p0<-p0+scale_x_date(date_breaks = "5 year", date_labels="%Y")
#p0.2<-p0.2+scale_x_date(date_breaks = "5 year", date_labels="%Y")

multiplot(p1, p2, p3,p5) #composition of Debt 
multiplot(p0, p3,p0.2, p5) # added EQuity to Debt ratio on the plot

#Issued
m<-aggregate(Priority$Issued, by=list(as.Date(Priority$Date.EOQ),Priority$Priority),sum)
colnames(m)<-c("Date.EOQ", "Priority", "Issued")

#Quarterly Adjusted  - Total by Time, Type
n<-aggregate(Type$Issued, by=list(as.Date(Type2$Date.EOQ),Type2$Type2),sum)
colnames(n)<-c("Date.EOQ", "Type", "Issued")


dev.new()
p1<-ggplot(Priority[Priority$Date.EOQ<today()+years(20)&Priority$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Issued, fill=Priority))+geom_bar(alpha=1, stat="identity")+scale_fill_brewer(palette="Spectral")
p2<-ggplot(Type2[Type2$Date.EOQ<today()+years(20)&Type2$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Issued, fill=Type2))+geom_bar(alpha=1, stat="identity")+scale_fill_brewer(palette="Spectral")
p3<-ggplot(m[m$Date.EOQ<today()+years(20)&m$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Issued, fill=Priority))+geom_bar(alpha=1, stat="identity",position="fill")+scale_fill_brewer(palette="Accent")
p5<-ggplot(n[n$Date.EOQ<today()+years(20)&n$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Issued, fill=Type))+geom_bar(alpha=1, stat="identity",position="fill")+scale_fill_brewer(palette="Spectral")+ylim(0,NA)

p1<-p1+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p2<-p2+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p3<-p3+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p5<-p5+scale_x_date(date_breaks = "5 year", date_labels="%Y")

multiplot(p1, p2, p3,p5)


#Matured
m<-aggregate(Priority$Matured, by=list(as.Date(Priority$Date.EOQ),Priority$Priority),sum)
colnames(m)<-c("Date.EOQ", "Priority", "Matured")

#Quarterly Adjusted  - Total by Time, Type
n<-aggregate(Type2$Matured, by=list(as.Date(Type2$Date.EOQ),Type2$Type2),sum)
colnames(n)<-c("Date.EOQ", "Type", "Matured")


dev.new()
p1<-ggplot(Priority[Priority$Date.EOQ<today()+years(20)&Priority$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Matured, fill=Priority))+geom_bar(alpha=1, stat="identity")+scale_fill_brewer(palette="Spectral")
p2<-ggplot(Type2[Type2$Date.EOQ<today()+years(20)&Type2$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Matured, fill=Type2))+geom_bar(alpha=1, stat="identity")+scale_fill_brewer(palette="Spectral")
p3<-ggplot(m[m$Date.EOQ<today()+years(20)&m$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Matured, fill=Priority))+geom_bar(alpha=1, stat="identity",position="fill")+scale_fill_brewer(palette="Accent")
p5<-ggplot(n[n$Date.EOQ<today()+years(20)&n$Date.EOQ>today()-years(30),], aes(x=Date.EOQ, y=Matured, fill=Type))+geom_bar(alpha=1, stat="identity",position="fill")+scale_fill_brewer(palette="Spectral")+ylim(0,NA)

p1<-p1+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p2<-p2+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p3<-p3+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p5<-p5+scale_x_date(date_breaks = "5 year", date_labels="%Y")

multiplot(p1, p2, p3,p5)



#
dev.new()

p1<-ggplot(Priority[Priority$Date.EOQ<today(),], aes(x=Date.EOQ, y=Issued, fill=Priority))+geom_bar(alpha=1, stat="identity")+scale_fill_brewer(palette="Spectral")
p2<-ggplot(Type[Type$Date.EOQ<today(),], aes(x=Date.EOQ, y=Issued, fill=Type))+geom_bar(alpha=1, stat="identity")+scale_fill_brewer(palette="Spectral")
p3<-ggplot(Priority[Priority$Date.EOQ<today(),], aes(x=Date.EOQ, y=Issued, fill=Priority))+geom_bar(alpha=1, stat="identity",position="fill")+scale_fill_brewer(palette="Spectral")
p4<-ggplot(Type[Type$Date.EOQ<today(),], aes(x=Date.EOQ, y=Issued, fill=Type))+geom_bar(alpha=1, stat="identity", position="fill")+scale_fill_brewer(palette="Spectral")
p5<-ggplot(n[n$Date.EOQ<today(),], aes(x=Date.EOQ, y=Sum, fill=Type))+geom_bar(alpha=1, stat="identity",position="fill")+scale_fill_brewer(palette="Spectral")+ylim(0,NA)
p6<-ggplot(m[m$Date.EOQ<today(),], aes(x=Date.EOQ, y=Sum, fill=Priority))+geom_bar(alpha=1, stat="identity",position="fill")+scale_fill_brewer(palette="Spectral")+ylim(0,NA)

p7<-ggplot(Index[Index$Date>min(m$Date.EOQ),],aes(x=Date, y=AAA))+geom_line()
p8<-ggplot(Index[Index$Date>min(m$Date.EOQ),],aes(x=Date, y=Fed_Rate))+geom_line()
p9<-ggplot(Index[Index$Date>min(m$Date.EOQ),],aes(x=Date, y=AAA-Fed_Rate))+geom_line()
multiplot(p7, p8,p9)

p1<-p1+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p2<-p2+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p3<-p3+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p4<-p4+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p5<-p5+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p6<-p6+scale_x_date(date_breaks = "5 year", date_labels="%Y")
p7<-p7+scale_x_date(date_breaks = "5 year", date_labels="%Y")

multiplot(p5, p6, p8)


# Top 10 plots for annual and quarterly inssued bonds  - Cumulative and raw
ordered<-aggregate(Yrly$Issued, by=list(Yrly$Ticker), sum_q<-function(x){sum(x, na.rm=T)})
o<-order(ordered$x, decreasing=T)
top10<-ordered[o[1:10],"Group.1"]
top10Q<-TO.Sum[TO.Sum$Ticker %in% top10,]
top10A<-A.TO.Sum[A.TO.Sum$Ticker %in% top10,]

p1<-ggplot(top10Q, aes(Date.EOQ, Issued, fill=Ticker))+geom_bar(stat="identity", alpha=1)+
  scale_fill_brewer(palette = "Spectral")+xlim(today()-years(20),today())
p2<-ggplot(top10Q, aes(Date.EOQ, Sum, fill=Ticker))+geom_bar(stat="identity", alpha=1)+
  scale_fill_brewer(palette = "Spectral")+xlim(today()-years(20),today()+years(20))
p3<-ggplot(top10Q, aes(Date.EOQ, Matured, fill=Ticker))+geom_bar(stat="identity", alpha=1)+
  scale_fill_brewer(palette = "Spectral")+xlim(today()-years(20),today()+years(20))

#p3<-ggplot(top10A, aes(Date, Issued, fill=Ticker))+geom_bar(stat="identity", alpha=1)+
#  scale_fill_brewer(palette = "Spectral")+xlim(today()-years(20),today())
#p4<-ggplot(top10A, aes(Date, Sum, fill=Ticker))+geom_bar(stat="identity", alpha=1, position="fill")+
#  scale_fill_brewer(palette = "Spectral")+xlim(today()-years(20),today()+years(20))
multiplot(p1, p2, p3)




#plot of Debt to Capital ratio, book  of all companies

plot_all <- function(dataset, measure,date_var, list){
  dataset<-dataset
  metric <- measure
  Banklist<-list
  time<-unique(dataset[, date_var])
  
  range = quantile(dataset[, metric], c(0.02,0.5, 0.98), na.rm=T)
  for (i in 1:length(Banklist)){
      if (i ==1){
      plot(dataset[dataset$Ticker==Banklist[i],metric]~dataset[dataset$Ticker==Banklist[i], date_var], col="red", type="l", ylim = c(range[1]*0.9,range[3]*1.1), xlim=c(min(time), max(time)))}
      else lines(dataset[dataset$Ticker==Banklist[i],metric]~dataset[dataset$Ticker==Banklist[i], date_var], col="red", type="l")

  }
  
  lines(rep(range[1], length(time))~time, lty=2, col="blue", lwd = 1, type="l")
  lines(rep(range[2], length(time))~time, lty=2, col="blue", lwd = 2, type="l")
  lines(rep(range[3], length(time))~time, lty=2, col="blue", lwd = 1, type="l")
}

Qtr$IntCoverage<-Qtr$EBITQ/Qtr$DTQ
measure = "IntCoverage"
date_var= "Date.EOQ"
list= Active
dataset = Qtr


plot_all(dataset, measure, date_var, list)