setwd("C:/Users/slim236/Desktop/TAX/Transfer Pricing/Liquidity Modeling/data/alteryx")
library("TTR")
#source("./Load_All.R")

get_ticker <-function(table, comp){
  colnames(table)<- gsub('Ticker.Q','Ticker', colnames(table))
  x<-filter(table, Ticker==comp)
  x<-na_if(x, 0)
  return(x)
  }

load_Aggregate<-function(temp){
  print(ncol(temp))
  sum_q<-function(x){sum(x, na.rm=T)}
  x<-aggregate(temp[,3:44], by=list(temp$Date), sum_q)
  x<-na_if(x,0)
  colnames(x)[1]<-"Date"
  x<-left_join(x, Index, by="Date")
  o<-order(as.Date(x$Date, "%Y-%m-%d"))
  x<-x[o,]
  x$Date<-as.Date(x$Date, "%Y-%m-%d")
  return(x)
}

x<-load_Aggregate(Quarter)

#plot graphs
DCratio<-function(comp){
  if(comp=='all'){
      temp<-Annual[, c("Date", "Ticker", "CEQT", "DT", "MKVALT")]}
  
  else {
      temp<-Annual[Annual$Ticker==comp, c("Date", "Ticker", "CEQT", "DT", "MKVALT")]}

  sum_q<-function(x){sum(x, na.rm=T)}
  
  x<-aggregate(temp[, 3:ncol(temp)], by = list(temp$Date), FUN=sum_q)
  #p<-aggregate(Bond$Amount.Issued, by= list(Bond$Issue.Date.EOM), sum)
  
  colnames(x)[1]<-c("Date")
  x$Date<-as.Date(x$Date, "%Y-%m-%d")
  DCMarket<-cbind(Date=x$Date, DCM=x$DT/(x$DT+x$MKVALT))
  DCBook<-cbind(Date=x$Date, DCM=x$DT/(x$DT+x$CEQT))

  dev.new()
  plot(DCMarket[,1], DCMarket[,2], 'b', col='blue')
  lines(DCBook[,1], DCBook[,2], 'b', col='red')
  
  dev.new()
  autocorrelation<-acf(DCMarket, lag.max=40, na.action=na.omit, col='blue')

  dev.new()
  autocorrelation.book<-acf(DCBook, lag.max=40, na.action=na.omit, col='red')
  summary(autocorrelation)
  
  return(x)
  }

DCratioQ<-function(comp){
  
  if(comp=='all'){
      temp<-Quarter[Quarter$Status==1 && Quarter$Ticker %in% Active,]}
  
  else {
      temp<-Quarter[Quarter$Ticker==comp,]}
  View(temp)
  sum_q<-function(x){sum(x, na.rm=T)}  
  x<-aggregate(temp[,c(3:44, 46:50)], by = list(temp$Date), FUN=sum_q)
  #p<-aggregate(Bond$Amount.Issued, by= list(Bond$Issue.Date.EOM), sum)
  
  colnames(x)[1]<-c("Date")
  DCMarket<-cbind(Date=x$Date, DCM=x$DTQ/(x$DTQ+x$MKVALTQ))
  DCBook<-cbind(Date=x$Date, DCM=x$DTQ/(x$DTQ+x$`Common.Equity.Total.Qtly`))
  
  dev.new()
  autocorrelation<-acf(logb(DCMarket), lag.max=40, na.action = na.pass)
   
  dev.new() 
  plot(as.Date(x$Date, "%m/%d/%Y"), logb(x$DTQ)-logb(x$DTQ+x$MKVALTQ), 'l', col='blue')
  lines(as.Date(x$Date, "%m/%d/%Y"), logb(x$DTQ)-logb(x$DTQ+x$`Common.Equity.Total.Qtly`), 'l', col='red')

  return(x)
  }
xq<-DCratioQ('all')
yq<-DCratioQ("BBT")

x<-DCratio("all")
y<-DCratio("BBT")


# tally 
Quarter %>% group_by(Status, Ticker) %>% summarise(n = n()) %>% View


#   39 columns
#   for (j in colnames(Quarter)[3]){
#       itemrunSum<-c()
#       
#   for (i in Tickers){
#       try(runsum<-cbind(date=format(Quarter$Date[Quarter$Ticker.Q==i],"%Y-%m-%d"), 
#                       runsum=runSum(Quarter[Quarter$Ticker.Q==i, j],n=4, cumulative=FALSE), 
#                       Ticker=as.character(i)))
#     
#       itemrunSum<-rbind(itemrunSum, runsum)}
#       
#     Quarter[,paste(colnames(Quarter)[3],'12MM')]<-itemrunSum[,"runsum"]
#   }