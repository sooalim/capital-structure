source("portfolio_compute.R")
source("portfolio_compute_reverse.R")

#convert to panel data frame, (first two rows are the individual names and time indexes)
diff_vars= c("DT", "MKVALT", "CEQT")
lag_vars= c("Capital_Book", "Capital_Market", "DCBook", "DCMarket", "Size","Profits", "MktToBook" )
control_vars=c("Size", "Profits", "MktToBook", "DCBook", "DCMarket", "NLA", "NLD", "Ratings")

sub.Yrly<-Annual_Ratio[, c("Ticker", "year", diff_vars, lag_vars,control_vars) ]
pdata_yrly<-pdata.frame(sub.Yrly, row.names=FALSE)

# dataset with lagged variables (dp)
#DT(t)-DT(t)/Capital(t-1)

for(i in lag_vars){
  if(i==lag_vars[1]){temp<-plm::lag(pdata_yrly[,i],k=1)} 
  else {temp<-cbind(temp, plm::lag(pdata_yrly[,i],k=1))}}

for(i in diff_vars){
  temp<-cbind(temp, diff(pdata_yrly[,i],lag=1))}


temp<-cbind(pdata_yrly, temp)
colnames(temp)<-c(colnames(temp)[1:NCOL(sub.Yrly)], paste("lag_", lag_vars, sep=""), paste("diff_", diff_vars, sep=""))

temp$NetDTISSU<-temp$diff_DT/temp$lag_Capital_Book
temp$NetEQISSU<-temp$diff_CEQT/temp$lag_Capital_Book
temp$NetMKVAL<-temp$diff_MKVALT/temp$lag_Capital_Market

#computer residual

test.book<-plm(formula = DCBook ~ lag_Size+lag_Profits+lag_MktToBook, data = temp, effect = c("time"), model = "within", 
                  index = c("Ticker", "year"))
new.temp<-cbind(temp[rownames(test.book$model),], residuals=test.book$residuals) 
new.temp$year<-as.numeric(as.character(new.temp$year))

#test.market<-plm(formula = DCMarket ~ Size+Profits+ MktToBook, data = temp, effect = c("time"), model = "within", 
#               index = c("Ticker", "year"))
#new.temp.market<-cbind(temp[rownames(test.market$model),], residuals=test.market$residuals) 
#new.temp.market$year<-as.numeric(as.character(new.temp.market$year))

#criteria based on unexpected book residuals
x<-portfolio_compute(data=new.temp, item="NetDTISSU",criteria="residuals", start=1980, end=2016, eventtime=21, 
                  survivor = "ns", scope = "pure",plot.all=TRUE, plot.confint = FALSE,
                  main="Net Debt Issuing Activiy (US Regionals)", 
                  ylab="Net Debt Issues / Book Capital", loc="topright")

y<-portfolio_compute(data=new.temp, item="NetEQISSU",criteria="residuals", start=1980, end=2016, eventtime=21, 
                  survivor = "ns", scope = "pure",plot.all=TRUE, plot.confint = FALSE,
                  main="Net Equity Issuing Activiy (US Regionals)", 
                  ylab="Net Equity Issues / Book Capital", loc="topright")

portfolio_compute(data=new.temp, item="DCBook",criteria="residuals", start=1980, end=2016, eventtime=21, 
                     survivor = "ns", scope = "pure",plot.all=TRUE, plot.confint = FALSE,
                     main="Net Equity Issuing Activiy (US Regionals)", 
                     ylab="Net Equity Issues / Book Capital", loc="topright")



#Summary table of issuances across time
z<-cbind(aggregate(x$x, by=list(x$quantile), FUN="mean_"),  aggregate(y$x, by=list(y$quantile), FUN="mean_")[,2])
colnames(z)<-c("Quantile", "Avg. Net Debt Issuance", "Avg. Net Equity Issuance")
View(z)

#criteria based on unexpected market residuals (not necessary?)
portfolio_compute(data=new.temp.market, item="NetDTISSU",criteria="residuals", start=1980, end=2016, eventtime=21, 
                  survivor = "ns", scope = "pure",plot.all=TRUE, plot.confint = FALSE,
                  main="Net Debt Issuing Activiy (US Regionals)", 
                  ylab="Net Debt Issues / Market Capital", loc="topright")

portfolio_compute(data=new.temp.market, item="NetMKVAL",criteria="residuals", start=1980, end=2016, eventtime=21, 
                  survivor = "ns", scope = "pure",plot.all=TRUE, plot.confint = FALSE,
                  main="Net Equity Issuing Activiy (US Regionals, Market Value)", 
                  ylab="Change in Total Market Value / Market Capital", loc="topright")


