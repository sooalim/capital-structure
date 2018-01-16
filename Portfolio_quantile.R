#Portfolio simlulation for All
library(plm)
source("./portfolio_compute.R")

mean_<-function(x){mean(x, na.rm=TRUE)}
median_<-function(x){median(x, na.rm=TRUE)}
Yrly<-unique(Yrly)

#remove dates with reporting month changes.
ndata<-data.frame()
for (i in unique(Yrly$Ticker)){
  sample<-subset(Yrly,Ticker==i)
  exclude_date<- min(subset(sample, year(sample$Date) %in% subset(count(year(sample$Date)), freq>1, 1))$Date)
  if(i ==unique(Yrly$Ticker)[1]){ndata<-subset(sample, Date!=exclude_date)} 
  else {ndata<-rbind(ndata,subset(sample, Date!=exclude_date))}
}
Yrly<-ndata
Annual_Ratio<-list()
Annual_Ratio$Ticker<-Yrly$Ticker
Annual_Ratio$year<-year(Yrly$Date)

# only for one change 

Annual_Ratio$DCBook<-Yrly$DCBook
Annual_Ratio$DCMarket<-Yrly$DCMarket
Annual_Ratio$LeverageBook<-ifelse(!is.na(Annual_Ratio$DCBook), Yrly$DT/Yrly$CEQT, NA)
Annual_Ratio$LeverageMarket<-ifelse(!is.na(Annual_Ratio$DCMarket), Yrly$DT/Yrly$MKVALT, NA)

Annual_Ratio$Profits<-Yrly$EBIT/Yrly$CEQT
Annual_Ratio$Size<-logb(Yrly$CEQT)
Annual_Ratio$MktToBook<-Yrly$MKVALT/Yrly$CEQT
Annual_Ratio$ProfitMargin<-Yrly$NI/Yrly$Sales
Annual_Ratio$ROE<-Yrly$ROE/100
Annual_Ratio$ROA<-Yrly$ROA/100
Annual_Ratio$EBITCoverage<-Yrly$EBIT/Yrly$IE

Annual_Ratio$NLD<-Yrly$NLD
Annual_Ratio$NLA<-Yrly$NLA
#Annual_Ratio$SubDebt<-Yrly$Sum.Subordinated
#Annual_Ratio$SeniorDebt<-Yrly$Sum.Senior/Yrly$Sum
#Annual_Ratio$sumdebt<-Yrly$Sum
Annual_Ratio$AssetCoverage<-Yrly$AT/(Yrly$LTD+Yrly$NP)
Annual_Ratio$Ratings<-Yrly$Revolving_Rating  
Annual_Ratio$MKVALT<-Yrly$MKVALT
Annual_Ratio$DT<-Yrly$DT
Annual_Ratio$CEQT<-Yrly$CEQT

Yrly$year<-year(Yrly$Date)
bc<-cast(unique(Yrly),year~Ticker, fun.aggregate=mean, value="Capital_Book")
bc1L<-rbind(NA, bc[1:NROW(bc)-1,])
Avg_Capital<-cbind(year=bc$year, (bc[,2:ncol(bc)]+bc1L[,2:ncol(bc1L)]/(2-(is.na(bc[,2:ncol(bc)])+is.na(bc1L[,2:ncol(bc1L)])))))
Avg_Capital<-melt(Avg_Capital, "year")
colnames(Avg_Capital)<-c("year", "Ticker", "Avg_Capital")

Annual_temp<-left_join(Yrly, Avg_Capital, by=c("year", "Ticker") )
Annual_Ratio$ROC<-Annual_temp$EBIT/Annual_temp$Avg_Capital

Annual_Ratio<-data.frame(Annual_Ratio)

#if we want to subset by valid "DCBook" values
Annual_Ratio<-subset(Annual_Ratio, !(Annual_Ratio$Ticker %in% unique(subset(Annual_Ratio, is.na(Annual_Ratio[,"DCBook"]), "Ticker"))))

#if we want to subset for survivors
Annual_Ratio<-subset(Annual_Ratio, Annual_Ratio$Ticker %in% setdiff(Active, BigBanks))

quantile<-data.frame()
var="DCBook"

#portfolio.all - reblanaced every year
for (i in min(Annual_Ratio$year):max(Annual_Ratio$year)){
  x<-subset(Annual_Ratio, Annual_Ratio$year==i)
  y<-cbind(x, quantile=cut(x[,var], breaks = quantile(x[,var], probs = seq(0, 1, 0.25), na.rm=TRUE, names=TRUE), include.lowest = TRUE, labels = 1:4))
  quantile<-rbind(quantile,y)
  #count(quantile[,c("quantile", "year")], vars=c("year","quantile"))
  }

pf.all<-aggregate(quantile$DCBook,by=quantile[,c("year", "quantile")], FUN="median")

plot(x~year, data=pf.all[pf.all$quantile==1,], type="l", ylim=c(min(pf.all$x), max(pf.all$x)))
lines(x~year, data=pf.all[pf.all$quantile==2,], type="l")
lines(x~year, data=pf.all[pf.all$quantile==3,],col="red", type="l")
lines(x~year, data=pf.all[pf.all$quantile==4,], col="blue",type="l")



#example of using portfolio_compute.R function with actual leverage

portfolio_compute(item="DCMarket",criteria="DCMarket", start=1979, end=2016, eventtime=21, 
                            survivor = "ns", scope = "all",
                            main="Debt to Market Capital Portfolio (Pure US Regional Active Banks)", 
                            ylab="Market Leverage")


#example of using portfolio_compute.R function with unexpected leverage
#(Using residuals from regression)

dp<-pdata.frame(Annual_Ratio)
bdp<-make.pbalanced(pdata.frame(Annual_Ratio),type="fill")

# dataset with lagged variables (dp)
for(i in colnames(dp)[7:NCOL(dp)]){
  if(i==colnames(dp)[7]){temp<-plm::lag(dp[,7],k=1)} 
  else {temp<-cbind(temp, plm::lag(dp[,i],k=1))}}
dp_l1<-cbind(dp[,1:6],temp)
colnames(dp_l1)<-colnames(dp)

# dataset with lagged variables (bdp)
for(i in colnames(bdp)[7:NCOL(bdp)]){
  if(i==colnames(bdp)[7]){temp<-plm::lag(bdp[,7],k=1)} 
  else {temp<-cbind(temp, plm::lag(bdp[,i],k=1))}}
bdp_l1<-cbind(bdp[,1:6],temp)
colnames(bdp_l1)<-colnames(bdp)
bdp$crisis<-ifelse(as.numeric(as.character(bdp$year))<2009, 0, 1)

test.bdp<-plm(formula = DCBook ~ Size + NLD + Profits+ MktToBook+ROA+AssetCoverage + ROE + ROC+Ratings+crisis+
                Ticker, data = bdp, effect = c("individual"), model = "within",index = c("Ticker", "year"))
test.bdp<-pgmm(dynformula(DCBook ~Size+crisis+Profits+ROE+AssetCoverage+Ratings, lag=1), data = bdp, effect = c("individual"), model = "twostep", index=c("Ticker", "year"), 
               gmm.inst=~(DCBook), lag.gmm=list(c(2,99)))


test.bdp<-plm(formula = DCBook ~ Size + NLD + Profits+ MktToBook+ROA+AssetCoverage + ROE + ROC+Ratings+
                Ticker, data = bdp, effect = c("twoway"), model = "within", 
              index = c("Ticker", "year"))

test.dp.book<-plm(formula = DCBook ~ Size + NLD + Profits+ MktToBook+ROA+AssetCoverage + ROE + ROC+Ratings
              , data = dp, effect = c("time"), model = "within", 
              index = c("Ticker", "year"))
test.dp.market<-plm(formula = DCMarket ~ Size + NLD + NLA + AssetCoverage + ROE + ROC+
                    Ticker, data = dp, effect = c("twoway"), model = "within", 
                  index = c("Ticker", "year"))

# with lag ****
test.bdp.l1<-plm(formula = DCBook ~ Size + NLD + Profits+ MktToBook+ROA+AssetCoverage + ROE + ROC+Ratings
                 , data = bdp_l1, effect = c("individual"), model = "within", 
              index = c("Ticker", "year"))
test.dp.book.l1<-plm(formula = DCBook ~ Size + NLD + Profits+ MktToBook+ROA+AssetCoverage + ROE + ROC+Ratings
                    , data = dp_l1, effect = c("time"), model = "within", 
                  index = c("Ticker", "year"))
test.dp.market.l1<-plm(formula = DCMarket ~ Size + NLD + NLA + AssetCoverage + ROE + ROC+
                      Ticker, data = dp_l1, effect = c("twoway"), model = "within", index = c("Ticker", "year"))


new.bdp<-cbind(residuals=test.bdp$residuals, test.bdp$model, bdp[rownames(test.bdp$model),]) 
new.dp.book<-cbind(residuals=test.dp.book$residuals, test.dp.book$model, dp[rownames(test.dp.book$model),]) 
new.dp.market<-cbind(residuals=test.dp.market$residuals, test.dp.market$model, dp[rownames(test.dp.market$model),]) 

new.dp.market$year<-as.numeric(as.character(new.dp.market$year))
new.dp.book$year<-as.numeric(as.character(new.dp.book$year))

#Book Leverage
portfolio_compute(data=new.dp.book, item="DCBook",criteria="residuals", start=min(new.dp.book$year), end=2016, eventtime=20, 
                  survivor = "ns", scope = "all",plot.all=FALSE,
                  main="Debt to Book Capital Portfolio (All inclusive, Book Leverage Residual)", 
                  ylab="Book Leverage")

portfolio_compute(data=new.dp.book, item="DCBook",criteria="residuals", start=min(new.dp.book$year), end=2016, eventtime=20, 
                  survivor = "s", scope = "all",plot.all=FALSE,
                  main="Debt to Book Capital Portfolio (Survivors, Book Leverage Residual)", 
                  ylab="Book Leverage")

portfolio_compute(data=new.dp.book, item="DCBook",criteria="residuals", start=min(new.dp.book$year), end=2016, eventtime=20, 
                  survivor = "ns", scope = "pure",plot.all=FALSE,
                  main="Debt to Book Capital Portfolio (US Regional Banks, Book Leverage Residual)", 
                  ylab="Book Leverage")

portfolio_compute(data=new.dp.book, item="DCBook",criteria="residuals", start=min(new.dp.book$year), end=2016, eventtime=20, 
                  survivor = "s", scope = "pure",plot.all=FALSE,
                  main="Debt to Book Capital Portfolio (US Regional Banks(S), Book Residual)", 
                  ylab="Book Leverage")

portfolio_compute(data=new.dp.book, item="DCBook",criteria="residuals", start=min(new.dp.book$year), end=2016, eventtime=20, 
                  survivor = "ns", scope = "active",plot.all=FALSE,
                  main="Debt to Book Capital Portfolio (Active US Regional Banks,Book Residual)", 
                  ylab="Book Leverage")

portfolio_compute(data=new.dp.book, item="DCBook",criteria="residuals", start=min(new.dp.book$year), end=2016, eventtime=20, 
                  survivor = "s", scope = "active",plot.all=FALSE,
                  main="Debt to Book Capital Portfolio (Active US Regional(S), Book Residual)", 
                  ylab="Book Leverage")



#Market Leverage
portfolio_compute(data=new.dp.market, item="DCMarket",criteria="residuals", start=min(new.dp.market$year), end=2016, eventtime=20, 
                  survivor = "ns", scope = "all",plot.all=TRUE,
                  main="Debt to Market Capital Portfolio (All inclusive, Market Leverage Residual)", 
                  ylab="Market Leverage", loc="bottomleft")


portfolio_compute(data=new.dp.market, item="DCMarket",criteria="residuals", start=min(new.dp.market$year), end=2016, eventtime=20, 
                  survivor = "s", scope = "all",plot.all=FALSE,
                  main="Debt to Market Capital Portfolio (Survivors, Market Leverage Residual)", 
                  ylab="Market Leverage")

portfolio_compute(data=new.dp.market, item="DCMarket",criteria="residuals", start=min(new.dp.market$year), end=2016, eventtime=20, 
                  survivor = "ns", scope = "pure",plot.all=FALSE,
                  main="Debt to Market Capital Portfolio (US Regional Banks, Market Residual)", 
                  ylab="Market Leverage")

portfolio_compute(data=new.dp.market, item="DCMarket",criteria="residuals", start=min(new.dp.market$year), end=2016, eventtime=20, 
                  survivor = "s", scope = "pure",plot.all=FALSE,
                  main="Debt to Market Capital Portfolio (US Regional Banks(S), Market Residual)", 
                  ylab="Market Leverage")

portfolio_compute(data=new.dp.market, item="DCMarket",criteria="residuals", start=min(new.dp.market$year), end=2016, eventtime=20, 
                  survivor = "ns", scope = "active",plot.all=FALSE,
                  main="Debt to Market Capital Portfolio (Active US Regional Banks,Market Residual)", 
                  ylab="Market Leverage")

portfolio_compute(data=new.dp.market, item="DCMarket",criteria="residuals", start=min(new.dp.market$year), end=2016, eventtime=20, 
                  survivor = "s", scope = "active",plot.all=FALSE,
                  main="Debt to Market Capital Portfolio (Active US Regional(S), Market Residual)", 
                  ylab="Market Leverage")
