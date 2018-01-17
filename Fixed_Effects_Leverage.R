# Requires "setup.R" and "portfolio_compute.R"
# Fixed effect - Regression controlled for non-time varying factor (unobserved) 
# and measure the R-square of the portion of this fixed amount.

# In orther words, we set an assumption that whatever effects the leverage at one year 
# will have the same effect at a different point in teim because they do not change.

# One factor fixed effects approach
# - Group means
# - Demeaning
# - First differences

#example of using portfolio_compute.R function with unexpected leverage
#(Using residuals from regression)
source("portfolio_compute.R")
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

#bdp$crisis<-ifelse(as.numeric(as.character(bdp$year))<2009, 0, 1)

test.bdp<-plm(formula = DCBook ~ Size + Profits+ MktToBook, data = bdp, effect = c("time"), model = "within",index = c("Ticker", "year"))

test.bdp.gmm<-pgmm(dynformula(DCBook ~Size+Profits+MktToBook, lag=1), data = bdp, effect = c("individual"), model = "onestep", index=c("Ticker", "year"), 
               gmm.inst=~(DCBook), lag.gmm=list(c(2,99)), time.dummies=TRUE)

test.bdp<-plm(formula = DCBook ~ Size + NLD + Profits+ MktToBook+ROA+AssetCoverage + ROE + ROC+Ratings+
                Ticker, data = bdp, effect = c("twoway"), model = "within", 
              index = c("Ticker", "year"))

test.dp.book<-plm(formula = DCBook ~ Size +NLD+Profits+ MktToBook, data = dp, effect = c("time"), model = "within", 
                  index = c("Ticker", "year"))

test.dp.market<-plm(formula = DCMarket ~ Size + Profits+MktToBook, data = dp, effect = c("time"), model = "within", 
                    index = c("Ticker", "year"))
fixef(test.dp.market)

coeftest(test.dp.market, vcov.=vcovHC)

summary(test.dp.market)
summary(test.dp.book)
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
new.dp.book$year<-as.numeric(as.character(new.dp.book$year))

new.dp.market<-cbind(residuals=test.dp.market$residuals, test.dp.market$model, dp[rownames(test.dp.market$model),]) 
new.dp.market$year<-as.numeric(as.character(new.dp.market$year))

#Book Leverage
portfolio_compute(data=new.dp.book, item="DCBook",criteria="residuals", start=min(new.dp.book$year), end=2016, eventtime=20, 
                  survivor = "ns", scope = "all",plot.all=FALSE,
                  main="Debt to Book Capital Portfolio (All inclusive, Book Leverage Residual)", 
                  ylab="Book Leverage", loc="bottomright")


portfolio_compute(data=new.dp.book, item="DCBook",criteria="residuals", start=min(new.dp.book$year), end=2016, eventtime=20, 
                  survivor = "s", scope = "all",plot.all=FALSE,
                  main="Debt to Book Capital Portfolio (Survivors, Book Leverage Residual)", 
                  ylab="Book Leverage")

portfolio_compute(data=new.dp.book, item="DCBook",criteria="residuals", start=min(new.dp.book$year), end=2016, eventtime=20, 
                  survivor = "ns", scope = "all",plot.all=FALSE,
                  main="Debt to Book Capital Portfolio (US Regional Banks, Book Residual)", 
                  ylab="Book Leverage", loc="bottomright")


#Market Leverage
portfolio_compute(data=new.dp.market, item="DCMarket",criteria="residuals", start=min(new.dp.market$year), end=2016, eventtime=20, 
                  survivor = "ns", scope = "all",plot.all=TRUE,
                  main="Debt to Market Capital Portfolio (All inclusive, Market Leverage Residual)", 
                  ylab="Market Leverage", loc="topright")


portfolio_compute(data=new.dp.market, item="DCMarket",criteria="residuals", start=min(new.dp.market$year), end=2016, eventtime=20, 
                  survivor = "s", scope = "all",plot.all=FALSE,
                  main="Debt to Market Capital Portfolio (Survivors, Market Leverage Residual)", 
                  ylab="Market Leverage")

portfolio_compute(data=new.dp.market, item="DCMarket",criteria="residuals", start=min(new.dp.market$year), end=2016, eventtime=20, 
                  survivor = "ns", scope = "pure",plot.all=FALSE,
                  main="Debt to Market Capital Portfolio (US Regional Banks, Market Residual)", 
                  ylab="Market Leverage")

#Leverage Reverse
#Book - US Regionals
portfolio_compute_reverse(data=Annual_Ratio, item="DCBook",criteria="DCBook", start=max(new.dp.book$year), end=1980, eventtime=20, 
                          survivor = "ns", scope = "pure",plot.all=FALSE,
                          main="Debt to Book Capital Portfolio (US Regionals, Reverse)", 
                          ylab="Book Leverage", loc="bottomleft")
#Market - US Regionals
portfolio_compute_reverse(data=Annual_Ratio, item="DCMarket",criteria="DCMarket", start=max(new.dp.book$year), end=1980, eventtime=20, 
                          survivor = "ns", scope = "pure",plot.all=FALSE,
                          main="Debt to Market Capital Portfolio (US Regionals, Reverse)", 
                          ylab="Book Leverage", loc="bottomleft")
#Book - Survivors
portfolio_compute_reverse(data=new.dp.book, item="DCBook",criteria="DCBook", start=max(new.dp.book$year), end=1980, eventtime=20, 
                          survivor = "s", scope = "pure",plot.all=FALSE,
                          main="Debt to Book Capital Portfolio (Survivors, Reverse)", 
                          ylab="Book Leverage", loc="bottomleft")
#Market - Survivors
portfolio_compute_reverse(data=new.dp.book, item="DCMarket",criteria="DCMarket", start=max(new.dp.book$year), end=1980, eventtime=20, 
                          survivor = "s", scope = "pure",plot.all=FALSE,
                          main="Debt to Market Capital Portfolio (Survivors, Reverse)", 
                          ylab="Book Leverage", loc="bottomleft")
#Book - US Regionals - Residuals
portfolio_compute_reverse(data=new.dp.book, item="DCBook",criteria="residuals", start=max(new.dp.book$year), end=1980, eventtime=20, 
                          survivor = "ns", scope = "pure",plot.all=FALSE,
                          main="Debt to Book Capital Portfolio (US Regionals, Reverse)", 
                          ylab="Book Leverage", loc="bottomleft")
#Market - US Regionals - Residuals
portfolio_compute_reverse(data=new.dp.market, item="DCMarket",criteria="residuals", start=max(new.dp.book$year), end=1980, eventtime=20, 
                          survivor = "ns", scope = "pure",plot.all=FALSE,
                          main="Debt to Market Capital Portfolio (US Regionals, Reverse)", 
                          ylab="Book Leverage", loc="bottomleft")

#Book - US Regionals - survivors -  Residuals
portfolio_compute_reverse(data=new.dp.book, item="DCBook",criteria="residuals", start=max(new.dp.book$year), end=1980, eventtime=20, 
                          survivor = "s", scope = "pure",plot.all=FALSE,
                          main="Debt to Book Capital Portfolio (Survivors, Reverse)", 
                          ylab="Book Leverage", loc="bottomleft")
#Market - US Regionals - survivors - Residuals
portfolio_compute_reverse(data=new.dp.market, item="DCMarket",criteria="residuals", start=max(new.dp.book$year), end=1980, eventtime=20, 
                          survivor = "s", scope = "pure",plot.all=FALSE,
                          main="Debt to Market Capital Portfolio (US Regionals, Reverse)", 
                          ylab="Book Leverage", loc="bottomleft")



