#Run VECM
library(urca)
library(vars)

# Debt to CApital Ratio is the dependent variables. Yields over the time is the explanatory variable. 
# Model Specification
  
# Find integration order for Debt model 

# Unit root test of log time series
summary(ur.df(logb(x[x$period==1,"50%"]), lags=4, type="trend"))
summary(ur.df(logb(x[x$period==2,"50%"]), lags=4, type="trend"))
summary(ur.df(logb(x[x$period==3,"50%"]), lags=4, type="trend"))


summary(ur.df(logb(x[,"50%"]), lags=4, type="drift"))
summary(ur.df(logb(x[,"50%"]), lags=4, type="none"))

summary(ur.df(logb(x[,"AAA"]), lags=4, type="trend"))
summary(ur.df(logb(x[,"AAA"]), lags=4, type="drift"))
summary(ur.df(logb(x[,"AAA"]), lags=4, type="none"))

# unit root test of differenced log time series
summary(ur.df(diff(logb(x[,"50%"])), lags=4, type="drift"))
summary(ur.df(diff(logb(x[,"AAA"])), lags=4, type="drift"))

# Conclusion
# Based on the test we conclude that Time series is non-stationary while first order differenced TS is stationary
# Yield ~ I(1) 
# Debt to Capital Ratio ~ I(1)

# Cointegration Test



VARselect(x[,2])

plot(auto.arima(x[,2]))


?ur.df
ur.df(logb(x[,2]), lags=4, type="trend")@cval
ur.df(logb(x[,2]), lags=4, type="trend")@cval

ur.df(x[,3], lags=4, type="trend")



# Series must be non-stationary


x<-left_join(t, Index, by="Date")[,c("Date","50%","period", "AAA")]


VARselect(x[,2:3], lag.max=10, type="const")$selection

#Johansen Procedure for VAR
ca.jo(x[,2:3], K=4, type="eigen", ecdet="const", spec="transitory")@cval
ca.jo(x[,2:3], K=4, type="trace", ecdet="const", spec="transitory")@cval
