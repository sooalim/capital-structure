setwd("C:/Users/slim236/Desktop/TAX/Transfer Pricing/Liquidity Modeling/data/alteryx")
library(plm)
source("Load_All.R")

Quarter.ss<-unique(Quarter[Quarter$Date >'2000-03-31' , c("Date","Ticker", "Status", "AAA","BAA", "DTQ", "MKVALTQ", "Common.Equity.Total.Qtly")])
attach(Quarter.ss)

Quarter.ss$log.DCmarket<-logb(ifelse(is.na(DTQ/(DTQ+MKVALTQ)),NA, DTQ/(DTQ+MKVALTQ)))
Quarter.ss$log.DCbook<-logb(ifelse(is.na(DTQ/(DTQ+`Common.Equity.Total.Qtly`)),NA, DTQ/(DTQ+`Common.Equity.Total.Qtly`)))
Quarter.ss$DCmarket<-ifelse(is.na(DTQ/(DTQ+MKVALTQ)),NA, DTQ/(DTQ+MKVALTQ))
Quarter.ss$DCbook<-ifelse(is.na(DTQ/(DTQ+`Common.Equity.Total.Qtly`)),NA, DTQ/(DTQ+`Common.Equity.Total.Qtly`))

Quarter.ss<-Quarter.ss[!is.na(Quarter.ss$DCmarket),]

attach(Quarter.ss)
par(mfrow=c(2,2))
hist(log.DCbook, n=100)
hist(DCbook, n=100)
hist(log.DCmarket, n=100)
hist(DCmarket, n=100)

#temp<-Quarter.ss %>% group_by(Ticker) %>% summarise(n = n()) %>% filter(.,n==68)
#Quarter.ss.b<-Quarter.ss[Quarter.ss$Ticker %in% temp$Ticker, ]
log.metric<-log.DCmarket
metric<-DCmarket

log.fixed.time <- plm(log.metric~poly(BAA, 3, raw=T), 
                      na.action ="na.omit" , 
                      data = Quarter.ss, 
                      index=c("Ticker", "Date"),
                      effect = "individual",
                      model = "within" #fixed effects model
                      )
summary(log.fixed.time)
plot(log.fixed.time, N = 10, seed=1, within=TRUE)
  
  log.random.time <- plm(log.metric~BAA+Status, 
                         na.action ="na.omit" ,  
                         data = Quarter.ss, 
                         index=c("Ticker", "Date"), 
                         model = "random" )
  plot(log.random.time, N = 10, seed=1, random=TRUE)
  legend(x="bottomright")

fixed.time <- plm(metric~AAA, 
                  na.action ="na.omit" , 
                  data = Quarter.ss, 
                  index=c("Ticker", "Date"),
                  xlab=c("Debt to Catpial Ratio"),
                  ylab=c("AAA"),
                  #effect = "time",
                  model = "within" )
summary(fixed.time)
plot(fixed.time, N = 10, seed=1, within=TRUE, cex=0.1)

par(mfrow=c(2,1))
hist(fixed.time$residual, n=50)
plot(fixed.time, N = 10, seed=1, between=TRUE);
dev.off()
random.time <- plm(metric~BAA+Status, 
                   na.action ="na.omit" ,
                   data = Quarter.ss, 
                   index=c("Ticker", "Date"), 
                   model = "random" )

between.time <- plm(metric~BAA+Status, 
                   na.action ="na.omit" ,
                   data = Quarter.ss, 
                   index=c("Ticker", "Date"), 
                   model = "between" , between=TRUE)
within.time <- plm(metric~BAA+Status, 
                    na.action ="na.omit" ,
                    data = Quarter.ss, 
                    index=c("Ticker", "Date"), 
                    model = "within" )
fd.time <- plm(metric~BAA+Status, 
                   na.action ="na.omit" ,
                   data = Quarter.ss, 
                   index=c("Ticker", "Date"), 
                   model = "ht" )
plot(fd.time, N=10)
summary(fd.time)
par(mfrow=c(2,2))
hist(log.fixed.time$residuals, n=100)
hist(fixed.time$residuals, n=100)
hist(log.random.time$residuals, n=100)
hist(random.time$residuals, n=100)

par(mfrow=c(2,2))
plot(log.fixed.time)
title("log.fixed.time ~ BAA")
plot(fixed.time, main="fixed.time")
title("fixed.time ~ BAA")
plot(log.random.time, main="random.time")
title("log.random.time ~ BAA")
plot(random.time, main="random.time")
title("random.time ~ BAA")

summary(fixed.time)
summary(random.time)
plot(log.fixed.time)
plot(random.time)
acf(Quarter.ss$lnDC,lag.max=40, na.action = na.pass, plot=TRUE)
phtest(fixed.time, random.time)