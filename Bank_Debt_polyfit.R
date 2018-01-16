setwd("C:/Users/slim236/Desktop/TAX/Transfer Pricing/Liquidity Modeling/data/alteryx")
source("./Bank_Debt.R")
source("./Loan_All.R")
par(mfrow=c(2,2))

BM<-load_Aggregate(Quarter)
BM$DCMarket<-BM$DTQ/(BM$DTQ+BM$Common.Equity.Total.Qtly)
plot(BM$DTQ/(BM$DTQ+BM$Common.Equity.Total.Qtly)~BM$AAA_BAA, col="black",ylab="Debt to Capital Ratio (Market)", main="Debt to Capital Ratio (Market) trend", type='p', cex=0.5)
lm1<-lm(BM$DCMarket~poly(BM$AAA_BAA, 2, raw=T), data=BM)
points(BM$AAA_BAA, predict(lm1, data.frame(BM$AAA_BAA), raw=T), type='p', col='red', lty=2)


plot(BM$DTQ/(BM$DTQ+BM$Common.Equity.Total.Qtly)~BM$Date, col="black",ylab="Debt to Capital Ratio (Market)", main="Debt to Capital Ratio (Market) trend", type='p', cex=0.5)
lm1<-lm(BM$DCMarket~poly(as.numeric(BM$Date), 3, raw=T), data=BM)
points(BM$Date, predict(lm1, data.frame(as.numeric(BM$Date)), raw=T), type='p', col='red', lty=2)



for (degree in 1:3) {
  fm<-lm(BM$DCMarket~poly(BM$AAA_BAA, degree, raw=T))
  print(summary(fm))
  assign(paste("debt.ratio", degree, sep="."), fm)
  points(BM$AAA_BAA, predict(fm, data.frame(BM$AAA_BAA), raw=T), col=degree, lty=2)
}

par(new=T);plot(BM$AAA~BM$Date, col='red', type='l')
par(new=T);plot(BM$BAA~BM$Date, col='blue', type='b', pch=2)
par(new=T);plot(BM$BAA-BM$AAA~BM$Date, col='purple', type='b', pch=2, cex=0.2)

anova(debt.ratio.1,debt.ratio.2,debt.ratio.3,debt.ratio.4)

dev.new()
BAC<-get_ticker(Quarter, "PNC")
BAC$DCMarket<-BAC$DTQ/(BAC$DTQ+BAC$MKVALTQ)
plot(BAC$DTQ/(BAC$DTQ+BAC$MKVALTQ)*100~BAC$Date, col="black",ylab="Debt to Capital Ratio (Market)", main="Debt to Capital Ratio (Market) trend", type='l', cex=0.5)
#BAC$SPSIRBK+BAC$AAA+
#lines(BAC$DTQ/(BAC$DTQ+BAC$Common.Equity.Total.Qtly)*100~BAC$Date, col="red", type='l',cex=0.5)

for (degree in 1:4) {
  fm<-lm(BAC$DCMarket*100~poly(BAC$BAA, degree, raw=T))
  print(summary(fm))
  assign(paste("debt.ratio", degree, sep="."), fm)
  lines(BAC$Date, predict(fm, data.frame(BAC$Date)), col=degree, lty=2)
}

anova(debt.ratio.1,debt.ratio.2,debt.ratio.3,debt.ratio.4)


dev.new()
BAC$DCBook<-BAC$DTQ/(BAC$DTQ+BAC$Common.Equity.Total.Qtly)
plot(BAC$DCBook*100~BAC$Date, col="black",ylab="Debt to Capital Ratio (Book)",main="Debt to Capital Ratio (Book) trend", type='l', cex=0.5, bg="red")

for (degree in 1:7) {
  fm<-lm(BAC$DCBook*100~poly(BAC$Date, degree))
  assign(paste("debt.ratio", degree, sep="."), fm)
  lines(BAC$Date, predict(fm, data.frame(BAC$Date)), col=degree)
}

anova(debt.ratio.1,debt.ratio.2,debt.ratio.3,debt.ratio.4)