
source("./LeeStrazicich.R")

model<-ur.ls(logb(t[,2]), model="break", breaks=2, lags=5, method="GTOS", pn=0.1, print.results="print")
ur.ls(PNC$DCBook_Q, model="break", breaks=2, lags=5, method="GTOS", pn=0.1, print.results="print")
ur.ls(logb(t[61:153,2]), model="break", breaks=2, lags=5, method="GTOS", pn=0.1, print.results="print")
ur.ls(logb(t[1:124,2]), model="break", breaks=2, lags=5, method="GTOS", pn=0.1, print.results="print")

dev.new()
par(mfrow=c(3,4))

for (i in Top10){
  print(summary(ur.za(Qtr[Qtr$Ticker==i, "DCBook_Q"]))@bpoint)
  print(ur.ls(Qtr[Qtr$Ticker==i, "DCBook_Q"],breaks=2)[1])

  }

