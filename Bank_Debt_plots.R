BAC<-get_ticker(Quarter, "BAC")
m<-lm(BAC$DTQ/(BAC$DTQ+BAC$MKVALTQ)*100~BAC$SPSIRBK+BAC$AAA+BAC$Date)
plm<-predict.lm(m, data.frame((AAA=BAC$BAA)), se.fit=T, interval = "prediction", level=.99)
all<-(cbind(plm$fit, se.fit=plm$se.fit, df=plm$df, residual.scale=plm$residual.scale, BAA=BAC$BAA))
all<-data.frame(all)
o<-order(all$BAA)
all<-all[o,]

  plot(all$BAA, all$fit, col='red')
  points((BAC$BAA), plm$fit[,"lwr"], col='blue')
  points((BAC$BAA), plm$fit[,"upr"], col='green')
  points((BAC$AAA), m$fitted.values, col='purple')
  
  par(mfrow=c(2,2))
  plot(all$BAA,all$se.fit)
  plot(all$BAA,all$fit, 'p')
  plot(m$model$`BAC$AAA`,m$model$`BAC$DTQ/(BAC$DTQ + BAC$MKVALTQ)`)
  plot(m$model$`BAC$SPSIRBK` ,m$model$`BAC$DTQ/(BAC$DTQ + BAC$MKVALTQ)`)
  abline(m)
  summary(m)
  
  dev.new();par(mfrow=c(2,2));plot(m)