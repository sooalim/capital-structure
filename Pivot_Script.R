library(data.table)


pivot<-function(file, sheetNum=1, nameVar){
  MV<-read.xlsx(file, sheetNum)
  
  colnames(MV)<-MV[1,]
  MV<-MV[2:nrow(MV),]
  
  MV.melt<-melt(MV, id.var=c("Ticker Symbol"),measure.vars=colnames(MV)[2:ncol(MV)] )
  print(head(MV.melt))
  MV.melt$`Ticker Symbol`<-as.POSIXct(as.numeric(MV.melt$`Ticker Symbol`)*3600*24, tz="UTC", origin = "1899-12-30 00:00")
  colnames(MV.melt)<-c("Date","Ticker", nameVar)
  return(MV.melt)
}

MKVAL<-pivot("MarketValue.xlsx", 1, "MKVAL")
CP<-pivot("MarketValue.xlsx", 2, "ClosePrice")

#join data set and write to csv
Market<-left_join(MKVAL, CP)
Market$Date<-format(Market$Date, "%Y/%m/%d")
write.csv(Market,"Market_price.csv", na="", row.names=F)