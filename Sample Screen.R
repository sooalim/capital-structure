library(reshape)
colnames(Quarter)

ltd<-dplyr::summarise(group_by(Qtr[(!is.na(Qtr$LTDQ))&&(Qtr$LTDQ>0),c("Ticker", "Date", "MKVALTQ")], Date),n() )
dt<-dplyr::summarise(group_by(Qtr[(!is.na(Qtr$DTQ))&&(Qtr$DTQ>0),c("Ticker", "Date", "MKVALTQ")], Date), n())

aggregate(Qtr[(!is.na(Qtr$LTDQ))&&(Qtr$LTDQ>0),c("MKVALTQ")], by=list(Qtr$Date), sum.na.rm<-function(x){sum(x,na.rm="T")})


ndebt<-join(ltd, dt, by="Date") %>% View
plot(ndebt[,2], type="l")
lines(ndebt[,3], type="l", col="red")


##=====================================================================================================================

#All
#DCBV<-cast(unique(Quarter),Date~Ticker, fun.aggregate=mean, value="DCBook_Q")

#Find Active for given item
Annual$year<-year(Annual$Date)
DCBV<-cast(unique(Annual_screen[Annual_screen$Ticker %in% Active,]),year~Ticker, fun.aggregate=mean, value="DCBook")
A.DT<-cast(unique(Annual_screen[Annual_screen$Ticker %in% Active,]),year~Ticker, fun.aggregate=mean, value="DT")
A.LTD<-cast(unique(Annual_screen[Annual_screen$Ticker %in% Active,]),year~Ticker, fun.aggregate=mean, value="LTD")

check_items<-c("Sales", "EBDA", "EBIT", "NI", "AT", "ROA", "ROE", "NLA", "NLD", "MKVALT", "CEQT", "EPS", 
               "TXT" ,"TR", "DT", "DLC","LTD", "Common.Stock", "Stockholders..Equity.Parent","Capital_Market", "Capital_Book", "year", "DCBook", "DCMarket", "AAA", "BAA", "AAA_BAA", "Fed_Rate")

ind<-cbind(Annual[,c("Ticker", "year", "DT", "LTD")], check=!is.na(rowSums(Annual[,check_items])))
Annual_screen<-left_join(filter(ind, ind$check==TRUE), Annual[,c("Ticker", "Date", check_items)])


#Name rows as Date
rownames(DCBV)<-DCBV$year

#create empty table
pct<-data.table()
countrow<-data.table()
ind<-data.table()
#Calculate number of observations(count) and ratio of observation (pct) out of total period downloaded
#(we need 5 consec. observations for robust diagnostics)

for (i in colnames(DCBV)){
  pct<-rbind(pct,(NROW(DCBV[is.nan(DCBV[,i])==FALSE,i])/NROW(DCBV[,i])*100))
  countrow<-rbind(countrow,NROW(DCBV[is.nan(DCBV[,i])==FALSE&is.na(DCBV[,i])==FALSE,i]))
  #Then compute the aggregates again and unit root test => go straight to VAR and find correlation
}

countrow<-cbind(Ticker=colnames(DCBV),count=countrow)
Active_C<-subset(countrow, countrow$count>=median(unlist(countrow$count.x))&countrow$Ticker!="year"&countrow$Ticker!="Date")

rownames(countrow)<-countrow$Ticker;countrow<-t(countrow)
pct<-cbind(Ticker=colnames(DCBV),pct=pct)
rownames(pct)<-pct$Ticker;pct<-t(pct)

# Screen for tickers that require our criteria (can be more than one)
#- Over 20 observations over whole period that are not NaN)
#- 

#create table that has 


#Append them to the last rows

Screen<-rbind(DCBV[,2:ncol(DCBV)],percent=pct[2,2:ncol(pct)])
Screen<-rbind(Screen, Counts=countrow[2,2:ncol(countrow)])
#Screen[,Active_C$Ticker] %>% View
#write.csv(Screen, "./ascreen.csv")

#==========================================
  
#Find Active for given item
DCBV<-cast(unique(Quarter[Quarter$Ticker %in% Active,]),Date~Ticker, fun.aggregate=mean, value="DCBook_Q")

#Name rows as Date
rownames(DCBV)<-DCBV$Date

#create empty table
pct<-data.table()
countrow<-data.table()

#Calculate number of observations(count) and ratio of observation (pct) out of total period downloaded
#(we need 5 consec. observations for robust diagnostics)

for (i in colnames(DCBV)){
  pct<-rbind(pct,(NROW(DCBV[is.nan(DCBV[,i])==FALSE,i])/NROW(DCBV[,i])*100))
  countrow<-rbind(countrow,NROW(DCBV[is.nan(DCBV[,i])==FALSE&is.na(DCBV[,i])==FALSE,i]))
}

countrow<-cbind(Ticker=colnames(DCBV),count=countrow)
Active_CQ<-subset(countrow, countrow$count>=20&countrow$Ticker!="Date")
