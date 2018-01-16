DCBook<-aggregate(logb(Annual_screen$DCBook), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(DCBook)<-c("year", "DCBook")
DCMarket<-aggregate(logb(Annual_screen$DCMarket), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(DCMarket)<-c("year", "DCMarket")
Fed_Rate<-aggregate((Annual_screen$Fed_Rate), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(Fed_Rate)<-c("year", "Fed_Rate")
AAA<-aggregate((Annual_screen$AAA), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(AAA)<-c("year", "AAA")
AdjMKVALT<-aggregate(logb(Annual_screen$MKVALT/Annual_screen$Capital_Market), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(AdjMKVALT)<-c("year", "AdjMKVALT")
ROE<-aggregate(logb(Annual_screen$ROE), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(ROE)<-c("year", "ROE")
ROA<-aggregate(logb(Annual_screen$ROA), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(ROA)<-c("year", "ROA")
NLA<-aggregate(logb(Annual_screen$NLA), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(NLA)<-c("year", "NLA")
NLD<-aggregate(logb(Annual_screen$NLD), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(NLD)<-c("year", "NLD")
NI<-aggregate((Annual_screen$NI), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(NI)<-c("year", "NI")
SALE<-aggregate((Annual_screen$Sales), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(SALE)<-c("year", "SALE")
EBIT<-aggregate((Annual_screen$EBIT), by=list(Annual_screen$year), med_q<-function(x){median(x, na.rm=T)});colnames(EBIT)<-c("year", "EBIT")

data<-left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(DCBook, Fed_Rate), DCMarket), AdjMKVALT), AAA), ROE), ROA), NLD), NLA)
data<-left_join(left_join(data, SALE), EBIT)

par(mfrow=c(5,2))
for (i in colnames(data[2:ncol(data)])){plot(data$year, data[,i], type="l", main=i, xlab="year", ylab=i)}

data<-data[, c("DCBook","DCMarket","AdjMKVALT","Fed_Rate", "AAA" ,"ROE", "ROA", "NLD" ,"NLA")]
VARselect(data[,1:2], lag.max=10, type="both", exogen=data[,3:9])
VARselect(data[,1:2], lag.max=7, type="both", exogen=data[,3:9])
VARselect(data[,1:2], lag.max=5, type="both", exogen=data[,3:9])
VARselect(data[,1:2], lag.max=3, type="both", exogen=data[,3:9])

model_both<-VAR(data[,1:2], p=3, type="both", exogen=data[,3:8] )
model_const<-VAR(data[,1:2], p=3, type="const", exogen=data[,3:8] )
model_trend<-VAR(data[,1:2], p=3, type="trend", exogen=data[,3:8] )

model_base<-VAR(data, p=3, type="const")

summary(model_base)
summary(model_both)
summary(model_const)
summary(model_trend)

plot(data)