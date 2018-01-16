  setwd("C:/Users/slim236/Desktop/TAX/Transfer Pricing/Liquidity Modeling/data/alteryx")
  
  library(data.table)
#  source("Load_All.R")
  library(lubridate)
  
    
  Bonds<-read.csv("../Shared/Bloomberg_BondData_xPerpetuals.csv")
  o<-order(as.Date(Bonds[,"Issue.Date"], "%m/%d/%Y"))
  Bonds<-Bonds[o,]
  Bonds$Issue.Date.EOM<-as.Date(Bonds[,"Issue.Date.EOM"], "%m/%d/%Y")
  
  
  
  Credit_MA<-function(comp='PNC', yr=5, interpolate=F) {
  BTicker<-unique(Bonds$Ticker)
    
  # Calculate moving average of credit ratings for individual companies
  comp=comp
  
  # Temporary table containing transations with only valid ratings
  temp<-filter(Bonds, Ticker==comp & !is.na(Average_Rating))
  temp[,"Issue.Date"]<-as.Date(temp[,"Issue.Date"], "%m/%d/%Y")
  if(NROW(temp)>0){
  ## Plot trend of Ratings at Point of time (Issue Date)
  # plot(as.Date(temp[,"Issue.Date.EOM"], "%m/%d/%Y"), temp[, "Average_Rating"], 'b')
  
  # Create new table with (1) Issue Date, 
  # (2) Date from which moving average should be calculated from - indicated by parameter 'yr'
  # (3) Calculated moving average of rating
  
      
  wma<-data.frame(Issue.Date=temp[,"Issue.Date"],
       Issue.Date.5=temp[,"Issue.Date"]+years(-1*yr), 
       diff=c(NA, diff(temp[,"Issue.Date"],lag=1)), Score = temp[, "Average_Rating"])
  
  #initiate new list  
  wMRating<-rep(0, nrow(wma))
  wMCount<-wMRating
  wMSum<-wMCount
  
  for (i in 1:nrow(wma)){
    
    #Credit Score weigted by time period
    wMSum[i]<-sum(wma$Score*pmin(wma$Issue.Date-wma$Issue.Date.5[i],wma$diff, na.rm=T)*
                    ifelse(wma$Issue.Date>wma$Issue.Date.5[i]&wma$Issue.Date<=wma$Issue.Date[i], 1, 0),na.rm=T)
    
    wMCount[i]<-sum(pmin(wma$Issue.Date-wma$Issue.Date.5[i],wma$diff, na.rm=T)*
                      ifelse(wma$Issue.Date>wma$Issue.Date.5[i]&wma$Issue.Date<=wma$Issue.Date[i], 1, 0), na.rm=T)
      }
  
  wma$wMRating<-wMSum/wMCount

  #plot(as.Date(wma$Issue.Date,"%m/%d/%Y"),wma$wMRating, type='b', col="black", lty=2, main=comp, xlab='Date', ylab='Rating(numeric) - Higher score indicates lower quality')
 
  if(interpolate == T){
      x<-sort(unique(Quarter$Date)) #new dates for interpolation
      if(nrow(wma[!is.na(wma$wMRating),])>1){
      inter<-approx(wma$Issue.Date, wma$wMRating, x, rule=1:2, ties="ordered")
      inter$Ticker<-comp
      inter<-data.frame(inter)
      colnames(inter)<-c("Date", "Revolving_Rating", "Ticker")
      }
      else {inter<-c()}
      #plot(inter$x, inter$y)
      #plot(as.Date(wma$Issue.Date, "%m/%d/%Y"), wma$wMRating, type='b',lty=2, xlim=c(min(x), max(x)))
      return(inter)
      }
  return(data.frame(wma))
  }  else {return(c())}
  }
  
  



#====================================================================================================#
Rating<-Credit_MA("PNC", 5,interpolate =T)

for (i in unique(Bonds$Ticker)[2:NROW(unique(Bonds$Ticker))]){
  Rating<-rbind(Rating, Credit_MA(i, yr=5, interpolate = T))
}
  
# PNCMA<-Credit_MA('PNC', yr=5, interpolate=T)
#  View(PNCMA)
  
  
# par(mfrow=c(5,1))
# yq<-DCratioQ("PNC")
# Credit_MA('PNC', 5)
# Credit_MA('BBT', 5)
# Credit_MA('KEY', 5)

# tally 
# Bonds %>% group_by(Ticker) %>% summarise(n = n()) %>% View

#for (i in 1:10){par(new=T);Credit_MA('BK', i)}

