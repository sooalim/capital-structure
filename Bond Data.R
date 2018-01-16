library("TTR")
library("zoo")
library("dplyr")
library("tidyr")
library("readr")
library("stringr")
library("openxlsx")
library("ggplot2")

#replaces MM or MMM to numeric (million, billion)
clean_units<-function(list, na=c("", "NA")){
  require("readr")
  list.num <- parse_number(list, na=na)
  m<-regexpr("[^0-9.]", list)
  list.char<-parse_number(paste('1', gsub("M", "000", substr(list,m,nchar(list))), sep=""), na=na)
  list.clean<-list.num*as.numeric(list.char)
  return(list.clean)
}
#wrong - substr(list, regexpr("[a-zA-Z]", list)[1], nchar(list))), sep='')
#converts XL dates to POSIXct
to.RDate<-function(list){
 return(unlist(as.POSIXct(as.numeric(list)*3600*24, origin="1899-12-30", tz="UTC")))
}

#find last day of month
to.EOM<-function(list){
  
  require(lubridate)
  day(list) <-1
  month(list) <- month(list)+1
  day(list) <- day(list) - 1
  return(list)
}  

to.ncredit_rating<-function(dataset, agency="Mdy.Init.Rtg"){ 
  
  require(dplyr)
  
  credit<-read.xlsx("../Credit_Ratings.xlsx")
  credit<-as_tibble(credit)
  colnames(credit)<-c("Mdy.Init.Rtg", "S&P.Init.Rtg", "Score_Asc", "Score_Desc")
  credit$Score_Desc<-NULL
  
  dataset[eval(agency)]<-gsub(" \\*[\\+,\\-]", "", unlist(dataset[eval(agency)]))
  dataset[eval(agency)]<-gsub("[eu]", "", unlist(dataset[eval(agency)]))  
  
  dataset<-left_join(dataset, credit, by = eval(agency))
  
  return(dataset$Score_Asc)
}

perp_filter<-function(test){
  index<-c()
  for (i in 1:length(test$Maturity.Type)){
    m<-regexpr("PERP", test$Maturity.Type[i])
    if(attr(m, "match.length")==-1){index<-append(i,index)}
  }
  test<-test[index,]
  return(test)
} 

qtr2date<-function(x,end="eoq"){
  require(lubridate)
  if(end=="eoq"){x<-lubridate::quarter(x,with_year=T)}
  as.Date(as.yearqtr(as.character(x), "%Y.%q"))+months(3)-1
}

#specifically cleans Bloomberg Bond data to R-friendly format
clean_data<-function(file, sheetNum=1, perp_off=T){
  require(openxlsx)
  
  df<-unique(read.xlsx(file, sheet=sheetNum))

  
  df$Amount.Issued <- clean_units(df$Amount.Issued)
  df$Amt.Out<-clean_units(df$Amt.Out)
  
  
  df$Issue.Date<- to.RDate(df$Issue.Date)
  df$Maturity<- to.RDate(df$Maturity)
   
  
  df$Issue.Date.EOM<-to.EOM(df$Issue.Date)
  df$Maturity.Date.EOM<-to.EOM(df$Maturity)
  df$Issue.Date.EOQ<-qtr2date(df$Issue.Date.EOM)
  df$Maturity.Date.EOQ<-qtr2date(df$Maturity.Date.EOM)
  
  
  df$Original.Matur<-as.numeric(df$Original.Matur)
  df$Original.Matur<-round(ifelse((df$Original.Matur)==0, NA,df$Original.Matur),1)
  
  df$Cpn<-as.numeric(df$Cpn)
  df$Yield.at.Issue<-as.numeric(df$Yield.at.Issue)
  df$Price.at.Issue<-as.numeric(df$Price.at.Issue)
  
  df<-cbind(df, mdy_n=to.ncredit_rating(df, "Mdy.Init.Rtg"), sp_n=to.ncredit_rating(df, "S&P.Init.Rtg"))
  df$Average_Rating = rowMeans(cbind(df$mdy_n,df$sp_n), na.rm=T)
  if(perp_off==T){df<-perp_filter(df)}
  return(df)
}

trend<-function(Bond, comp, plot=F, filter=NA, freq=1) {#Bond - table with bond issuance data, comp - Company ticker
  require(dplyr)
  
  #filter = c(NA, "Priority","type", "Collateral.Type" )
  type<-read.csv("./type.csv") 
  id<-ifelse(freq==1,"Issue.Date.EOM","Issue.Date.EOQ")
  md<-ifelse(freq==1,"Maturity.Date.EOM","Maturity.Date.EOQ")
  
  Bond<-left_join(Bond, type)
  Bond$Issue.Date.EOM<-as.Date(Bond$Issue.Date.EOM)
  Bond$Maturity.Date.EOM<-as.Date(Bond$Maturity.Date.EOM, "%Y-%m-%d")
  Bond$Issue.Date.EOQ<-as.Date(Bond$Issue.Date.EOQ)
  Bond$Maturity.Date.EOQ<-as.Date(Bond$Maturity.Date.EOQ)
  
  Bond<-filter(Bond,Ticker==comp)
  if(is.na(filter)){
    Issued<-aggregate(Bond[,"Amount.Issued"], by=list(Date=Bond[, id]), sum_q<-function(x){sum(x,na.rm=T)})
    Matured<-aggregate(Bond[,"Amount.Issued"], by=list(Date=Bond[,md]), sum_q<-function(x){sum(x,na.rm=T)})
    colnames(Issued)[2]<-c("Amount.Issued")
    colnames(Matured)[2]<-c("Amount.Issued")
    combined<-full_join(Issued, Matured, by=c("Date"))
    combined$Date <- as.Date(combined$Date)
    o<-order(combined$Date)
    combined<-data.frame(combined[o,])
    combined$Diff<-ifelse(is.na(combined[,"Amount.Issued.x"]), 0, combined[,"Amount.Issued.x"])-ifelse(is.na(combined[,"Amount.Issued.y"]), 0, combined[,"Amount.Issued.y"])
    combined$Sum<-cumsum(combined$Diff)
  }
  
  else {
    Issued<-aggregate(Bond[,"Amount.Issued"] , by=list(Date=as.Date(Bond[,id], "%Y-%m-%d") , Bond[, filter]), sum_q<-function(x){sum(x,na.rm=T)})
    Matured<-aggregate(Bond[,"Amount.Issued"], by=list(Date=as.Date(Bond[,md], "%Y-%m-%d") , Bond[,filter]), sum_q<-function(x){sum(x,na.rm=T)})
    colnames(Issued)[2:3]<-c(filter, "Amount.Issued")
    colnames(Matured)[2:3]<-c(filter, "Amount.Issued")
    combined<-full_join(Issued, Matured, by=c("Date", filter))
    
    combined$Date <- as.Date(combined$Date)
    o<-order(combined$Date)
    combined<-data.frame(combined[o,])
    
    
    Diff<-c()
    Sum<-c()
    Date<-c()
    f<-c()

    for(i in unique(combined[,filter])){
      
      temp<-combined[combined[,filter]==i,]
      temp$Diff<-ifelse(is.na(temp[,"Amount.Issued.x"]), 0, temp[,"Amount.Issued.x"])-ifelse(is.na(temp[,"Amount.Issued.y"]), 0, temp[,"Amount.Issued.y"])
      temp$Sum<-cumsum(temp$Diff)
      
      Diff<-c(Diff,unlist(temp$Diff))
      Sum<-c(Sum,unlist(temp$Sum))
      Date<-c(Date,temp$Date)
      f<-c(f,rep(i, length(temp$Date)))
      r<-data.frame(Diff, Sum, Date=as.Date(Date),f)
      colnames(r)[colnames(r)=="f"]<-filter
      
    }
    
    combined<-left_join(combined,r,by=c("Date", filter))
  }
    
  combined$Ticker<-comp

  if(!is.na(filter)){
    cname_filtered<-c("Date", filter,"Issued", "Matured", "Diff", "Sum", "Ticker")
    colnames(combined)<-cname_filtered
    o<-order(combined[,filter])
    combined<-combined[o,]
    }
  
  else{ 
    cname_nfiltered<-c("Date","Issued", "Matured", "Diff", "Sum", "Ticker")
    colnames(combined)<- cname_nfiltered
        
  }
  combined$Date.EOQ<-qtr2date(combined$Date)

  if(plot==T){
      plot(combined$Date, combined$Sum, 
           col=rgb(1, 0, 0, 0.3), 
           type="s", 
           xlim=c( today()-years(30), today()+years(10)))
    }

  return(combined)
}

read_xlsx_bond<-function(){
  file="../Bonds+Loans_v2.xlsx"
  sheetNum=5
  test<-clean_data(file, sheetNum)
  write.csv(test, "Bloomberg_BondData_test.csv")
  return(test)
}

Aggtrend<-function(Bond){
  
  Issued<-aggregate(Bond$Amount.Issued, by=list(Date=as.Date(Bond$Issue.Date.EOM, "%Y-%m-%d")), sum  )
  Matured<-aggregate(Bond$Amount.Issued, by=list(Date=as.Date(Bond$Maturity.Date.EOM ,"%Y-%m-%d")), sum  )
  print(Matured)
  require(dplyr)
  combined<-full_join(Issued, Matured, by="Date")
  combined$Date<-as.Date(combined$Date)
  combined$Date.EOQ<-qtr2date(combined$Date)
  o<-order(combined$Date)
  combined<-data.frame(combined[o,])
  combined$Diff<-ifelse(is.na(combined$x.x), 0, combined$x.x)-ifelse(is.na(combined$x.y), 0, combined$x.y)
  combined$Sum<-cumsum(combined$Diff)
  combined$Ticker<-'Market'
  colnames(combined)<-c("Date", "Issued", "Matured", "Diff", "Sum", "Ticker")
  
  return(combined)
}

Bonds<-read_xlsx_bond()

run_trend<-function(runtype="Priority"){
  rm(list)
  for (i in unique(Bonds$Ticker)){
    if (!"list" %in% ls()){list<-c()}
    
    Priority<-trend(Bonds, i, F, runtype)
    list<-rbind(list, Priority)  
    if(!is.na(runtype)){
    o<-order(list[,runtype]);list<-list[o,]}
    o<-order(list$Date);list<-list[o,]
    o<-order(list$Ticker);list<-list[o,]
  } 
  return(list)
}

