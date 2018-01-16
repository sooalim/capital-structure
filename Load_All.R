  setwd("C:/Users/slim236/Desktop/TAX/Transfer Pricing/Liquidity Modeling/data/alteryx")
  source("Bond Data.R")
  library(dplyr)
  library(data.table)
  library(lubridate)
  
  
  Quarter <- read.csv("../Shared/SP_Bank_All_FS_Q.csv")
  Annual <-read.csv("../Shared/SP_Bank_All_FS_A.csv")

  Quarter.Equity <- read.csv("../alteryx/SP_Bank_Equity_Q.csv")
  
  Tickers<-sort(unique(Quarter$Ticker))
  BigBanks<-c("BAC", "C", "CMA", "JPM", "USB", "WFC")
  CommBanks<-c("BK", "NTRS", "STT")
  RegBanks<-Tickers[!Tickers %in% c(BigBanks, CommBanks)]
  Active<-RegBanks[RegBanks %in% Quarter[Quarter$Status=="Active","Ticker.Q"]]
  Dead<-RegBanks[!RegBanks %in% Quarter[Quarter$Status=="Active","Ticker.Q"]]
  
  Quarter.Equity <- read.csv("../alteryx/SP_Bank_Equity_Q.csv")
  Annual.Equity <-read.csv("../alteryx/SP_Bank_Equity_A.csv")
  Quarter.Rating <- read.csv("../alteryx/SP_Bank_Rating_Q.csv")
  Annual.Rating <-read.csv("../alteryx/SP_Bank_Rating_A.csv")
  
  Quarter$Date<-as.Date(Quarter$Date,"%m/%d/%Y")
  Quarter$Date.EOQ<-qtr2date(as.Date(Quarter$Date,"%m/%d/%Y"), "eoq")
  Quarter.Equity$Date<-as.Date(Quarter.Equity$Date,"%Y-%m-%d")
  Quarter.Rating$Date<-as.Date(Quarter.Rating$Date,"%Y-%m-%d")
  
  Annual$Date<-as.Date(Annual$Date,"%m/%d/%Y")
  Annual$Date.EOQ<-qtr2date(as.Date(Annual$Date,"%m/%d/%Y"), "eoq")
  Annual.Equity$Date<-as.Date(Annual.Equity$Date,"%Y-%m-%d")
  Annual.Rating$Date<-as.Date(Annual.Rating$Date,"%Y-%m-%d")
  Annual.Rating$DCBook<-ifelse(is.na(Annual.Rating$DT)|is.na(Annual.Rating$CEQT)|(Annual.Rating$CEQT<=0), NA, Annual.Rating$DT/(Annual.Rating$DT+Annual.Rating$CEQT))

  Quarter<-left_join(Quarter, Quarter.Equity)
  Quarter<-left_join(Quarter, Quarter.Rating[, !colnames(Quarter.Rating) %in% c("Common.Equity.Total.Qtly","DTQ")], by=c("Date", "Ticker.Q"))
  Annual<-left_join(Annual, Annual.Equity)
  Annual<-left_join(Annual, Annual.Rating[, !colnames(Annual.Rating) %in% c("CEQT","DT")], by=c("Date", "Ticker"))
  
  #correcting for DCBook
  
  Annual$DCBook<- ifelse(is.na(Annual$DCBook),ifelse((Annual$CEQT>0)&(!is.na(Annual$CEQT)), Annual$DT/(Annual$CEQT+Annual$DT), NA),Annual$DCBook)
  Annual$DCBook <-ifelse(Annual$DCBook>1, NA, Annual$DCBook)
  Annual$DCBook<-ifelse(Annual$DCBook==0|is.nan(Annual$DCBook ), NA, Annual$DCBook)
  
  
  Annual$DCMarket<- ifelse(is.na(Annual$DCMarket),ifelse((Annual$MKVALT>0)&(!is.na(Annual$MKVALT)), Annual$DT/(Annual$MKVALT+Annual$DT), NA),Annual$DCMarket)
  Annual$DCMarket <-ifelse(Annual$DCMarket>1, NA, Annual$DCMarket)
  Annual$DCMarket<-ifelse(Annual$DCMarket==0|is.nan(Annual$DCMarket ), NA, Annual$DCMarket)
   
   
  # Quarter$DCBook_Q<- ifelse(is.na(Quarter$DCBook_Q)|(Quarter$DCBook__Q>1),ifelse(Quarter$Common.Equity.Total.Qtly>0,Quarter$DTQ/(Quarter$Common.Equity.Total.Qtly+Quarter$DTQ),NA),Quarter$DCBook_Q)
  # Quarter$DCBook_Q<- ifelse(Quarter$DCBook_Q==0|is.nan(Quarter$DCBook_Q), NA, Quarter$DCBook_Q)
  # 
  # Quarter$DCMarket_Q<- ifelse(is.na(Quarter$DCMarket_Q)|(Quarter$DCMarket__Q>1),ifelse(Quarter$MKVALTQ>0,Quarter$DTQ/(Quarter$MKVALTQ+Quarter$DTQ),NA),Quarter$DCMarket_Q)
  # Quarter$DCMarket_Q<- ifelse(Quarter$DCMarket_Q==0|is.nan(Quarter$DCMarket_Q), NA, Quarter$DCMarket_Q)
  # 

  
  Index<-read.csv("../Shared/US Corporate Bond Yield_FRED.csv")
  Index$Date<-as.Date(Index$Date, "%m/%d/%Y")
  Index<-Index[, c(1:5,21:23)]
  Annual<-left_join(Annual, Index)
  Quarter<-left_join(Quarter, Index)
  
  Quarter$log.SALEQ <-ifelse(!is.na(Quarter$SALEQ), logb(Quarter$SALEQ), NA)
  Annual$log.SALE <-ifelse(!is.na(Annual$Sales), logb(Annual$Sales), NA)
  
  Quarter$log.ATQ <-ifelse(!is.na(Quarter$ATQ), logb(Quarter$ATQ), NA)
  Annual$log.AT <-ifelse(!is.na(Annual$AT), logb(Annual$AT), NA)
  
  
#  Quarter$MB<-ifelse(!is.infinite(Quarter$MKVALTQ/Quarter$CEQQ), Quarter$MKVALTQ/Quarter$CEQQ, NA)
#  Annual$MB<-ifelse(!is.infinite(Annual$MKVALT/Annual$CEQT.x), Annual$MKVALT/Annual$CEQT.x, NA)
  
  
  Annual[3:ncol(Annual)]<-na_if(Annual[3:ncol(Annual)],0)
  Quarter[3:ncol(Quarter)]<-na_if(Quarter[3:ncol(Quarter)],0)
  
  numcols<-colnames(Annual[,3:ncol(Annual)])[!colnames(Annual[,3:ncol(Annual)]) %in% c("Rating_ST_T", "Rating_LT_T")]
  numcolq<-colnames(Quarter[,3:ncol(Quarter)])[!colnames(Quarter[,3:ncol(Quarter)]) %in% c("Rating_ST_T", "Rating_LT_T")]
  
  for (i in numcols){Annual[,i]<-as.numeric(Annual[,i])}
  for (i in numcolq){Quarter[,i]<-as.numeric(Quarter[,i])}
  
  Tickers<-sort(unique(Quarter$Ticker.Q))
  colnames(Quarter)[colnames(Quarter)=="Ticker.Q"]<-"Ticker"
  dates<-sort(unique(as.Date(Quarter$Date,"%m/%d/%Y")))
  
  source("./Bond Data_Calc.R")
  Rating$Date<-as.Date(Rating$Date,"%Y-%m-%d")
  Annual<-left_join(Annual, Rating)
  Quarter<-left_join(Quarter, Rating)
  
  