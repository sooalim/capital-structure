setwd("C:/Users/slim236/Desktop/TAX/Transfer Pricing/Liquidity Modeling/data/alteryx")

library(vars)
library(TSA)
library(urca)

#Loading Financial Data and cleansing
source("Load_All.R")

#Sum of issued data collected from Bloomberg
source("Bond_Data_plot.R")

#QuarterAgg
source("OverallView.R")

source("Sample Screen.R")