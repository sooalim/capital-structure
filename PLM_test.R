#run load_all
#run Bond Data / Bond Data_calc / Bond Data_plot
setwd("C:/Users/slim236/Desktop/TAX/Transfer Pricing/Liquidity Modeling/data/alteryx")
source("setup.R")

# Assuming Debt Ratio follows a VAR(1) model
# D(t) = c +D(t-1) + D(t-p) + Yield(t) + e(t)  across panel

# Panel assumption:

## CASE 1: ALL SLOPES ARE COMPLETELY UNRELATED
# Mean Group: 
# - MG estimator will produce consistent estimates of the average of the parameters
# - All parameters/intercepts/short-run coefficients/long-run coefficients/error variances are allowed to differ across groups

## CASE 2: ALL SLOPES ARE IDENTICAL
# Pooled estimators:
# - Intercepts are allowed to differ across groups, other parameters are constrained to be the same
# - can lead to misleading estimates of average values of parameters in dynamic panel data
# unless all slope coefficients are in fact identical ~ usually this is not the case

# Fixed effect: individual specific effects is correlated with indepedent variables
# Random effect:individual specific effects are uncorrelated with independent variables

## CASE 3: Pooled Mean Group : Pooled + Mean Group
# - Allows for Intercept, short-run coefficients, error variances to be different but long-run coefficients to be the same
# - i.e. speed of adjustments or convergence to the steady state can be different but in the long-run, function parameters would be the same


# Applying PGM estimator for equilibrium modeling
# assuming stationary regressors (no unit root processes)

# DCBook_Q(t) = c + Yield + Rating_LT + Fed Rate + CDLQ/DTQ + EBITQ/SALEQ + ln(Asset) + NLDQ
#               +e(t)  across panel 

# Balanced Dataset

library(plm)
Qtr<-unique(Qtr) #investigate later
Qtr<-Qtr[Qtr$Ticker %in% Active,]
Qtr$r1<-Qtr$CLDQ/Qtr$DTQ
Qtr$r2<-Qtr$EBITQ/Qtr$SALEQ
lm1<-plm(Qtr$DCBook_Q~Qtr$Revolving_Rating+Qtr$NLDQ + Qtr$AAA, 
         data=Qtr, index=c("Ticker", "Date.EOQ"), model="within", effect="individual")

lm1<-plm(Qtr$DCBook_Q~Qtr$Rating_LT+ Qtr$AAA ,
         data=Qtr, index=c("Ticker", "Date.EOQ"), model="within", effect="individual")

lm1<-pmg(Qtr$DCBook_Q~Qtr$Rating_LT+ Qtr$AAA ,
         data=Qtr, index=c("Ticker", "Date.EOQ"), model="cmg")


summary(lm1)
library(foreign)


