# Statistical Analysis Load Package
library(e1071)  # for skewness

library(PerformanceAnalytics)  # for correlogram (chart.Correlation)

library(Hmisc)  # for missing value treatement (impute)

library(corrplot)  # for correlogram (corrplot)

library(party)  # selecting best variables (cforest)

library(Boruta)  # deciding if a variable is important (Boruta)

library(caret)  # for boxcox transformation (BoxCoxTrans)

library(car)  # for vif

library(DMwR)  # for knnImputation

library(DAAG)  # for cross validation

library(relaimpo)  # for finding relative importance of predictors in lm mod