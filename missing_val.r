setwd("~/r_works")
getwd()

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
install.packages('abind')
install.packages('zoo')
install.packages('xts')
install.packages('quantmod')
install.packages('ROCR')
install.packages("DMwR")
#install packages(x)
lapply(x, require, character.only = TRUE)
rm(market_data)

#read the data
library(xlsx)
marketing_train=read.csv("market_data.csv",header=T,na.strings=c(" ","","NA"))
names(market_train)

str(marketing_train)

marketing_train$schooling[marketing_train$schooling %in% "illiterate"] = "unknown"
marketing_train$schooling[marketing_train$schooling %in% c("basic.4y","basic.6y","basic.9y","high.school","professional.course")] = "high.school"
marketing_train$default[marketing_train$default %in% "yes"] = "unknown"
marketing_train$default = as.factor(as.character(marketing_train$default))
marketing_train$marital[marketing_train$marital %in% "unknown"] = "married"
marketing_train$marital = as.factor(as.character(marketing_train$marital))
marketing_train$month[marketing_train$month %in% c("sep","oct","mar","dec")] = "dec"
marketing_train$month[marketing_train$month %in% c("aug","jul","jun","may","nov")] = "jun"
marketing_train$month = as.factor(as.character(marketing_train$month))
marketing_train$loan[marketing_train$loan %in% "unknown"] = "no"
marketing_train$loan = as.factor(as.character(marketing_train$loan))
marketing_train$schooling = as.factor(as.character(marketing_train$schooling))
marketing_train$profession[marketing_train$profession %in% c("management","unknown","unemployed","admin.")] = "admin."
marketing_train$profession[marketing_train$profession %in% c("blue-collar","housemaid","services","self-employed","entrepreneur","technician")] = "blue-collar"
marketing_train$profession = as.factor(as.character(marketing_train$profession))

##missing value analysis
missing_val=data.frame(apply(marketing_train,2,function(x) {sum(is.na(x))}))
names(missing_val)[1]="missing_per"
missing_val$columns=row.names(missing_val)
row.names(missing_val)=NULL
missing_val$missing_per=(missing_val$missing_per/nrow(marketing_train))*100
missing_val$missing_per=missing_val[order(-missing_val$missing_per),]

#actual value marketing_train[71,1]=29
#mean = 40.01
#median = 38
#KNN = 36.18 (this is the closest to actual value so we use this for calculating all missing values in the dataset)

marketing_train$custAge[is.na(marketing_train$custAge)]=mean(marketing_train$custAge,na.rm=T)
marketing_train$custAge[is.na(marketing_train$custAge)]=median(marketing_train$custAge,na.rm=T)
marketing_train=knnImputation(marketing_train,k=5)


