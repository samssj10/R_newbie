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

#convert string factors into numeric factors
for(i in 1:ncol(marketing_train))
{
  if(class(marketing_train[,i])=="factor")
  {
    marketing_train[,i]=factor(marketing_train[,i],labels=factor(1:length(levels(factor(marketing_train[,i])))))
  }
}

##outlier analysis
numeric_index=sapply(marketing_train,is.numeric) #selects the numeric variables
numeric_data=marketing_train[,numeric_index]
cnames=colnames(numeric_data)
for (i in 1:length(cnames))
   {
     assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "responded"), data = subset(marketing_train))+ 
              stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(y=cnames[i],x="responded")+
              ggtitle(paste("Box plot of responded for",cnames[i])))
   }
# paste and paste 0 used to concatenate strings
# paste (…, sep = " ", collapse = NULL)
# paste0(…, collapse = NULL)
# paste0 is same as paste(…, sep = "", collapse = NULL)
# labs (Modify axis, legend, and plot labels)

##plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)

#remove outliers using boxplot method
df=marketing_train
val=marketing_train$previous[marketing_train$previous %in% boxplot.stats(marketing_train$previous)$out] #%in% is a search operator that searches for values in rhs in lhs
marketing_train=marketing_train[which(!marketing_train$previous %in% val),] #removes the outlier values from marketing_train$previous
#for every pattern searched using a vector using %in% that pattern is removed from the vector
for(i in cnames)
{
  val=marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
  marketing_train=marketing_train[which(!marketing_train[,i] %in% val),]
}

#replace all outliers with NA
for(i in cnames)
{
  val=marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
  print(length(val))
  marketing_train[,i][marketing_train[,i] %in% val]=NA
}

head(knnImputation(marketing_train,k=3)) #does not work due to even no of obs
sum(complete.cases(marketing_train))

##correlation plot (works on numeric variables)
corrgram(marketing_train[,numeric_index],order = F,upper.panel=panel.pie,text.panel = panel.txt,main = "correlation plot")
#we will reject those variables which are having high negative or positive correlation with multiple variables

## chi squared test of independence (works on factor variables which are of numeric type ie, should not be string facors)
factor_index=sapply(marketing_train,is.factor)
factor_data=marketing_train[,factor_index]

for(i in 1:10)
{
  print(names(factor_data[i]))
  print(chisq.test(table(factor_data$responded,factor_data[,i])))
}

##dimension reduction
marketing_train_deleted=subset(marketing_train,select =-c(pdays,emp.var.rate,housing,loan,day_of_week))

