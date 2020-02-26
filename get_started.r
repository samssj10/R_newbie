#clear RAM contents or basically your environment
rm(list=ls()) 

#set working directory 
setwd("~/r_works")

#current working directory
getwd()

#installing packages
install.packages("dmm")

#install mutiple packages at a time
install.packages(c(""))

##load data in R
#reading csv
df = read.csv("filename.csv", header = T) #header means columns

#reading excel sheet 
install.packages("xlsx") #issues with installing on ubuntu 18.04 can be resolved by this https://askubuntu.com/questions/1168264/problem-installing-rjava-with-r-3-6-on-ubuntu-18-04
library(xlsx)
data_excel = read.xlsx("filename.xlsx" , sheetIndex = 1, header = T) #excel can have multile sheets in 1 workbook so specify that using sheetIndex

#eading .txt file as dataframe
data_text = read.delim("filename.txt", sep="\t", header = T)#in case there is a header and still we used F value then the header is counted as one observation

#consider mtcars dataset to understand R commands
data = mtcars

#getting column names of the dataset
colnames(data)

#getting structure of dataset
str(data)

#getting no of obs and var in the dataset
dim(data)

#getting first 10 rows of dataset 
head(data,10) #by default takes 6 as second arg

#getting last 10 rows of dataset
tail(data,10)

#getting first 5 obs with 6 variables 
data[1:5,1:6]

#getting first 6 columns of dataset 
data[ ,1:6]

#data type of a particular var
class(data$cyl) #data$cyl can be used explicitly to view contents of the selected variable

#unique values in a variable
unique(data$gear)

#length of unique values in a selected variable
length(unique(data$gear))

#dist of unique val in a col and their frequencies
table(data$gear)

#summary of a var
summary(data$cyl)

#assignment operator
df=30
df1="i want to be a data scientist"

#logical operation
df==20
df==30

#arithmetic operations
#add
2 + 3
#sub
2-3
#mul
3*5
#div
7/2
#rem
7%%2
#power
7^2
4^3
#mean
mean(data$mpg)
#median
median(data$mpg)
#standard deviation
sd(data$mpg)
#variance
var(data$mpg)
#add all val of col
sum(data$mpg)

#writing a csv file
write.csv(data,"try.csv",row.names = F) #row.names=F means i will not be storing the index of obs as it is not part of my original data

#writing a xlsx file
write.xlsx(data,"try1.xlsx",row.names = F)

rm(list=ls())
setwd("/home/samssj10/r_works")
getwd()

#create vector
vec_v1=c(2,3,4,6)

#create list 
ls=list("data",5,7,9,"science") #can hold heterogenous data

#create matrix
sample_matrix=matrix(1:9,byrow=T,nrow=3) #matrix(elements,filling order,no of rows)
sample_matrix=matrix(1:9,byrow=F,nrow=3)
sample_matrix=matrix(1:9,byrow=T,nrow=4) #does not work as no of rows should neccesarily be divivsble by no of elements
sample_matrix=matrix(1:9,byrow=T,ncol=3)
sample_matrix=matrix(1:9,byrow=T,ncol=9)

#naming a matrix
colnames(sample_matrix)=c("red","blue","green")

#transpose matrix
t(sample_matrix)

#arithmetic with matrices
sample_matrix*5
sample_matrix/10

#create data frame
df=data.frame(age=c(2,3,4,5),gender=c('m','f','f','m'),income=c(1,2,3,4))
df

#rename variable
names(df)[2]="gen"
df
vec_v1
#convert vector to dataframe
df_vec=as.data.frame(vec_v1)
df_vec

#selecting variables from a dataset and creating a new dataset
data=mtcars
newdata_v1=subset(data,select=c("mpg","cyl","hp","am"))
newdata_v1

#selecting the rows wih condition
row_data=data[which(data$mpg==21.4),]#extracts all column values for which a row has mpg==21.4
row_data

#creating new variable in a dataset
data$Logmpg=with(data,log(mpg))#LHS - dest dataset , RHS - source dataset

#merging 2 dataframes
d1=data.frame(a=c(2,3,4,5),b=c(6,7,8,9))
d2=data.frame(a=c(6,7,8,9),b=c(2,3,4,5))
##merging by row
d3=rbind(d1,d2)
##merging by column
d4=cbind(d1,d2)

#convert each variable 
data$cyl=as.factor(data$cyl)
data$vs=as.factor(data$vs)
str(data)

#convert n variables at a time
  for(i in c(2,8:11))
  {
    data[ ,i]=as.factor(data[ ,i])
  }
0101 || 1010
0101 | 1010
#Operators & and | perform element-wise operation producing result having length of the longer operand.
#But && and || examines only the first element of the operands resulting into a single length logical vector.
#Zero is considered FALSE and non-zero numbers are taken as TRUE.

#convert numeric to categorical 
data$mpgcat[data$mpg>=10 & data$mpg<=16]="low" #compares each val>=10 with each val<=16
data$mpgcat[data$mpg>16 & data$mpg<=20]="medium"
data$mpgcat[data$mpg>20]="high"

#convert categoric to numeric
voice = c(rep("medium",30),rep("poor",30),rep("rich",40))
str(voice)
voice_factor=factor(voice,labels=(1:length(levels(factor(voice))))) #in case of as.factor only the data type changes which can be seen in structure but here actual data gets affected.
str(voice_factor)  
voice_numeric=as.numeric(voice_factor) #voice cannot be used here as it is holding characters
str(voice_numeric)

#sorting in ascending order
##note : descending order sorting can be done only for numeric values and not on factor type
data=data[order(data$cyl),]

#descending order
data = data[order(-data$mpg),]
data = data[order(-data$mpg,-data$gear),] #if 2 obs have same mpg then gear val is compared

#merging 2 dataframes
df1=data.frame(cust_id=c(1:6),product=c(rep("toaster",3),rep("radio",3)))
df2=data.frame(cust_id=c(2,4,6),state=c(rep("delhi",2),"pune"))
merged_data=merge(df1,df2,by="cust_id") #by parameter contains name of common variable

#join
left_join=merge(df1,df2,by="cust_id",all.x=TRUE) #merge + keep all obs from df1
right_join=merge(df1,df2,by="cust_id",all.y=TRUE)#merge + keep all obs from df2
cross_join=merge(df1,df2,by=NULL)# all variales are considered unique

#aggregating (gives a summary table wrt a reference variable)
data=mtcars
list(data$gear)
aggregate(data,by=list(data$gear),median)
aggregate(data,by=list(data$cyl,data$vs),mean)
aggregate(data,by=list(data$cyl),sd)

#gives obs wrt a specific value
grep(6,data$cyl)
grep(14.3,data$mpg)

#replacing one/multiple characters with other
text="i/need/to/do/this"
gsub("/"," ",text)
x=c("this is a sentence about axis","a second pattern is also listed here")
x
sub("is","XY",x)#only first occurrence
gsub("is","XY",x)"all occurrence"

#convert row.names into a column
rownames(data)
data=cbind(rownames(data),data)
rownames(data)=NULL
data$gear=NULL
colnames(data)[1]="car_names" #use rownames and colnames when u have specific names for both rows and columns otherwise only names is suffcient
getwd()
write.csv(data,"data_transform.csv",row.names=F)
