rm(list=ls())
getwd()

#loops

##for loop
player1=c(1,3,5,7,9)
player1
for(i in player1)
{
  print(i)
  print(i+1)
}

##if inside for
data = mtcars
data$newcol=with(data,0)
for(i in 1:nrow(data))
{
  if(data$cyl[i]==6)
  {
    data$newcol[i]=1
  }
  else
  {
    data$newcol[i]=0
  }
}

#ifelse condition
data$some_obs=ifelse(data$gear==4,1,0) 

#apply function (better than loops in terms of time complexity)
apply(data[1:10,2:10],2,mean) # 1= row level op , 2= col level op
apply(data[1:10,2:10],1,sd)
#apply fn can return list,array,vector,dataframe etc..

rnorm(30,0) #generates random nos from the normal dist with provided mean and variance

#create your function
m=matrix(data=cbind(rnorm(30,0),rnorm(30,2),rnorm(30,5)),nrow=30,ncol=3)
apply(m,2,function(x) length(x[x<0]))

#lapply returns list
data$car_names=NULL
lapply(data[,1:10],sd)
lapply(data[1:10,],mean) #lapply iterates over column ONLY

#sapply returns vector
temp=c(10,20,30,40,50)
sapply(temp,function(x){x*x})
lapply(temp,function(x){x*x})
unlist(lapply(temp,function(x){x*x})) #same as sapply


