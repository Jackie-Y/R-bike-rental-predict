### STAT 425 Project
### Jiaqi Yao

options(warns=-1)
library(lubridate)
library(scales)
library(leaps)
library(car)
library(MASS)
library(dplyr)
library(ggplot2)
library(randomForest)
library(forecast)

## DATA PREPARATION
train = read.csv("C:/Users/Jackie/Documents/1A-STAT 425/Project/Original Data Set/train.csv")
test = read.csv("C:/Users/Jackie/Documents/1A-STAT 425/Project/Original Data Set/test.csv")

# add year,month,hour as new variables
train$year  = year(train$datetime) 
train$month  = month(train$datetime)
# train$date = date(train$datetime)
train$hour  = hour(train$datetime)

# add weekday as new variable
train$wkday = wday(train$datetime, label=TRUE) # add weekday as variable
train$wkday = ifelse(wday(train$datetime)==1, 7, wday(train$datetime)-1)
train$wkday = as.factor(train$wkday)
levels(train$wkday)=c("Mon", "Tu", "Wed", "Th", "Fri", "Sat", "Sun")

#########################  Exploratory Data Analysis ###############################

boxplot(train$count~as.factor(train$hour),main="Figure 0-1: Boxplot of Hour",xlab="Hour",ylab="Count")
abline(h=mean(train$count),lty=3,lwd=2)

boxplot(train$count~as.factor(train$year),main="Figure 0-4: Boxplot of Month",xlab="Year",ylab="Count")

boxplot(train$count~as.factor(train$month),main="Figure 0-2: Boxplot of Month",xlab="Month",ylab="Count")

# not apparant on difference
boxplot(train$count~as.factor(train$season))
boxplot(train$count~as.factor(train$wkday),main="Boxplot of Week Day",xlab="Weekday",ylab="Count")
boxplot(train$count~as.factor(train$workingday))
boxplot(train$count~as.factor(train$holiday))

table(train$weather);table(test$weather)
par(mfrow=c(1,1))
boxplot(train$count~as.factor(train$weather),main="Figure 0-3(1): Boxplot of Weather",xlab="Weather",ylab="Count")
# only 1 observation, group it to 3
train$weather[which(train$weather==4)]=3
boxplot(train$count~as.factor(train$weather),main="Figure 0-3(2): Boxplot of Weather",xlab="Weather",ylab="Count")

### Quantative Variables
par(mfrow=c(1,1))
scatter.smooth(log(train$count+1)~train$atemp,main="Figure 0-4: logged count~atemp",xlab="atemp",ylab="logged count")
scatter.smooth(log(train$count+1)~train$humidity,main="Figure 0-5: logged count~humidity",xlab="humidity",ylab="logged count")
scatter.smooth(log(train$count+1)~train$windspeed,main="Figure 0-6: logged count~windspeed",xlab="windspeed",ylab="logged count")
# seems like higher order term could be added in

# correlation
pairs(~temp+atemp+humidity+windspeed,data=train)

# Transformation on numeric variables
summary(powerTransform(cbind(atemp,humidity+1,windspeed+1)~1,train))

## DATA MODIFICATION ON TRAIN AND SET
train = read.csv("C:/Users/Jackie/Documents/1A-STAT 425/Project/Original Data Set/train.csv")
test = read.csv("C:/Users/Jackie/Documents/1A-STAT 425/Project/Original Data Set/test.csv")

DataModification=function(dat){
  # add year,month,hour as new variables
  dat$year  = as.factor(year(dat$datetime))
  dat$month = as.factor(month(dat$datetime))
  dat$hour  = as.factor(hour(dat$datetime))
  
  # add weekday as variable
  dat$wkday = wday(dat$datetime, label=TRUE) 
  dat$wkday = ifelse(wday(dat$datetime)==1, 7, wday(dat$datetime)-1)
  dat$wkday = as.factor(dat$wkday)
  levels(dat$wkday)=c("Mon", "Tu", "Wed", "Th", "Fri", "Sat", "Sun")
  
  # define variable type and levels
  dat$weather[which(dat$weather==4)]=3
  dat$weather=as.factor(dat$weather)
  levels(dat$weather)=c("clear","mist","rain")
  
  dat$workingday=as.factor(dat$workingday)
  levels(dat$workingday)=c("nonwork","work")
  
  dat$season=as.factor(dat$season)
  dat$holiday=as.factor(dat$holiday)
  dat$windspeed=(dat$windspeed)^0.68 # transform windspeed to approximate normal
  
  return (dat)
}

Newtrain=DataModification(train)
Newtrain$lcount = log(train$count +1)
Newtest=DataModification(test)

#############################  SIMPLE MODEL  #################################

## same code with provided, I think it's pretty simple and clear
submission1 = data.frame(datetime=test$datetime,hour=Newtest$hour,wkday=Newtest$wkday)

for (i_year in unique(Newtest$year)){
  for (i_month in unique(Newtest$month)) {
    trainSubset = filter(Newtrain, year==i_year, month==i_month)
    by_wkday_hour = group_by(trainSubset, wkday, hour)
    wkday_hour_lcounts = summarise(by_wkday_hour, lcounts=mean(lcount))    
    testLocs = Newtest$year ==i_year & Newtest$month == i_month
    tmp = submission1[testLocs, ]
    tmp = inner_join(tmp, wkday_hour_lcounts)
    submission1[testLocs, "count"] = exp(tmp$lcounts)-1
  }
}
submission1=submission1[, c(1, 4)]
write.csv(submission1, file = "Submission1.csv", row.names=FALSE)


#########################  SIMPLE LINEAR MODEL  ################################

##### PART ONE: Linear Regression on ENTIRE DATASET
set.seed(7)
rowsample=sample(1:dim(Newtrain)[1],2000,replace=F)
randomset=Newtrain[rowsample,]

pairs(~lcount+year+season+month+workingday+wkday+hour+
        weather+temp+atemp+humidity+windspeed,data=randomset)

pairs(~season+month+temp+atemp,data=randomset)
## season and month correlated, temp and atemp correlated, thus drop season and temp

mfull=lm(lcount~ year+month+workingday+wkday+hour+
           weather+atemp+humidity+windspeed,data=randomset)
anova(mfull)

step(mfull,direction="both",trace=F)$call
step(mfull,direction="backward",trace=F)$call

m3=lm(lcount ~ year + month + wkday + hour + weather + 
        atemp + humidity + windspeed, data = randomset)
summary(m3)

par(mfrow=c(2,2))
plot(m3)# problems with constant variance

## Quadratic Terms?
pairs(~lcount+atemp+humidity+windspeed,data=randomset)
# seems like higher order term could be added in

m4=lm(lcount~year+month+wkday+hour+weather+
        poly(atemp,3)+poly(humidity,4)+poly(windspeed,2),data=randomset)

summary(m4)
par(mfrow=c(2,2))
plot(m4)
anova(m3,m4)

## polynomial term does improve performance

# interaction?
attach(randomset)
xreg=data.frame(year,month, wkday,hour,weather,
                atemp,humidity,windspeed,deparse.level=2)
xreg[,1]=as.factor(xreg[,1]);xreg[,2]=as.factor(xreg[,2])
xreg[,3]=as.factor(xreg[,3]);xreg[,4]=as.factor(xreg[,4])
xreg[,5]=as.factor(xreg[,5])


for(i in 1:7){
  for(j in (i+1):8){
    temp=update(m4,~ .+xreg[,i]:xreg[,j] )
    result=anova(temp)$Pr[9]
    if(result<0.05){
      print( c(paste0( c(colnames(xreg)[i],colnames(xreg)[j])), result) ,quote=F)
      print(" ",quote=F)
    }
  }
}
# First include all the significant interations tested before
m5=update(m4,~.+year:month+year:humidity+ month:weather+month:atemp+ 
            wkday:hour+wkday:atemp+ wkday:humidity+wkday:windspeed+ 
            hour:weather+ hour:humidity+ weather:atemp+humidity:windspeed)

# Using AIC to find the reduce model with interaction
step(m5,direction="backward",trace=F)$call

# Here is the final Model
m6=lm(lcount ~ year + month + wkday + hour + weather + 
        poly(atemp, 3) + poly(humidity, 4) + poly(windspeed, 2) + 
        year:month + year:humidity + month:weather + wkday:hour + 
        wkday:atemp + wkday:humidity + hour:humidity + humidity:windspeed, 
      data = randomset)
summary(m6)
anova(m6)
par(mfrow=c(1,1))
plot(m6)
 
##### PART 2: Linear Regression on YEAR 2011
Newtrain_2011=Newtrain[which(Newtrain$year=="2011"),]
rowsample=sample(1:dim(Newtrain_2011)[1],1000,replace=F)
randomset=Newtrain_2011[rowsample,]

mfull=lm(lcount~ month+workingday+wkday+hour+
           weather+atemp+humidity+windspeed,data=randomset)
step(mfull,direction="backward",trace=F)$call

m3=lm(lcount ~ month + workingday + hour + weather + atemp + 
        humidity + windspeed, data = randomset)

## Quadratic Terms?
m4=lm(lcount~month+workingday+hour+weather+
        poly(atemp,3)+poly(humidity,2)+poly(windspeed,1),data=randomset)

# interaction?
attach(randomset)
xreg=data.frame(month, workingday,hour,weather,
                atemp,humidity,windspeed,deparse.level=2)
xreg[,1]=as.factor(xreg[,1]);xreg[,2]=as.factor(xreg[,2])
xreg[,3]=as.factor(xreg[,3]);xreg[,4]=as.factor(xreg[,4])

for(i in 1:6){
  for(j in (i+1):7){
    temp=update(m4,~ .+xreg[,i]:xreg[,j] )
    result=anova(temp)$Pr[8]
    if(result<0.1){
      print( paste0( c(colnames(xreg)[i],colnames(xreg)[j])) ,quote=F)
      print(result)
      print(" ",quote=F)
    }
  }
}
# First include all the significant interations tested before
m5=update(m4,~.+month:workingday+month:humidity+workingday:hour+workingday:atemp+
            workingday:humidity+hour:atemp+hour:windspeed+weather:atemp)
# Using AIC to find the reduce model with interaction
anova(m5)
step(m5,direction="backward",trace=F)$call

# Here is the final Model
m6=lm(lcount ~ month + workingday + hour + weather + poly(atemp,3) + 
        poly(humidity, 2) + month:workingday + month:humidity + 
        workingday:hour + hour:atemp, data = randomset)
anova(m6)
summary(m6)
par(mfrow=c(2,2))
plot(m6)

#### PART 3: Linear Model for 1st Month
Newtrain_mon=Newtrain[which(Newtrain$year==2011 & Newtrain$month==1),]
mfull=lm(lcount~ workingday+wkday+hour+
           weather+atemp+humidity+windspeed,data=Newtrain_mon)
step(mfull,direction="both",trace=F)$call
m3=lm(lcount ~ workingday + hour + weather + atemp + humidity + 
        windspeed, data = Newtrain_mon)

# interaction?
attach(Newtrain_mon)
xreg=data.frame(workingday,hour,weather,
                atemp,humidity,windspeed,deparse.level=2)
xreg[,1]=as.factor(xreg[,1]);xreg[,2]=as.factor(xreg[,2])
xreg[,3]=as.factor(xreg[,3])

for(i in 1:5){
  for(j in (i+1):6){
    temp=update(m3,~ .+xreg[,i]:xreg[,j] )
    result=anova(temp)$Pr[7]
    if(result<0.05){
      print( paste0( c(colnames(xreg)[i],colnames(xreg)[j])) ,quote=F)
      print(result)
      print(" ",quote=F)
    }
  }
}
# First include all the significant interations tested before
m5=update(m3,~.+workingday:hour+hour:atemp+weather:windspeed)
# Using AIC to find the reduce model with interaction
step(m5,direction="backward",trace=F)$call
# Here is the final Model
m6=lm(lcount ~ workingday + hour + weather + atemp + humidity + 
        windspeed + workingday:hour + hour:atemp + weather:windspeed, 
      data = Newtrain_mon)

####  (Actual code for submission) ### Simply Linear Regression #####
submission2 = data.frame(datetime=test$datetime,count=NA)

for (i_year in unique(year(ymd_hms(test$datetime)))) {
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs = year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    trainLocs = ymd_hms(train$datetime) <= min(ymd_hms(test$datetime[testLocs]))
    
    # main problem with level deficiency would be year and month when prediction warming up
    # Thus I calculate the real level in the train and test subset, to decide whether varaible need to be dropped
    yr_train= sum( ifelse(table(Newtrain[trainLocs,]$year)!=0,1,0) )
    yr_test= sum( ifelse(table(Newtest[testLocs,]$year)!=0,1,0) )
    mon_train= sum( ifelse(table(Newtrain[trainLocs,]$month)!=0,1,0) )
    mon_test= sum( ifelse(table(Newtest[testLocs,]$month)!=0,1,0) )
    
    # when real level in the subset smaller than 2, or train level smaller than test level(not very likely tough), 
    # variable and it's cross term need to be dropped
    yr_exclude=yr_train<2 || yr_train<yr_test
    mon_exclude=mon_train<2 || mon_train< mon_test
    
    if(yr_exclude && mon_exclude)
    {
      myfit = lm(lcount ~ workingday + hour + weather + atemp + humidity + 
                   windspeed + workingday:hour + hour:atemp + weather:windspeed, 
                 data=Newtrain[trainLocs,])
    }
    else if(yr_exclude)
    {
      myfit = lm(lcount ~ month + workingday + hour + weather + poly(atemp,3) + 
                   poly(humidity, 2) + month:workingday + month:humidity + 
                   workingday:hour + hour:atemp,  data=Newtrain[trainLocs,])
    }
    else{
      myfit = lm(lcount ~ year + month + wkday + hour + weather + 
                   poly(atemp, 3) + poly(humidity, 4) + poly(windspeed, 2) + 
                   year:month + year:humidity + month:weather + wkday:hour + 
                   wkday:atemp + wkday:humidity + hour:humidity + humidity:windspeed, 
                 data=Newtrain[trainLocs,])
    }    
    mypred.count = exp(predict(myfit, Newtest[testLocs,]))-1
    mypred.count[mypred.count < 0] = 0; 
    submission2[testLocs, "count"] = mypred.count
  }
}
write.csv(submission2, file = "Submission2.csv", row.names=FALSE)


#############################  RANDOM FOREST  ##################################
## The following code is retrieved from https://www.kaggle.com/benhamner/bike-sharing-demand/random-forest-benchmark/code
# slightly modified on the variables

train = read.csv("C:/Users/Jackie/Documents/1A-STAT 425/Project/Original Data Set/train.csv")
test = read.csv("C:/Users/Jackie/Documents/1A-STAT 425/Project/Original Data Set/test.csv")
Newtrain=DataModification(train)
Newtrain$lcount = log(train$count +1)
Newtest=DataModification(test)

extractFeatures<-function(data){
  if( dim(data)[2]>13){
    dat=subset(data,select=-c(datetime,casual,registered,count,lcount))
  }
 else{
   dat=subset(data,select=-c(datetime))
 }
 return(dat)
}

submission3 <- data.frame(datetime=test$datetime, count=NA)

for (i_year in unique(year(ymd_hms(test$datetime)))) {
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs   <- year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    testSubset <- Newtest[testLocs,]
    trainLocs  <- ymd_hms(train$datetime) <= min(ymd_hms(testSubset$datetime))
    rf <- randomForest(extractFeatures(Newtrain[trainLocs,]), Newtrain[trainLocs,"lcount"], ntree=200)
    pred=exp(predict(rf, extractFeatures(testSubset)))-1
    pred[pred<0]=0
    submission3[testLocs, "count"] =pred;  
  }
}
write.csv(submission3, file = "submission3.csv", row.names=FALSE)

rf <- randomForest(extractFeatures(train), train$count, ntree=100, importance=TRUE)
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p)


###################### Other Method (Time Series) ######################
train = read.csv("C:/Users/Jackie/Documents/1A-STAT 425/Project/Original Data Set/train.csv")
test = read.csv("C:/Users/Jackie/Documents/1A-STAT 425/Project/Original Data Set/test.csv")
Newtrain=DataModification(train)
Newtrain$lcount = log(train$count +1)
Newtest=DataModification(test)

## APPROACH ONE:
submission4=data.frame(datetime=Newtest$datetime,count=NA)
# merge and sort train and set data according to time
m_train=subset(Newtrain,select=-c(casual,registered,lcount,count))
m_train$lcount=Newtrain$lcount
m_test=Newtest;m_test$lcount=NA
mix=rbind(m_train,m_test)
sortmix=mix[with(mix,order(ymd_hms(datetime))),]
sortmix$predicted=NA
ts.plot(sortmix$lcount,main="Figure 1-3: logged Count")

# find turning points to determine the range we need to predict
start=NA; end=NA
for(i in 1:dim(sortmix)[1]){
  if(!is.na(sortmix$lcount[i]) && is.na(sortmix$lcount[i+1])){
    start=c(start,i)
  }
  if(is.na(sortmix$lcount[i]) && !is.na(sortmix$lcount[i+1])){
    end=c(end,i)
  }
}
start=start[-1];end=end[-1];end=c(end,dim(sortmix)[1])
start;end

# time series with linear regressor with only quantitative variables and its higher order
xregressor=subset(sortmix,select=c(atemp,humidity,windspeed))
xregressor$atemp2=sortmix$atemp^2;xregressor$humidity2=sortmix$humidity^2;xregressor$windspeed2=sortmix$windspeed^2
xregressor$atemp3=sortmix$atemp^3;xregressor$humidity3=sortmix$humidity^3

# First attempt is to build the model only on the data from the same month and same year
temp1=NA
for(i in 1:length(start))
{
  if(i==1)
  {
    model=auto.arima(sortmix$lcount[1:start[i]],approximation=TRUE,allowdrift=FALSE,
                     stepwise=TRUE,xreg=as.matrix(xregressor[1:start[i],]) )
  }
  else{
    model=auto.arima(sortmix$lcount[(end[i-1]+1):start[i]], approximation=TRUE,allowdrift=FALSE,
                     stepwise=TRUE,xreg=as.matrix(xregressor[(end[i-1]+1):start[i],]))
  }
  temp_pred= predict(model,n.ahead=end[i]-start[i],newxreg=xregressor[(start[i]+1):end[i],])$pred
  sortmix$predicted[(start[i]+1):end[i]]=temp_pred
  temp1=c(temp1,temp_pred)
}
temp1=temp1[-1];temp1=exp(temp1)-1;temp1[which(temp1<0)]=0

submission4$count=temp1
write.csv(submission4, file = "Submission4.csv", row.names=FALSE)

orig_count=exp(sortmix$lcount)-1;orig_count[which(orig_count<0)]=0
orig_pred=exp(sortmix$predicted)-1;orig_pred[which(orig_pred)<0]=0
plot.ts(orig_count,main="Figure 1-4: Time Series Prediction (1)",ylab="Count Number")
lines(orig_pred,col="red")
legend("topleft",c("Prediction","Observation"),col=c("red","black"),lwd=4)

#####APPROACH 2
submission5=data.frame(datetime=Newtest$datetime,count=NA)

# Second attempt is to build the model on all the data from the past, including the previous predicted values
temp=NA
for(i in 1:length(start))
{
  
  model=auto.arima(sortmix$lcount[1:start[i]],approximation=TRUE,allowdrift=FALSE,
                   stepwise=TRUE,xreg=as.matrix(xregressor[1:start[i],]) )
  
  temp_pred= predict(model,n.ahead=end[i]-start[i],newxreg=xregressor[(start[i]+1):end[i],])$pred
  sortmix$predicted[(start[i]+1):end[i]]=temp_pred
  sortmix$lcount[(start[i]+1):end[i]]=temp_pred
  temp=c(temp,temp_pred)
}
temp=temp[-1];temp=exp(temp)-1;temp[which(temp<0)]=0
submission5$count=temp
write.csv(submission5, file = "Submission5.csv", row.names=FALSE)

orig_count=exp(sortmix$lcount)-1;orig_count[which(orig_count<0)]=0
orig_pred=exp(sortmix$predicted)-1;orig_pred[which(orig_pred)<0]=0
plot.ts(orig_count,main="Figure 1-5: Time Series Prediction (2)",ylab="Count Number")
lines(orig_pred,col="red")
legend("topleft",c("Prediction","Observation"),col=c("red","black"),lwd=4)

