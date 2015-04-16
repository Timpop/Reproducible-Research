---
title: "Reproducible Research Assignment"
author: "Timpop"
date: "Wednesday, March 25, 2015"
output: html_document
---
```{r,echo=FALSE}
library(RCurl)
library(ggplot2)
library(bitops)
library(scales)
library(gridExtra)
library(lattice)

x<-paste0(getwd(),"/Reproducible Research/act.zip")
y<-paste0(getwd(),"/Reproducible Research")
z<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url=z,destfile=x,method="auto")
unzip(zipfile=x,exdir=y)
```
#What is mean total number of steps taken per day?
```{r}
act<-read.csv(paste0(getwd(),"/Reproducible Research/activity.csv"),header=T)
act<-cbind(rownumber.at = rownames(act), act) 
act$rownumber.at<-as.numeric(as.character(act$rownumber.at))
perd<-aggregate(steps ~ date, data = act, FUN = mean,na.rm = TRUE) #mean
permedian<-aggregate(steps ~ date, data = act, FUN = median,na.rm = TRUE)#median
pers<-aggregate(steps ~ date, data = act, FUN = sum,na.rm = TRUE) #sum
daiydata<-cbind(as.Date(perd[,1]),perd[,2],permedian[,2],pers[,2])
daiydata<-as.data.frame(daiydata)
daiydata[,1]<-as.Date(daiydata[,1],origin="1969-12-31")
colnames(daiydata)<-c("Date","Mean","Median","Sum")
head(daiydata)
```

```{r,echo=FALSE}
perd[,1]<-as.Date(as.character(perd[,1]),"%Y-%m-%d")
pers[,1]<-as.Date(as.character(pers[,1]),"%Y-%m-%d")
permedian[,1]<-as.Date(as.character(permedian[,1]),"%Y-%m-%d")
perd$date=as.POSIXct(perd$date)
pers$date=as.POSIXct(pers$date)
permedian$date=as.POSIXct(permedian$date)
p1<-ggplot(pers, aes(x=date, y=steps)) + geom_bar(aes(fill=as.factor(date)),stat="identity", position="dodge") + scale_x_datetime(breaks = date_breaks("6 days"),labels = date_format("%m/%d"))+ggtitle(paste0("mean = ",round(mean(pers[,2]),3)," \n median = ",round(median(pers[,2]),3)))+theme(legend.position="none",axis.text.x=element_text(angle=-90))
p2<-ggplot(perd, aes(x=date, y=steps)) + geom_bar(aes(fill=as.factor(date)),stat="identity", position="dodge") + scale_x_datetime(breaks = date_breaks("6 days"),labels = date_format("%m/%d"))+ggtitle(paste0("mean = ",round(mean(perd[,2]),3)," \n median = ",round(median(perd[,2]),3)))+theme(legend.position="none",axis.text.x=element_text(angle=-90))
p3<-ggplot(permedian, aes(x=date, y=steps)) + geom_bar(aes(fill=as.factor(date)),stat="identity", position="dodge") + scale_x_datetime(breaks = date_breaks("6 days"),labels = date_format("%m/%d"))+ggtitle(paste0("mean = ",round(mean(permedian[,2]),3)," \n median = ",round(median(permedian[,2]),3)))+theme(legend.position="none",axis.text.x=element_text(angle=-90))
grid.arrange(p1,p2,p3, ncol=3)
```

#What is the average daily activity pattern? 
### First I would analyze everyday stutas, cuz the data source is activity monitoring devices, calculate with zero value would cause different outcome and interpretation. the analyze as follow is optional

```{r,echo=FALSE}
act_no_na<-na.omit(act[2])
act2<-cbind(rownumber = rownames(act_no_na), act_no_na) ;rownames(act2)<-NULL
act2[,1]<-as.numeric(as.character(act2[,1]))
step_no_na<-act[act2[,1],]
stepbig0<-step_no_na[step_no_na$steps>0,] 
big0dy<- aggregate(steps ~ date, data = stepbig0, FUN = mean)
no_nady<- aggregate(steps ~ date, data = step_no_na, FUN = mean)
big0dy[,1]<-as.Date(as.character(big0dy[,1]),"%Y-%m-%d")
no_nady[,1]<-as.Date(as.character(no_nady[,1]),"%Y-%m-%d") 

v1<-ggplot(big0dy, aes(x=date, y=steps))+ geom_line()+xlab("Period 2012-10-01 ~ 2012-11-29")+stat_summary(fun.y=mean, geom="point", shape=2, size=2)+scale_x_date(breaks = date_breaks("7 days"),labels = date_format("%m/%d"))+ggtitle("sports status of everyday")+theme(legend.position="none")

v2<-ggplot(no_nady, aes(x=date, y=steps))+ geom_line()+xlab("Period 2012-10-01 ~ 2012-11-29")+stat_summary(fun.y=mean, geom="point", shape=2, size=2)+scale_x_date(breaks = date_breaks("5 days"),labels = date_format("%m/%d"))+ggtitle("steps of everyday")+theme(legend.position="none")
grid.arrange(v1,v2, ncol=2)
```

#What is the average daily activity pattern?

```{r}
invlm <- aggregate(steps ~ interval, data = act, FUN = mean)
ggplot(invlm, aes(interval, steps))+ geom_bar(aes(fill=as.factor(interval)),stat="identity", position="dodge")+geom_line(stat="identity",size = 1)+ggtitle("daily activity pattern")
```
#Imputing missing values
###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
sum(is.na(act))
```
###Devise a strategy for filling in all of the missing values in the dataset. I use the mean/median for every 5-minute interval of days, the diagram show the mean and median like that:
```{r,echo=FALSE}
act$interval<-as.numeric(as.character(act$interval))
actinter<-aggregate(steps ~ interval, data = act, FUN = mean, na.rm = TRUE)
ggplot(actinter, aes(x=interval, y=steps))+geom_bar(aes(fill=as.factor(interval)),stat="identity", position="dodge")+geom_line(stat="identity",size = 1)+theme(legend.position="none",axis.text.x=element_text(angle=-90))
```

###Then put the mean value into NA and recalculate the value present on diagrm.
```{r,echo=FALSE}
act_na<-which(is.na(act[,2]))
step_na<-act[act_na,]
step_na<-cbind(rownumber.sn = rownames(step_na), step_na) 
step_na$rownumber.sn<-as.numeric(as.character(step_na$rownumber.sn))
test<-merge(step_na,actinter,by="interval",all=T)
test1<-merge(test,act,by="rownumber.at",all = T)
test1<-test1[,-c(2:5)]
test1$date<-as.character(test1$date)
test1$steps<-as.numeric(test1$steps)
test1[test1$date=="2012-10-01",]$steps<-test1[test1$date=="2012-10-01",]$steps.y
test1[test1$date=="2012-10-08",]$steps<-test1[test1$date=="2012-10-08",]$steps.y
test1[test1$date=="2012-11-01",]$steps<-test1[test1$date=="2012-11-01",]$steps.y
test1[test1$date=="2012-11-04",]$steps<-test1[test1$date=="2012-11-04",]$steps.y
test1[test1$date=="2012-11-09",]$steps<-test1[test1$date=="2012-11-09",]$steps.y
test1[test1$date=="2012-11-10",]$steps<-test1[test1$date=="2012-11-10",]$steps.y
test1[test1$date=="2012-11-14",]$steps<-test1[test1$date=="2012-11-14",]$steps.y
test1[test1$date=="2012-11-30",]$steps<-test1[test1$date=="2012-11-30",]$steps.y
test1<-test1[,-c(2,4)]
testmean<-aggregate(steps ~ date, data = test1, FUN = mean)
testmedian<-aggregate(steps ~ date, data = test1, FUN = median)
testmean$date=as.POSIXct(testmean$date)
testmedian$date=as.POSIXct(testmedian$date)
r1<-ggplot(testmean, aes(x=date, y=steps)) + geom_bar(aes(fill=as.factor(date)),stat="identity", position="dodge") + scale_x_datetime(breaks = date_breaks("6 days"),labels = date_format("%m/%d"))+ggtitle(paste0("mean of days with no NAs","\n mean = ",round(mean(testmean[,2]),3)," median = ",round(median(testmean[,2]),3)))+theme(legend.position="none",axis.text.x=element_text(angle=-90))
r2<-ggplot(testmedian, aes(x=date, y=steps)) + geom_bar(aes(fill=as.factor(date)),stat="identity", position="dodge") + scale_x_datetime(breaks = date_breaks("6 days"),labels = date_format("%m/%d"))+ggtitle(paste0("median of days with no NAs","\n mean = ",round(mean(testmedian[,2]),3)," median = ",round(median(testmedian[,2]),3)))+theme(legend.position="none",axis.text.x=element_text(angle=-90))
grid.arrange(r1,r2, ncol=2)
```

##Are there differences in activity patterns between weekdays and weekends?
###First,identified the weeksday and wenkend then make the diagram for weeksday and weekend.

```{r,echo=FALSE}
testmean2<-aggregate(steps ~ interval.y+date, data = test1, FUN = mean)
Sys.setlocale("LC_TIME", "English")
testmean2$date=as.Date(testmean2$date)
testmean2$date<-weekdays(testmean2$date)
wkdtest2<-testmean2[(testmean2$date=="Sunday"|testmean2$date=="Saturday"),]
wdytest2<-testmean2[(testmean2$date=="Monday"|testmean2$date=="Tuesday"|testmean2$date=="Wednesday"|testmean2$date=="Thursday"|testmean2$date=="Friday"),]
wkdtest2$interval.y=as.integer(as.character(wkdtest2$interval.y))
wdytest2$interval.y=as.integer(as.character(wdytest2$interval.y))
weekdays_steps <- aggregate(act$steps, by=list(interval = act$interval),FUN=mean, na.rm=T)
u1<-ggplot(wkdtest2,aes(x=interval.y, y=steps,group=interval.y))+geom_line(color="violet")+ggtitle(paste0("weekend steps ","\n mean = ",round(mean(wkdtest2[,3]),3)," median = ",round(median(wkdtest2[,3]),3)))+xlab("Interval")+theme_bw()
u2<-ggplot(wdytest2,aes(x=interval.y, y=steps, group=interval.y))+geom_line()+ggtitle(paste0("weekday steps ","\n mean = ",round(mean(wdytest2[,3]),3)," median = ",round(median(wdytest2[,3]),3)))+xlab("Interval")
grid.arrange(u1,u2, ncol=2)
```
