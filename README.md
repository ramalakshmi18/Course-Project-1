title: "Reproducible Research: Peer Assessment 1"
author: "Peter Dunham"
date: "February 22, 2016"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

The raw data "activity.csv"  to be included in the GitHub repo. This .csv file is to be extracted from the activity.zip file.  The file can also be downloaded from:  https://www.coursera.org/learn/reproducible-research/peer/gYyPt/course-project-1

Eitherway, the working directory should be set to the directory that contains "activity.csv"

After setting your working directory:
```{r}
   #getwd('YOUR DIRECTORY PATH HERE')
```

Then you can load activity data using "read.csv"
```{r}
   RawActivityData<-read.csv(paste0(getwd(),'/activity.csv'))
```

The following displays a small subset of the data to show how "interval" fields relate to
5 minute intervals in a 24 hour date:
```{r}
   RawActivityData[861:868,]
```

## What is the average daily activity pattern?

Histogram of the total number of steps taken each day
```{r}
StepsByDay<-aggregate(list(Steps=RawActivityData$steps),list(Date=RawActivityData$date),sum)
hist(StepsByDay$Steps, col="blue", breaks=20)
```

### What is mean total number of steps taken per day?

What is mean total number of steps taken per day?

```{r}
mean(StepsByDay$Steps, na.rm=TRUE)
```

What is the median number of steps taken per day?

```{r}
median(StepsByDay$Steps, na.rm=TRUE)
```

### 5-interval activity

Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
ByInterval<-aggregate(list(Steps=RawActivityData$steps),list(Interval=RawActivityData$interval),na.rm=TRUE, na.action=NULL,mean)
plot(ByInterval)
```

### The 5-minute interval that, on average, contains the maximum number of steps

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
ByInterval[ByInterval$Steps == max(ByInterval$Steps),]
```

On average 8:35-8:40AM has the maximum number of streps.

## Inputing missing values

Number of missing cases:
```{r}
   nrow(RawActivityData)-NROW(na.omit(RawActivityData))
```

Replacing data with mean of interval 
```{r message=F, warning=F}
library(dplyr)
#Function to replace with mean of interval
myreplace<-function(interval){
  as.integer(ByInterval$Steps[match(interval, ByInterval$Interval)])
}
FilledActivityData<-mutate(RawActivityData,steps = ifelse(!complete.cases(RawActivityData),myreplace(RawActivityData$interval), RawActivityData$steps))
```

This shows a small subset of filled data.
```{r}
   FilledActivityData[2250:2260,]
```

### Histogram of the total number of steps taken each day after missing values are inputed

```{r, message=F, warning=F}
library(ggplot2)
#RawDataActivity with set marked as missing to show it has not been filled
ByDateMissing<-aggregate(list(Steps=RawActivityData$steps),list(Date=RawActivityData$date),sum)
ByDateMissing$set<-'missing'
#Filled DataActivity had NA data filled with mean of interval
ByDateFilled<-aggregate(list(Steps=FilledActivityData$steps),list(Date=FilledActivityData$date),sum)
ByDateFilled$set<-'filled'
compare<-rbind(ByDateFilled,ByDateMissing)
ggplot(compare, aes(Steps, fill = set)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
```

What is mean total number of steps taken per day?

```{r}
mean(ByDateFilled$Steps, na.rm=TRUE)
```

This number is different from omitted NA data by:
```{r}
mean(ByDateFilled$Steps, na.rm=TRUE)-mean(ByDateMissing$Steps, na.rm=TRUE)
```


What is the median number of steps taken per day?

```{r}
median(ByDateFilled$Steps, na.rm=TRUE)
```

This number is different from omitted NA data by:
```{r}
median(ByDateFilled$Steps, na.rm=TRUE)-median(ByDateMissing$Steps, na.rm=TRUE)
```

## Are there differences in activity patterns between weekdays and weekends?
```{r}
library(dplyr)
library(lattice)
#Function to tag a date as a "weekend" or "weekday"
daytype<-function(daystring)
{
  weekend<-c("Saturday","Sunday")
  ifelse(is.element(as.character(daystring),weekend),"weekend","weekday")
}
#converting date fields to "Date" data type
ByDateFilled$Date<-as.Date(ByDateFilled$Date)
FilledActivityData$date<-as.Date(FilledActivityData$date)
DayAddedActivityData<-mutate(FilledActivityData, DayOfWeek = weekdays(FilledActivityData$date))
DayAddedActivityData<-mutate(DayAddedActivityData,WeekType=daytype(DayAddedActivityData$DayOfWeek))
DayCompare<-aggregate(data=DayAddedActivityData,list(Steps=DayAddedActivityData$steps),list(days=DayAddedActivityData$WeekType, interval=DayAddedActivityData$interval),na.rm=TRUE, na.action=NULL,mean)
#foprinting debug
#weekdays(ByDateFilled$Date)
#Adding a DayOfWeek column with "weekend" or "weekday" tag
ByDateFilled<-mutate(ByDateFilled, DayOfWeek = weekdays(ByDateFilled$Date))
##ByDay 
MeanByDayOfWeek<-aggregate(list(Steps=ByDateFilled$Steps),list(Days=ByDateFilled$DayOfWeek),na.rm=TRUE, na.action=NULL,mean)
DataWithDayType<-mutate(ByDateFilled, WeekType=daytype(ByDateFilled$DayOfWeek))
xyplot(Steps ~ interval | days, type="l",data = DayCompare, layout = c(1, 2))
```
© 2022 GitHub, Inc.
Terms
Privacy
Security
Status
Docs
Contact G
