---
title: "PA1"
author: "Lisa Rodgers"
date: "May 15, 2016"
output: html_document
---

#Analysis of personal activity monitoring device data

-----


##Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

In this report we make use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. Data was provided for the course students for this particular assignment, avaliable to download via this link (avaliable at 2014-05-25).

In the report, we try to answer the following questions:

* What is mean total number of steps taken per day?
* What is the average daily activity pattern?
* Are there differences in activity patterns between weekdays and weekends?


-----

## R preparations

-----

```{R R_Preparations}
library(dplyr)
library(lubridate)
library(ggplot2)
```


-----

## Load the Data

-----

###Reading in the data

The data is loaded using the read.csv().

**NOTE:** It is assumed that you have already downloaded the activity.zip and saved it in your working directory. If not, please download the code [here](https://github.com/StarGazer007/RepData_PeerAssessment1/blob/master/activity.zip?raw=true), unzip it and save it to your working directory.

```{R Load_Data}
if(!file.exists('activity.csv')){
  unzip('activity.zip')
}
activityData <-  read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character", "integer"))

```


```{R Tidy_Date}
activityData$date <- ymd(activityData$date)
```

```{R Examine_Data}
str(activityData)

head(activityData)

summary(activityData)

```



----

### What is the average daily activity pattern?

----

1. Calculate the total number of steps everyday ignoring missing values.

```{R}

total_day <- activityData %>% 
  group_by(date) %>%
  summarise(total_steps=sum(steps,na.rm=TRUE),na=mean(is.na(steps))) %>% 
  print


mean_steps <- mean(total_day$total_steps, na.rm = TRUE)
median_steps <- median(total_day$total_steps, na.rm = TRUE)

```

The mean steps per day is **`r round(mean_steps) `**

The median steps per day is **`r round(median_steps) `**

```{r}

barplot(height = total_day$total_steps,names.arg=total_day$date,cex.names=0.68,main="Total Step Per Day", las=3,col="yellow")
abline(h=median_steps, lty=2,lwd=5, col="black")
abline(h=mean_steps, lty=2,lwd=5, col="blue")
text(x = 2,y=median_steps,pos=3,labels = "median")
text(x = 2,y=mean_steps ,pos=1,labels = "mean",col="blue")

## Saving to file
dev.copy(png, file="Total_Steps_Per_Day.png", height=600, width=800)
dev.off() 
```
 ![](https://github.com/StarGazer007/RepData_PeerAssessment1/blob/master/Total_steps_per_day.png) 

2. Make a histogram of the total number of steps taken each day

Histogram does not contain days where all observations are missing (i.e. there have to be a number of steps for at least one interval for that day, to be included). Otherwise, there would be about ten days with 0 steps.

```{r Hist_Total_Days}
total_day <- filter(total_day, na < 1)
hist(total_day$total_steps,col="yellow",breaks=20,main="TOTAL STEPS PER DAY",xlab="Steps per day")
abline(v=median(total_day$total_steps),lty=3, lwd=2, col="black")
legend(legend="median","topright",lty=3,lwd=2,bty = "n")

```


----

###What is the average daily activity pattern?

----

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
library(dplyr,quietly = TRUE)
daily_patterns <- activityData %>% 
  group_by(interval) %>% 
  summarise(average=mean(steps,na.rm=TRUE))

plot(x = 1:nrow(daily_patterns),
     y = daily_patterns$average,type = "l",
      main = "Daily Patterns",
     col = "red", xaxt = "n",
     xlab="Time Intervals", 
     ylab = "Average Steps")
     axis(1,labels=daily_patterns$interval[seq(1,288,12)],
     at = seq_along(daily_patterns$interval)[seq(1,288,12)])
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
max_steps_interval <- filter(daily_patterns,average==max(average))

```

Interval **<span style="background:#e6e6e6; color:red">  `r max_steps_interval$interval `</span>** contains on average the maximum number of steps at  **<span style="background:#e6e6e6; color:red">`r round(max_steps_interval$average) `</span>**.

----

###Imputing missing values

----

**Note** that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
na_number <- sum(is.na(activityData$steps))
na_number

percentage_na <- mean(is.na(activityData$steps))
percentage_na

```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

As the number of missing values in this dataset is **`r na_number `**, **`r round(percentage_na * 100)`%** of the activity data is missing. Therefore we impute missing values based on average number of steps in particular 5-minutes interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
without_NAs <- numeric(nrow(activityData))
for (i in 1:nrow(activityData))
{
        if (is.na(activityData[i,"steps"])==TRUE)
            {
                    without_NAs[i]<-filter(daily_patterns,interval==activityData[i,"interval"]) %>% select(average)
            } 
        else
            {
                    without_NAs[i]<-activityData[i,"steps"]
            }

}


activity_without_NAs<-mutate(activityData,steps_no_NAs=without_NAs)
head(activity_without_NAs)

```

Verify if process of imputing missing values correctly preserved original values (lines with no NAs)
```{r}
check <- filter(activity_without_NAs,!is.na(steps)) %>% 
  mutate(ok = (steps==steps_no_NAs))
mean(check$ok)
```



4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
total_day_noNAs <- activity_without_NAs %>% 
  mutate(steps_no_NAs=as.numeric(steps_no_NAs)) %>% 
  group_by(date) %>% 
  summarise(total_steps=sum(steps_no_NAs))

hist(total_day_noNAs$total_steps,col="red",breaks=20,main="Total steps per day",xlab="Steps per day")
abline(v=median(total_day$total_steps),lty=3, lwd=2, col="black")
legend(legend="median","topright",lty=3,lwd=2,bty = "n")

summary(total_day_noNAs$total_steps)

```

Recall before Imputing the missing values our 

```{r}
mean_steps 
median_steps
```

Imputing missing values, mean of the total number of steps taken per day increased by **`r round(mean(total_day_noNAs$total_steps)-mean_steps)`**. While median decreased **`r round(median_steps-median(total_day_noNAs$total_steps), digits=1)`**,compared to estimates from the first part (ingoring missing values). Imputing missing data resulted in increase of total daily number of steps (instead of each NAs we have average that is always >=0)


----

### Are there differences in activity patterns between weekdays and weekends?

----

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

```{r}
library(lubridate)
is_weekday <-function(date){
        if(wday(date)%in%c(1,7)) result<-"weekend"
        else
                result<-"weekday"
        result
}

activity_without_NAs <- mutate(activity_without_NAs,date=ymd(date)) %>% 
  mutate(day=sapply(date,is_weekday))

table(activity_without_NAs$day)

```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```{r}
library(ggplot2)

daily_patterns <- activity_without_NAs %>%     
  mutate(day=factor(day,levels=c("weekend","weekday")),steps_no_NAs=as.numeric(steps_no_NAs)) %>% 
  group_by(interval,day) %>% summarise(average=mean(steps_no_NAs))

head(daily_patterns)


ggplot(daily_patterns, aes(x=interval, y=average, color = day)) +
  geom_line() +
  facet_wrap(~day, ncol = 1, nrow=2) + ggtitle("Weekday vs Weekend Activity") + xlab("Time Intervals") + ylab("Average Steps") + theme(legend.position = "right")


```

----

From the two plots it seems that the test object is more active earlier in the day during weekdays compared to weekends, but more active throughout the weekends compared with weekdays (probably because the object is working during the weekdays, hence moving less during the day).

----


