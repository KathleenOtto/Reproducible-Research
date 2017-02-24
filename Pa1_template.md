---
title: "Pa1_template.Rmd"
author: "KO"
date: "20 February 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Reproducible Research - Course Project 1

### 1. Code for reading in the dataset and/or processing the data

```{r data_reading}
activity<-read.csv("activity.csv",header = TRUE,na.strings = "NA")
activity$date<-as.Date(activity$date)
head(activity)
```

### 2. Histogram of the total number of steps taken each day

```{r}
sum_steps<-tapply(activity$steps,activity$date,FUN = sum)
hist(sum_steps,breaks = 20,col = "blue",main = "Summary of Steps")
```

### 3. Mean and median number of steps taken each day

```{r}
mean_steps<-mean(sum_steps,na.rm = TRUE)
median_steps<-median(sum_steps,na.rm = TRUE)
mean_steps
median_steps
```

### 4. Time series plot of the average number of steps taken

```{r}
avg_steps_interval<-aggregate(x=list(steps = activity$step),by=list(interval=activity$interval),FUN = mean,na.rm=TRUE)
plot(avg_steps_interval,type="l",col="green",xlab = "Interval Series",ylab = "Average Steps")
```

### 5. The 5-minute interval that, on average, contains the maximum number of steps

```{r}
head(avg_steps_interval)
avg_steps_interval[which.max(avg_steps_interval$steps),]
```

### 6. Code to describe and show a strategy for imputing missing data

#### 6-1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
sum(is.na(activity))
```

#### 6-2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Stragegy = Replace NA's with mean for that 5-minute interval

#### 6-3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
activity_imputed<-activity
gaps<-is.na(activity_imputed$steps)
avg_interval<-tapply(activity_imputed$steps,activity_imputed$interval,mean,na.rm=TRUE)
activity_imputed$steps[gaps]<-avg_interval[as.character(activity_imputed$interval[gaps])]
head(activity_imputed)
sum(is.na(activity_imputed))
```

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Comparison of imputed vs. original step summary --> Increase of frequency of most frequent case
```{r}
sum_steps_imputed<-tapply(activity_imputed$steps,activity_imputed$date,FUN = sum)
par(mfrow=c(1,2))
hist(sum_steps_imputed,breaks = 20,col = "red",main = "Summary of Imputed Steps",ylim = c(0,20))
hist(sum_steps,breaks = 20,col = "blue",main = "Summary of Steps",ylim = c(0,20))
mean_steps_imputed<-mean(sum_steps_imputed,na.rm = TRUE)
median_steps_imputed<-median(sum_steps_imputed,na.rm = TRUE)
mean_steps_imputed
median_steps_imputed
```
With the imputed value the median value now matches the mean value.


## 7. Histogram of the total number of steps taken each day after missing values are imputed

Refer item 6-4.

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Split data by type of day, i.e. weekday or weekend.
```{r}
day_type <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
activity_imputed$date <- as.Date(activity_imputed$date)
activity_imputed$day_type <- sapply(activity_imputed$date, FUN=day_type)
head(activity_imputed)
```

Plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r}
weekday<-subset(activity_imputed,activity_imputed$day_type=="weekday")
weekend<-subset(activity_imputed,activity_imputed$day_type=="weekend")
#layout(matrix(1,1,2,2),heights = c(1,1))
par(mfrow=c(2,1),mar=c(4,2,1,0))
avg_steps_interval_weekday<-aggregate(x=list(steps = weekday$step),by=list(interval=weekday$interval),FUN = mean,na.rm=TRUE)
avg_steps_interval_weekend<-aggregate(x=list(steps = weekend$step),by=list(interval=weekend$interval),FUN = mean,na.rm=TRUE)
plot(avg_steps_interval_weekday,type="l",col="blue",xlab = "Weekday Interval Series",ylab = "Average Steps",ylim = c(0,250))
plot(avg_steps_interval_weekend,type="l",col="red",xlab = "Weekend Interval Series",ylab = "Average Steps",ylim = c(0,250))
```


## 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

See above.

