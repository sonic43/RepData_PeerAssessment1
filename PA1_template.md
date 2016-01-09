---
title: 'Reproducible Research: Peer Assessment 1'
author: "Samuel_Lin"
date: "2016/1/7"
output: html_document
---
## Loading and preprocessing the data

```{r loading data}
Sys.setlocale("LC_TIME", "English")
activity <- read.csv("activity.csv")
activity$date <-  as.Date(activity$date, format = "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```{r mean steps}
suppressMessages(library(dplyr))
activity_day <- activity %>%
                group_by(date) %>%
                summarize(sumSteps = sum(steps, na.rm = T))
                
hist(activity_day$sumSteps, 
     xlab = "Total steps by day", 
     ylab = "Frequency (Days)", 
     main = "Frequency of Daily Number of Steps", 
     breaks = 10)

meanSteps <- mean(activity_day$sumSteps)
medianSteps <- median(activity_day$sumSteps)
```
The mean of the total number of steps taken per day is `r round(meanSteps,0)`.
The median of the total number of steps taken per day is `r medianSteps`.

## What is the average daily activity pattern?
### Time Series Plot
```{r time series plot}
activity_interval <- activity %>%
                     group_by(interval) %>%
                     summarize(meanSteps = mean(steps, na.rm = T))
                     
plot(activity_interval, type = "l", 
     xlab = "5-min interval", 
     ylab = "Avg number of Steps",
     main = "Avg Steps by 5-min interval")

```

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r max number of steps}
maxSteps <- which.max(activity_interval$meanSteps)
activity_interval[maxSteps,]
```
The `r activity_interval[maxSteps,1]`th 5-mins interval has the max `r round(activity_interval[maxSteps,2],2)` steps on average across all the days.

## Imputing missing values
### Calculate and report the total number of missing values in the dataset
```{r missing value}
NAs <- sum(is.na(activity$steps))
```
There are `r NAs` missing values.

### Devise a strategy for filling in all of the missing values in the dataset.
The strategy is to substitute missing values with average number of steps of the same 5-minute interval.

### Create a new dataset that is equal to the original dataset but with the missing data filled in
```{r filling the missing values}
activity2 <- merge(activity, activity_interval)
for (i in 1 : nrow(activity2)) {
      if (is.na(activity2[i,2])) {
            activity2[i,2] <- activity2[i,4]
      }
}
activity2 <- activity2[, -4]
```

### The impact of imputing missing data on the estimates of the total daily number of steps
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
```{r new data set}
activity2_day <- activity2 %>%
                 group_by(date) %>%
                 summarize(sumSteps = sum(steps, na.rm = T))
                 
hist(activity2_day$sumSteps, 
     xlab = "Total steps by day", 
     ylab = "Frequency (Days)", 
     main = "Frequency of Daily Number of Steps", 
     breaks = 10)

meanSteps2 <- mean(activity2_day$sumSteps)
medianSteps2 <- median(activity2_day$sumSteps)

meanSteps - meanSteps2
medianSteps - medianSteps2
```
With imputing missing data, 
The mean and median total number of steps are both greater than the original data set.

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
```{r new factor: weekday and weekend}
activity2$weekdays <- weekdays(activity2$date)
activity2$weekdays <- ifelse(activity2$weekdays %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```

### Make a panel plot containing a time series plot, weekdays and weekend days
```{r panel plot}
activity2_interval <- activity2 %>%
                      group_by(interval, weekdays) %>%
                      summarize(meanSteps = mean(steps, na.rm = T))

library(ggplot2)
ggplot(activity2_interval, aes(interval, meanSteps)) +
       geom_line() +
       labs(x = "5-min Interval", y = "Avg Number of Steps") +
       facet_wrap(~ weekdays, ncol = 1) +
       theme_bw()
```

According to the plots, we can say there is a differance between weekdays and weekend.
Weekdays have a peak over 200 steps and other intervals are below 100 steps.
Weekend have average higher steps than weekdays.
