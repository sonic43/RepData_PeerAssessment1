# Reproducible Research: Peer Assessment 1
Samuel_Lin  
2016/1/7  
## Loading and preprocessing the data


```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
activity <- read.csv("activity.csv")
activity$date <-  as.Date(activity$date, format = "%Y-%m-%d")
```


## What is mean total number of steps taken per day?


```r
suppressMessages(library(dplyr))
activity_day <- activity %>%
                group_by(date) %>%
                summarize(sumSteps = sum(steps, na.rm = T))
                
hist(activity_day$sumSteps, 
     xlab = "Total steps by day", 
     ylab = "Frequency (Days)", 
     main = "Frequency of Daily Number of Steps", 
     breaks = 10)
```

![](PA1_template_files/figure-html/mean steps-1.png)\

```r
meanSteps <- mean(activity_day$sumSteps)
medianSteps <- median(activity_day$sumSteps)
```
The mean of the total number of steps taken per day is 9354.
The median of the total number of steps taken per day is 10395.

## What is the average daily activity pattern?
### Time Series Plot

```r
activity_interval <- activity %>%
                     group_by(interval) %>%
                     summarize(meanSteps = mean(steps, na.rm = T))
                     
plot(activity_interval, type = "l", 
     xlab = "5-min interval", 
     ylab = "Avg number of Steps",
     main = "Avg Steps by 5-min interval")
```

![](PA1_template_files/figure-html/time series plot-1.png)\

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxSteps <- which.max(activity_interval$meanSteps)
activity_interval[maxSteps,]
```

```
## Source: local data frame [1 x 2]
## 
##   interval meanSteps
##      (int)     (dbl)
## 1      835  206.1698
```
The 835th 5-mins interval has the max 206.17 steps on average across all the days.

## Imputing missing values
### Calculate and report the total number of missing values in the dataset

```r
NAs <- sum(is.na(activity$steps))
```
There are 2304 missing values.

### Devise a strategy for filling in all of the missing values in the dataset.
The strategy is to substitute missing values with average number of steps of the same 5-minute interval.

### Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
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

```r
activity2_day <- activity2 %>%
                 group_by(date) %>%
                 summarize(sumSteps = sum(steps, na.rm = T))
                 
hist(activity2_day$sumSteps, 
     xlab = "Total steps by day", 
     ylab = "Frequency (Days)", 
     main = "Frequency of Daily Number of Steps", 
     breaks = 10)
```

![](PA1_template_files/figure-html/new data set-1.png)\

```r
meanSteps2 <- mean(activity2_day$sumSteps)
medianSteps2 <- median(activity2_day$sumSteps)

meanSteps - meanSteps2
```

```
## [1] -1411.959
```

```r
medianSteps - medianSteps2
```

```
## [1] -371.1887
```
With imputing missing data, 
The mean and median total number of steps are both greater than the original data set.

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend”

```r
activity2$weekdays <- weekdays(activity2$date)
activity2$weekdays <- ifelse(activity2$weekdays %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```

### Make a panel plot containing a time series plot, weekdays and weekend days

```r
activity2_interval <- activity2 %>%
                      group_by(interval, weekdays) %>%
                      summarize(meanSteps = mean(steps, na.rm = T))

library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
ggplot(activity2_interval, aes(interval, meanSteps)) +
       geom_line() +
       labs(x = "5-min Interval", y = "Avg Number of Steps") +
       facet_wrap(~ weekdays, ncol = 1) +
       theme_bw()
```

![](PA1_template_files/figure-html/panel plot-1.png)\

According to the plots, we can say there is a differance between weekdays and weekend.
Weekdays have a peak over 200 steps and other intervals are below 100 steps.
Weekend have average higher steps than weekdays.
