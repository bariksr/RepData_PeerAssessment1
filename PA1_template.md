---
title: "Reproducible Research Course Project 1"
output: html_document
---






## Loading and preprocessing the data



```r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, "%Y-%m-%d")
data$day <- weekdays(as.Date(data$date))
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
data_NA <- data[complete.cases(data$steps),]

dataQ1 <- data_NA %>%
    group_by(date) %>%
    summarize(step = sum(steps))

dataQ1 %>% ggplot(aes(x=step)) + 
    geom_histogram(binwidth = 2500) +
    xlab("Steps") +
    ylab("Frequency") 
```

![plot of chunk Q1](figure/Q1-1.png)

```r
mean(dataQ1$step)
```

```
## [1] 10766.19
```

```r
median(dataQ1$step)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
dataQ2 <- data_NA %>%
    group_by(interval) %>%
    summarize(step = mean(steps))

dataQ2 %>% ggplot(aes(x=interval,y=step)) + geom_line() +
    xlab("Interval") +
    ylab("Steps")
```

![plot of chunk Q2](figure/Q2-1.png)

```r
max_val <- max(dataQ2$step)
dataQ2[dataQ2$step==max_val,1,1]
```

```
## [1] 835
```

##Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
I will use the weekday average from the data without missing values
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The values do differ from the estimates from the first part. Some of the middle histogram bars are a bit higher than they were in the first part


```r
nrow(data) - sum(complete.cases(data$steps))
```

```
## [1] 2304
```

```r
data_day <- data_NA %>%
    group_by(day) %>%
    summarise(step = mean(steps))

data_impute <- merge(data,data_day, by = "day", all.x=TRUE)

data_impute$steps2 <- ifelse(is.na(data_impute$steps),data_impute$step,data_impute$steps)

dataQ3 <- data_impute %>%
    group_by(date) %>%
    summarize(step2 = sum(steps2),
              step = sum(steps,na.rm=TRUE))

dataQ3 %>% ggplot() + 
    geom_histogram(aes(x=step2, col = "red"),binwidth = 2500) +
    geom_histogram(aes(x=step, col = "blue"),binwidth = 2500) + 
    xlab("Steps") +
    ylab("Frequency") +
    scale_color_manual(labels = c("steps_NA_rm","steps_with_imputed_data"), values = c("red", "blue"))
```

![plot of chunk Q3](figure/Q3-1.png)

```r
mean(dataQ3$step)
```

```
## [1] 9354.23
```

```r
median(dataQ3$step)
```

```
## [1] 10395
```

##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
data4 <- data_impute
data4$new_factor <- ifelse(data4$day %in% c("Saturday","Sunday"),"weekend","weekday")
data4$new_factor <- as.factor(data4$new_factor)

dataQ4 <- data4 %>%
    group_by(interval,new_factor) %>%
    summarize(step = mean(steps2))

dataQ4 %>% ggplot(aes(x=interval,y=step)) + geom_line() +
    xlab("Interval") +
    ylab("Steps") + 
    facet_grid(rows = vars(new_factor))
```

![plot of chunk Q4](figure/Q4-1.png)
