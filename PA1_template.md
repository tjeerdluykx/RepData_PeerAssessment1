---
title: "PA1_template"
author: "Tjeerd Luykx"
date: "Sunday, July 19, 2015"
output: html_document
---

Description: R Markdown document for the Peer Assessment 1 of the Coursera class Reproducible Research. The document contains the analysis and output
code for the assignment. 

# Process and transform activity data

Load data into activity variable.

```{r}
activity <- read.csv("activity.csv")
```

Transform activity variable by creating date.time variable which has proper format.

```{r}
time <- formatC(activity$interval/100, digits = 2, format = "f")
activity$date.time <- as.POSIXct(paste(activity$date, time),
                                 format = "%Y-%m-%d %H.%M",
                                 tz = "GMT")
```

Adding time column of today for analysis.

```{r}
activity$time <- format(activity$date.time, format = "%H:%M:%S")
activity$time <- as.POSIXct(activity$time, format = "%H:%M:%S")
```

# What is mean total number of steps taken per day?

Calculate mean and median.

```{r}
total.steps <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)
mean(total.steps)
median(total.steps)
```

Create histogram of total steps.

```{r}
library(ggplot2)
qplot(total.steps, xlab = "Total steps", ylab = "Frequency")
```

# What is the average daily activity pattern?

Calculate mean steps and format into data frame in order to plot.

```{r}
mean.steps <- tapply(activity$steps, activity$time, mean, na.rm = TRUE)
daily.pattern <- data.frame(time = as.POSIXct(names(mean.steps)), mean.steps = mean.steps)
```

Plot daily.pattern data frame.

```{r}
library(scales)
ggplot(daily.pattern, aes(time, mean.steps)) + geom_line() + xlab("5-Minute Interval") + 
    ylab("Mean Number of Steps")
```

Calculate interval with maximal mean number of steps.

```{r}
max <- which.max(daily.pattern$mean.steps)
format(daily.pattern[max, "time"], format = "%H:%M")
```

# Imputing missing values

Calculate and report the total number of missing values in the dataset.

```{r}
sum(is.na(activity$steps))
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
steps.interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
activity <- merge(activity, steps.interval, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```{r}
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "Date", ylab = "Steps")
round(mean(steps.date$steps))
round(median(steps.date$steps))
```

Values do not differ greatly from first estimation with NAs but an increase in the mean of number of steps is displayed. Effect of imputing NAs is not significant. 

# Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend".

```{r}
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "Weekend"
    } else {
        "Weekday"
    }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

Make a panel plot containing a time series plot and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
par(mfrow = c(2, 1))
for (type in c("Weekend", "Weekday")) {
    steps.type <- aggregate(steps ~ interval, data = activity, subset = activity$daytype == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}
```


