---
title: "Peer Assignment 1"
author: "Viola"
date: "Sunday, December 14, 2014"
output: html_document
---

Data Prepraration
---

I load and preprocess the data as well as include all the libraries I need. 


```r
library(lubridate)
library(knitr)
act <- read.csv("activity.csv")
act$hour <- floor(act$interval/100)
act$minute <- 100*(act$interval/100 - floor(act$interval/100))
```

What is mean total number of steps taken per day?
---

```r
stepsperday <- NULL
for (j in 1:length(unique(act$date))){
  stepsperday[j] <- sum(act$steps[act$date == unique(act$date)[j]],na.rm=TRUE)
}
```

Histogram of the total number of steps taken per day


```r
hist(stepsperday, breaks=length(unique(act$date)), xlab = "Steps per Day", main = "Steps per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Median and Mean of Steps taken per Day


```r
stepsmean <- mean(stepsperday)
stepsmedian <- median(stepsperday)
stepsmean
```

```
## [1] 9354.23
```

```r
stepsmedian
```

```
## [1] 10395
```

What is the average daily activity pattern?
---

Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days


```r
timefiveminute <- 1:288/12
fiveminint <- NULL
for (k in 1:length(unique(act$interval))){
  fiveminint[k] <- mean(act$steps[act$interval == unique(act$interval)[k]], na.rm=TRUE)
}
plot(timefiveminute,fiveminint, type="l", xlab = "Hour of the day", ylab = "Average number of steps taken", main = "Average daily activity pattern")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The answer is


```r
unique(act$interval)[fiveminint == max(fiveminint)]
```

```
## [1] 835
```

Imputing missing values
---

Calculate and report the total number of missing values in the dataset


```r
sum(is.na(act$interval))
```

```
## [1] 0
```

```r
sum(is.na(act$steps))
```

```
## [1] 2304
```

```r
sum(is.na(act$date))
```

```
## [1] 0
```

As there are only NAs in the column with the steps the total number of rows with NAs is


```r
sum(is.na(act$steps))
```

```
## [1] 2304
```

Strategy

Idea: substitute NAs by average value of that time
Create a new dataset that is equal to the original dataset but with the missing data filled in.

I call this new dataset act2.


```r
act2 <- act
for (k in 1:length(act2$steps)){
  if (is.na(act2$steps[k]) == TRUE){
    act2$steps[k] <- round(fiveminint[k - 288*((k-1) %/% 288)])
  }  
}
```

Histogram of the total number of steps taken each day and 


```r
stepsperday2 <- NULL
for (j in 1:length(unique(act2$date))){
  stepsperday2[j] <- sum(act2$steps[act2$date == unique(act2$date)[j]],na.rm=TRUE)
}
hist(stepsperday2, breaks=length(unique(act2$date)), xlab = "Steps per Day", main = "Steps per Day after Manipulating Dataset")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

Mean and Median total number of steps taken per day


```r
stepsmean2 <- mean(stepsperday2)
stepsmedian2 <- median(stepsperday2)
stepsmean2
```

```
## [1] 10765.64
```

```r
stepsmedian2
```

```
## [1] 10762
```

Do these values differ from the estimates from the first part of the assignment? 


```r
stepsmean2 - stepsmean
```

```
## [1] 1411.41
```

```r
stepsmedian2 - stepsmedian
```

```
## [1] 367
```

Yes, they do differ as the difference is not zero. 

What is the impact of imputing missing data on the estimates of the total daily number of steps?

As we have seen the impact is that in the new dataset the mean and median are both higher.

Are there differences in activity patterns between weekdays and weekends?
---

New factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day


```r
act2$weekday <- factor(act2$date)
act2$weekday <- as.Date(act2$weekday)
act2$newweekday <- NULL 
for (j in 1:length(act2$weekday)){
  if (weekdays(act2$weekday[j]) == "Samstag" | weekdays(act2$weekday[j]) == "Sonntag"){
    act2$newweekday[j] <- "weekend"
  }
  else {
    act2$newweekday[j] <- "weekday"
  }
}
act2$newweekday <- factor(act2$newweekday)
```

Panel Plot of average number of steps taken, averaged across all weekday days and all weekend days


```r
fiveminwd <- NULL
fiveminwe <- NULL
for (k in 1:length(unique(act2$interval))){
  fiveminwd[k] <- mean(act2$steps[act2$interval == unique(act2$interval)[k] & act2$newweekday == "weekday"], na.rm=TRUE)
  fiveminwe[k] <- mean(act2$steps[act2$interval == unique(act2$interval)[k] & act2$newweekday == "weekend"], na.rm=TRUE)
}
par(mfrow=c(2,1))
plot(timefiveminute,fiveminwd, type="l", xlab = "Hour of the day", ylab = "Average number of steps taken", main = "Average daily activity pattern on weekday")
plot(timefiveminute,fiveminwe, type="l", xlab = "Hour of the day", ylab = "Average number of steps taken", main = "Average daily activity pattern on weekend")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
