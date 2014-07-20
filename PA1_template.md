# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
actdata <- read.csv("activity.csv")
actdata$date <- as.Date(actdata$date)
```


## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day:

```r
library(plyr)
library(ggplot2)
actdaily <- ddply(actdata, .(date), summarize, dailysteps = sum(steps))
qplot(dailysteps, data = actdaily)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Calculate and report the mean and median total number of steps taken per day

```r
actdailysta <- list(mean = mean(actdaily$dailysteps, na.rm = TRUE), 
     median = median(actdaily$dailysteps, na.rm = TRUE))
actdailysta
```

```
## $mean
## [1] 10766
## 
## $median
## [1] 10765
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
actoneday <- ddply(actdata, .(interval), summarize, avgsteps = mean(steps, na.rm = TRUE))
qplot(interval, avgsteps, data = actoneday, geom = "line")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
actoneday[actoneday$avgsteps == max(actoneday$avgsteps), ]
```

```
##     interval avgsteps
## 104      835    206.2
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(actdata$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Here the mean for that 5-minute interval will be used for filling the NAs. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
actdatanona <- actdata
for(i in 1:nrow(actdatanona)){
  if(is.na(actdatanona$steps[i])){
    actdatanona$steps[i] <- actoneday[actoneday$interval == actdatanona$interval[i], "avgsteps"]
  }
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
actdailynona <- ddply(actdatanona, .(date), summarize, dailysteps = sum(steps))
qplot(dailysteps, data = actdailynona)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
actdailynonasta <- list(mean = mean(actdailynona$dailysteps), 
     median = median(actdailynona$dailysteps))
```
The mean is 1.0766 &times; 10<sup>4</sup> and the median is 1.0766 &times; 10<sup>4</sup> for imputed data, which is almost the same value for data with missing values, mean: 1.0766 &times; 10<sup>4</sup> and median: 10765. The impact can be ignored.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
weekdayend <- function(x){
  if (x == "Sunday" | x == "Saturday") {
    "weekend"
  } else {
      "weekday"
    }
}

actdatanona$weekday = as.factor(mapply(weekdayend, weekdays(actdatanona$date)))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
actweekday <- ddply(actdatanona, .(weekday, interval), summarize, avgsteps = mean(steps))
qplot(interval, avgsteps, data = actweekday, facets = weekday~., geom = "line")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

