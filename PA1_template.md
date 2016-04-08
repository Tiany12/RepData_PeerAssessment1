# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity = read.csv("activity.csv")
file.remove("activity.csv")
```

```
## [1] TRUE
```

```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
Caluculate the total number of steps per day

```r
dailysteps <- aggregate(activity$steps, by =list(activity$date), FUN = sum, na.rm = TRUE)
```
Make a histogram

```r
colnames(dailysteps) <- c("date","steps")
hist(dailysteps$steps,main = "Daily Steps", xlab = "Number of steps")
```

![image](https://github.com/Tiany12/RepData_PeerAssessment1/blob/master/instructions_fig/Daily%20Steps.png)

Calculate the mean and median of total number of steps taken per day

```r
mean(dailysteps$steps)
```

```
## [1] 9354.23
```

```r
median(dailysteps$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
dailyactivity <- aggregate(activity$steps,by = list(activity$interval),FUN = mean,na.rm = TRUE)
colnames(dailyactivity) = c("interval","steps")
plot(dailyactivity$interval, dailyactivity$steps, type = "l", xlab = "Interval", ylab = "Number of steps")
```

![imagine](https://github.com/Tiany12/RepData_PeerAssessment1/blob/master/instructions_fig/Num.%20of%20steps.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
dailyactivity$interval[which.max(dailyactivity$steps)]
```

```
## [1] 835
```



## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(activity) - sum(complete.cases(activity))
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
FillInInterval <- function(interval){
avg = dailyactivity[(interval/5)+1,2]
ifelse(is.na(avg),0,avg)
}
```
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
completeactivity<- activity
completeactivity$steps <- ifelse(
    is.na(activity$steps),FillInInterval(activity$interval),activity$steps
  )
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
completedailysteps <- aggregate(completeactivity$steps,by = list(completeactivity$date),FUN = sum)
colnames(completedailysteps) <- c("date","steps")
hist(completedailysteps$steps,main = "Daily Steps (Imputed data)",xlab = "Number of steps")
```

![imagine](https://github.com/Tiany12/RepData_PeerAssessment1/blob/master/instructions_fig/Daily%20steps%20with%20imputed%20data.png)

```r
mean(completedailysteps$steps)
```

```
## [1] 10282.14
```

```r
median(completedailysteps$steps)
```

```
## [1] 10395
```

The mean is greater and the median is the same
## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
DayType <- function(date){
ifelse(weekdays(date) %in% c("Saturday","Sunday"), "weekend","weekday")
}
completeactivity$day.type <- factor(DayType(completeactivity$date))
completedailyactivity <- aggregate(completeactivity$steps,by = list(completeactivity$interval,completeactivity$day.type),FUN = mean)
colnames(completedailyactivity) = c("interval","day.type","steps")
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
with(completedailyactivity, xyplot(steps ~ interval | day.type, ylab = "Number of steps", xlab = "Interval"), layout = c(2,1), type = "l")
```

![imagine](https://github.com/Tiany12/RepData_PeerAssessment1/blob/master/instructions_fig/Num.%20of%20steps%20by%20weekday.png)
