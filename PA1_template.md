Analysis of Activity Monitoring Data
========================================================

To begin the analysis, we first load the required libraries and the CSV file into R. 



```r
library(lubridate)
library(plyr, warn.conflicts = FALSE)
library(ggplot2)
activityData <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
```

```
## Warning: cannot open file 'activity.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
actiData <- na.omit(activityData)
```

```
## Error: object 'activityData' not found
```


To explore the data a bit, we make a histogram of the total number of steps taken each day and also calculate and report the mean and median total number of steps taken per day.




```r
## First we plot the Histogram
actiHist <- ddply(actiData, "date", summarise, stepSum = sum(steps))
```

```
## Error: object 'actiData' not found
```

```r
hist(actiHist$stepSum, breaks = 30, col = "red", xlab = "Total Steps per Day", 
    main = "Histogram of Total Steps")
```

```
## Error: object 'actiHist' not found
```



Then we generate the mean and median steps per day


```r
# In the SummaryActivity dataframe, the meanSteps and medianSteps colums
# show the data for the corresponding day.
summaryActivity <- ddply(actiData, "date", function(x) {
    mean <- mean(x$steps)
    median <- median(x$steps)
    data.frame(meanSteps = mean, medianSteps = median)
})
```

```
## Error: object 'actiData' not found
```

```r
summaryActivity
```

```
## Error: object 'summaryActivity' not found
```



To get a sense of the daily activity pattern, we plot a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



```r
stepInterval <- ddply(activityData, "interval", function(x) {
    sumInterval <- sum(x$steps, na.rm = TRUE)
    lengthInt <- count(x$interval)
    aveStep <- sumInterval/lengthInt
})
```

```
## Error: object 'activityData' not found
```

```r
stepsInterval <- data.frame(interval = stepInterval$interval, MeanSteps = stepInterval$freq)
```

```
## Error: object 'stepInterval' not found
```

```r
plot(stepsInterval$interval, stepsInterval$MeanSteps, type = "l")
```

```
## Error: object 'stepsInterval' not found
```

```r
# To compute the interval with the maximum number of steps
maxSteps <- stepsInterval[which(stepsInterval$MeanSteps == max(stepsInterval$MeanSteps)), 
    1]
```

```
## Error: object 'stepsInterval' not found
```

The 5-minute interval with the maximum number of steps is 

```

Error in eval(expr, envir, enclos) : object 'maxSteps' not found

```

.

In order to get analyse the data with consideration for missing data, we try to impute values for data that are 'NA'


```r
## How many rows have missing data?
stepsNA <- length(which(is.na(activityData$steps)))
```

```
## Error: object 'activityData' not found
```

```r

## To complete the missing data('fill in blanks'), we use the following rule:
## for each interval that we do not have a step count, impute a value equal
## to ##average for that interval accross all the days for which we captured
## data.

activityData$compSteps <- ifelse(is.na(activityData$step), stepsInterval$MeanSteps, 
    activityData$step)
```

```
## Error: object 'activityData' not found
```

```r

activityHist <- ddply(activityData, "date", summarise, compSteps = sum(compSteps))
```

```
## Error: object 'activityData' not found
```

```r

hist(activityHist$compSteps, breaks = 30, col = "red", xlab = "Total Steps (missing values filled in)", 
    main = "Histogram of Total Steps")
```

```
## Error: object 'activityHist' not found
```

```r

meanComp <- mean(activityHist$compSteps)
```

```
## Error: object 'activityHist' not found
```

```r

medianComp <- median(activityHist$compSteps)
```

```
## Error: object 'activityHist' not found
```

```r

medianInc <- median(actiHist$stepSum)
```

```
## Error: object 'actiHist' not found
```

```r

meanInc <- mean(actiHist$stepSum)
```

```
## Error: object 'actiHist' not found
```


The total number of missing values is 

```

Error in eval(expr, envir, enclos) : object 'stepsNA' not found

```

. The mean and median for the dataset with NA values filled in is 

```

Error in eval(expr, envir, enclos) : object 'meanComp' not found

```

 and 

```

Error in eval(expr, envir, enclos) : object 'medianComp' not found

```

 and compares with 

```

Error in eval(expr, envir, enclos) : object 'meanInc' not found

```

 and 

```

Error in eval(expr, envir, enclos) : object 'medianInc' not found

```

 with missing values. Note that to complete the missing values we imputeed a value equal to the average for that interval accross all the days for which we captured data.

Lastly, to investigate if there are differences in activity patterns between weekdays and weekends, we make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis


```r
daf <- wday(activityData$date)
```

```
## Error: object 'activityData' not found
```

```r
activityData$day <- factor(ifelse(daf == 1 | daf == 7, "Weekend", "Weekday"))
```

```
## Error: object 'daf' not found
```

```r

pattern <- ddply(activityData, c("interval", "day"), function(x) {
    sumInterval <- sum(x$steps, na.rm = TRUE)
    lengthInt <- count(x$interval)
    aveStep <- sumInterval/lengthInt
})
```

```
## Error: object 'activityData' not found
```

```r
stepsDoW <- data.frame(interval = pattern$interval, day = pattern$day, meanSteps = pattern$freq)
```

```
## Error: object 'pattern' not found
```

```r
p <- ggplot(stepsDoW, aes(x = interval, y = meanSteps)) + geom_line()
```

```
## Error: object 'stepsDoW' not found
```

```r
p + facet_grid(day ~ .)
```

```
## Error: object 'p' not found
```


The grpghs show that the general pattern is similar for both weekdays and weekends, but that the weekday average steps are higher than the weekends'.








