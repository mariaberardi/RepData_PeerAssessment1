---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

- Dataset: [Activity monitoring data [52K]](https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip)

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

We download the dataset from the link above and store it in the desired directory. We then select our chosen directory as working directory and store the dataset in the variable *rawdata*. 


```r
 setwd("~/Desktop/R-programming-class/activity_data")
 rawdata <- read.csv(file = 'activity.csv')
```

We install (if necessary) and load all libraries required for our data processing. 


```r
if (("dplyr" %in% (installed.packages())) == F) {
        install.packages("dplyr")
}

if (("stringr" %in% (installed.packages())) == F) {
        install.packages("stringr")
}

if (("sqldf" %in% (installed.packages())) == F) {
        install.packages("sqldf")
}

if (("lubridate" %in% (installed.packages())) == F) {
        install.packages("lubridate")
}

if (("lattice" %in% (installed.packages())) == F) {
        install.packages("lattice")
}
```


```r
library(dplyr)
library(stringr)
library(sqldf)
library(lubridate)
library(lattice)
```
We first inspect the raw data. 


```r
head(rawdata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(rawdata)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

We first transform the interval column to datetime format, using the simple conversion that every 12 5-minute intervals correspond to one hour. This will allow us to perform a day-by-day analysis later. 


```r
## create a function that converts an interval string into a datetime YYYY-MM-DD
interval_to_datetime <- function(interv) {
        sapply(interv, function(interv) {
                # fill the interval string with zeros and split into four characters
                digits = strsplit(str_pad(interv, 4, pad = "0"),"")[[1]]
                # create a time string in the format XX:XX:XX
                t_string = paste0(paste(digits[1:2], collapse = ""), ":", paste(digits[3:4], collapse = ""), ":00")
                # convert the time string into a datetime object
                dt = as.POSIXct(strptime(paste("1970-01-01", t_string), "%Y-%m-%d %H:%M:%S", tz = "GMT"))
                # return the result
                return(dt)
        })
}
```

We add the datetime column and rename the processed data *activity*. 


```r
## add the new column to the data frame using the function interval_to_datetime
activity <- mutate(tbl_df(rawdata), interv_time = interval_to_datetime(interval))
```

```
## Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
## ℹ Please use `tibble::as_tibble()` instead.
```

## What is mean total number of steps taken per day?

Now we can use the processed data to calculate the total number of steps taken per day. 


```r
total_steps_by_day <- 
        activity %>%
        na.omit %>%
        group_by(date) %>% 
        summarise(steps = sum(steps))
```

We can also make a histogram of the total number of steps taken each day:


```r
hist(total_steps_by_day$steps, xlab = "Total Steps Per Day", ylab = "Number of Days", breaks = 10,
     main = "Frequency of Total Steps per Day")
```

![](Activity_data_assessment_files/figure-html/total_steps_by_day_histogram-1.png)<!-- -->

and we can calculate and report the mean and median of the total number of steps taken per day:


```r
mean_steps <- format(mean(total_steps_by_day$steps), nsmall = 0)
median_steps <- format(median(total_steps_by_day$steps), nsmall = 0)
```

We can answer the desired questions:  
- The mean of the total number of steps taken per days is 'mean_steps' = 10766.19.  
- The median of the total number of steps taken per days is 'median_steps' = 10765.

## What is the average daily activity pattern?

Next we make a time series plot (type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
avg_steps_per_interval <- 
        activity %>% 
        na.omit %>%
        group_by(interv_time) %>% 
        summarise(avg_steps = mean(steps))
plot(avg_steps_per_interval$interv_time, avg_steps_per_interval$avg_steps, type = "l", 
     xlab = "Interval", ylab = "Average Steps per Interval", main = "Average Daily Activity Pattern")
```

![](Activity_data_assessment_files/figure-html/avg_steps_per_interval_plot-1.png)<!-- -->

We now want to asnwer the question: which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_max_steps <- avg_steps_per_interval$interv_time[which.max(avg_steps_per_interval$avg_steps)]
datetime_max_steps <- as.POSIXct(interval_max_steps, origin = "1970-01-01", tz = "GMT")
datetime_max_steps <- strftime(datetime_max_steps, format="%H:%M", tz = "GMT")
```

The 5-minute interval at 'datetime_max_steps' = 08:35 contains the maximum number of steps on average accross all the days in the dataset.

## Imputing missing values

We now compute the total number of missing values in the dataset.


```r
## check data for NA values
na_steps <- sum(is.na(activity$steps))
na_dates <- sum(is.na(activity$date))
na_intervals <- sum(is.na(activity$interval))
```

We have 2304 NA values for steps, no NA values for dates, and no NA values for intervals.

Our next objective is to devise a strategy for filling in all of the missing values in the dataset. Common practices are to use the mean/median for that day, or the mean for that 5-minute interval. We will use the mean. 


```r
activity_imp <- 
        sqldf('SELECT act.*, avg.avg_steps FROM "avg_steps_per_interval" as avg
              JOIN "activity" as act ON act.interv_time = avg.interv_time
              ORDER BY act.date, act.interv_time') 
activity_imp$steps[is.na(activity_imp$steps)] <- activity_imp$avg_steps[is.na(activity_imp$steps)]
activity_imp$avg_steps <- NULL
```

Now we generate a histogram of the total number of steps taken each day, then calculate and report the mean and median total number of steps taken per day. The next questions we wish to answer are:  
- Do these values differ from the estimates from the first part of the assignment?   
- What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
## Calculate the total number of steps taken per day from the data frame with imputed values
total_steps_by_day <- 
        activity_imp %>%
        group_by(date) %>% 
        summarise(steps = sum(steps))
hist(total_steps_by_day$steps, xlab = "Total Steps Per Day", ylab = "Number of Days", breaks = 10,
     main = "Frequency of Total Steps per Day")
```

![](Activity_data_assessment_files/figure-html/total_steps_by_day_histogram_imputed-1.png)<!-- -->

We calculate and report the mean and median total number of steps taken per day:



- The mean of the total number of steps taken per days is 10766.19.
- The median of the total number of steps taken per days is 10766.19.

The mean and median values are almost exactly equal as the values calculated before imputing missing values.
The histogram shows higher step frequency counts. 
The chosen strategy did not change the mean and median values since the imputed values were mean values themselves. Therefore the histogram shape did not change, but the step frequency increased, since more values are available in the data frame.

## Are there differences in activity patterns between weekdays and weekends?

We use the dataset with the filled-in missing values for this part as well.

First we create a new factor variable in the dataset using a binary classification: “weekday” and “weekend”.


```r
weekdays <- function(x) {
        weekX <- ifelse(wday(x, label = T, abbr = T) %in% c("Sat", "Sun"), "weekend", "weekday")
        return(as.factor(weekX))
}
activity_imp$weekdays <- sapply(activity_imp$date, weekdays)
```

We make a panel plot containing a time series plot (type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
avg_steps_per_interval <- 
        activity_imp %>% 
        group_by(interv_time, weekdays) %>% 
        summarise(avg_steps = mean(steps))
```

```
## `summarise()` has grouped output by 'interv_time'. You can override using the
## `.groups` argument.
```

```r
xyplot(avg_steps ~ interv_time | weekdays, data = avg_steps_per_interval, type = 'l', layout = c(1, 2),
       xlab = "Interval", ylab = "Average Steps per Interval", main = "Average Daily Activity Pattern\nWeekday or Weekend")
```

![](Activity_data_assessment_files/figure-html/avg_steps_per_interval_weekdays_plot-1.png)<!-- -->


