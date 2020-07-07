---
title: "Reproducible Research - Course Project 1"
author: "César Carrera"
date: "6/7/2020"
output: 
  html_document: 
    keep_md: yes
---



## Introduction

This document containts the scritps require for the Course Project 1 of the Reproducible Research course, acording to the instructions.


## Reading and processing

Loading libraries

```
## -- Attaching packages ------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v tibble  3.0.1     v dplyr   0.8.3
## v tidyr   1.0.2     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
## v purrr   0.3.3
```

```
## -- Conflicts ---------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```


Reading the data:


```r
data <- read.csv("activity.csv")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Processing: We need to format the date variable and create the identifier for weekday and weekend.


```r
data <- data %>%
    mutate(date_format = ymd(data$date))
```


##Histogram of the total number of steps taken each day
First, we calculate the total number of steps taken per day.


```r
data <- data %>%
    group_by(date_format) %>%
    mutate(steps_by_day = sum(steps, na.rm = TRUE)) %>%
    ungroup()

data$steps_by_day[is.na(data$steps)] <- NA_real_
```

The histogram for the steps taken per day is: 

```r
hist(data$steps_by_day, main = "Total number of steps taken per day", xlab = "Steps per day", col="green")
```

![](Reproducible-Research---Course-Project-1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The mean of the total number of steps taken per day is **NA** and the median of the total number of steps taken per day is **NA**.


##Average number of steps taken

First, we calculate the total number of steps taken per day.


```r
average_steps <- data %>%
    group_by(interval) %>%
    summarise(average_by_interval = mean(steps, na.rm = TRUE)) %>%
    ungroup()
```


The time series plot of the average number of steps taken is the following:

```r
plot(average_steps$interval, average_steps$average_by_interval, type = "l", lwd = 2, col="green", xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```

![](Reproducible-Research---Course-Project-1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The interval with the maximum number of steps is: **835**.

##Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
summary(data)
```

```
##      steps                date          interval       date_format        
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0   Min.   :2012-10-01  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8   1st Qu.:2012-10-16  
##  Median :  0.00   2012-10-03:  288   Median :1177.5   Median :2012-10-31  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5   Mean   :2012-10-31  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2   3rd Qu.:2012-11-15  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0   Max.   :2012-11-30  
##  NA's   :2304     (Other)   :15840                                        
##   steps_by_day  
##  Min.   :   41  
##  1st Qu.: 8841  
##  Median :10765  
##  Mean   :10766  
##  3rd Qu.:13294  
##  Max.   :21194  
##  NA's   :2304
```
There are 2304 cases of NA's in the variable steps. 

The filling in all of the missing values in the dataset, for the variable steps, is goint to be the mean average number of steps by day


```r
data2 <- data %>%
    group_by(date_format) %>%
    mutate(steps_by_day = sum(steps, na.rm = TRUE)) %>%
    ungroup()

data2 <- data2 %>%
    mutate(steps_no_na = case_when(!is.na(steps) ~ steps, TRUE ~ steps_by_day))

summary(data2)
```

```
##      steps                date          interval       date_format        
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0   Min.   :2012-10-01  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8   1st Qu.:2012-10-16  
##  Median :  0.00   2012-10-03:  288   Median :1177.5   Median :2012-10-31  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5   Mean   :2012-10-31  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2   3rd Qu.:2012-11-15  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0   Max.   :2012-11-30  
##  NA's   :2304     (Other)   :15840                                        
##   steps_by_day    steps_no_na    
##  Min.   :    0   Min.   :  0.00  
##  1st Qu.: 6778   1st Qu.:  0.00  
##  Median :10395   Median :  0.00  
##  Mean   : 9354   Mean   : 32.48  
##  3rd Qu.:12811   3rd Qu.:  0.00  
##  Max.   :21194   Max.   :806.00  
## 
```
As shown, the variable steps_no_na has no NA's. Now we plot the histogram for the new variable:

```r
data2 <- data2 %>%
    group_by(date_format) %>%
    mutate(steps_by_day_no_na = sum(steps_no_na, na.rm = TRUE)) %>%
    ungroup()

hist(data2$steps_by_day_no_na, main = "Total number of steps taken per day (imputing NA's)", xlab = "Steps per day", col="green")
```

![](Reproducible-Research---Course-Project-1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

The mean of the total number of steps taken per day (with no NA's) is **9354.23** and the median of the total number of steps taken per day is **10395**.


##Differences in activity patterns between weekdays and weekends

First, we create the factor variable for weekday and weekend:

```r
data2 <- data2 %>%
    mutate(day_name = weekdays(date_format),
           day_type = case_when(day_name=="sábado" | day_name=="domingo" ~ "Weekend", TRUE ~ "Weekday"))
```

Next we plot the time series of the 5-minute interval and the average number of steps taken, average across all weekday days or weekend day.


```r
data2 <- data2 %>%
    group_by(interval, day_type) %>%
    mutate(steps_by_day_type = sum(steps_no_na, na.rm = TRUE)) %>%
    ungroup()

xyplot(steps_by_day_type ~ interval | day_type, data2, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Average number of steps")
```

![](Reproducible-Research---Course-Project-1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->





