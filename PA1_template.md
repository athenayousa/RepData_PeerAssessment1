coursera-reproducible-research-assignment1
==========================================

Loading and preprocessing the data
----------------------------------

#### 1.load the data

    unzip("./activity.zip")
    data <- read.csv("./activity.csv")

What is mean total number of steps taken per day?
-------------------------------------------------

    steps_perday <- aggregate(steps~date,data,sum,na.rm=TRUE)

#### 1.make a histogram of the total number of the steps taken each day

    hist(steps_perday$steps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)
\#\#\#\# 2.Calculate and report the mean and median total number of
steps taken per day

    mean_steps_perday <- mean(steps_perday$steps)
    median_steps_perday <- median(steps_perday$steps)

-   Mean: 1.076618910^{4}
-   Median: 10765

What is the average daily activity pattern?
-------------------------------------------

    steps_perinterval <- aggregate(steps~interval,data,mean,na.rm=TRUE)

#### 1.make a time series plot

    plot(steps~interval,steps_perinterval,type="l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)
\#\#\#\# 2.Which 5-minute interval, on average across all the days in
the dataset, contains the maximum number of steps?

    max_step_interbal <- steps_perinterval[which.max(steps_perinterval$steps),]$interval

-   Most steps at: 835

Imputing missing values
-----------------------

##### 1. Calculate and report the total number of missing values in the dataset

    totalValuesMissings <- sum(is.na(data$steps))

-   Number of missing values: 2304

##### 2. Devise a strategy for filling in all of the missing values in the dataset.

we’ll fill in all the missing values in the dataset with the mean per
interval. Here’s the function that will return, for a particular
interval, the mean value

    getMeanStepsPerInterval<-function(interval){
        steps_perinterval[steps_perinterval$interval==interval,]$steps
    }

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

    data_noNA<-data
    for(i in 1:nrow(data)){
        if(is.na(data[i,]$steps)){
            data_noNA[i,]$steps <- getMeanStepsPerInterval(data_noNA[i,]$interval)
        }
    }

##### 4. Make a histogram of the total number of steps taken each day

    data_noNA_sum <- aggregate(steps ~ date, data=data_noNA, sum)
    hist(data_noNA$steps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)
\#\#\#\#\# … and Calculate and report the mean and median total number
of steps taken per day.

    mean <- mean(data_noNA_sum$steps)
    median <- median(data_noNA_sum$steps)

-   Mean: 1.076618910^{4}
-   median: 1.076618910^{4}

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    library(ggplot2)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    day_of_week <- data_noNA %>%
      mutate(
        date = ymd(date),
        weekday_or_weekend = case_when(wday(date) %in% 2:6 ~ "Weekday",
                                       wday(date) %in% c(1,7) ~ "Weekend")
      ) %>% select(-date) %>%
      group_by(interval, weekday_or_weekend) %>%
      summarise(
        steps = mean(steps)
      )

    ## `summarise()` regrouping output by 'interval' (override with `.groups` argument)

##### 2. Make a panel plot containing a time series plot

    ggplot(day_of_week, aes(interval, steps)) + 
      geom_line() + 
      facet_wrap(~weekday_or_weekend, nrow = 2) +
      xlab("5-Minute intervals") + 
      ylab("Average number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-15-1.png)
