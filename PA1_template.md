---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```{r reding data, message=FALSE, warning=FALSE}
library(dplyr)
library(ggplot2)
library(lubridate)
dd <- read.csv(unz("activity.zip", "activity.csv"))
dd$date <- ymd(dd$date)
```

## 2. Histogram of the total number of steps taken each day

```{r hist, message=FALSE}

daily_steps <- dd %>% group_by(date) %>% summarise(steps = sum(steps, na.rm=TRUE), average=mean(steps, na.rm=TRUE)) 
hist(daily_steps$steps,col="lightblue",  main="Number of steps per day", xlab="Steps")

```


## What is mean total number of steps taken per day?
## 3. Mean and median total number of steps taken per day

Mean number of steps per day:
```{r }
mean(daily_steps$steps, na.rm = TRUE)
```
Median number of steps per day:
```{r }
median(daily_steps$steps, na.rm = TRUE )
```


# What is the average daily activity pattern?
## 4. Time series plot of the average number os steps taken
```{r}
ggplot(daily_steps, aes(x= date, y= average))+
    geom_line()+labs(x="Date",y= "Average steps taken by day")
```

## 5. The 5-minute interval that, on average, contains the maximum number of steps
```{r}
dd %>% filter(interval==5) %>%
    filter(steps == max(steps, na.rm=TRUE))

```


# Imputing missing values
## 6. Code to describe and show a strategy for imputing missing data

Imputation using the mean of each day
Total number of missing values `r sum(is.na(dd$steps))`
```{r}
dd$stepsImp <- ifelse(is.na(dd$steps), mean(dd$steps, na.rm=TRUE),dd$steps) 
```

## 7. Histogram of the total number of steps taken each day after missing values are imputed
```{r message=FALSE}
daily_steps <- dd %>% group_by(date) %>% summarise(steps = sum(stepsImp, na.rm=TRUE), average=mean(stepsImp, na.rm=TRUE)) 
hist(daily_steps$steps,col="lightblue",  main="Number of steps per day with Imputed data", xlab="Steps")

```


# Are there differences in activity patterns between weekdays and weekends?
## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r}
dd$dateDay <- wday(dd$date, label = T)
dd$dateDayWeekend <- ifelse(dd$dateDay == "Sun" | dd$dateDay == "Sat", "Weekend", "Week")


stepsweekend <-  aggregate(steps ~ interval + dateDayWeekend, data = dd, mean)

library(lattice)
xyplot(steps~interval|factor(dateDayWeekend),data=stepsweekend,aspect=1/2,type="l")

```

































