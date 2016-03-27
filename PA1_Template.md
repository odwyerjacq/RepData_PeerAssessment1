---
title: "Reproducible Research Project 1"
author: "Jackie O'Dwyer"
date: "Saturday, March 26, 2016"
output: html_document
---

## Load data from working directory
```{r echo = TRUE}
activity <- read.csv("activity.csv", header = TRUE)
```

Clean date from factor to date
```{r echo = TRUE}
activity$date <- as.Date(activity$date)
```

## What is the mean and total steps per day?
Sum steps by day, create histogram, and get mean and median per day
```{r}
library(dplyr)
DailySteps <- activity  %>% 
    group_by(date) %>%
    summarise(total_steps=sum(steps,na.rm=TRUE),na=mean(is.na(steps))) 

hist(DailySteps$total_steps,col="blue",main="Total Steps per Day",xlab="Total Steps")

mean(DailySteps$total_steps, na.rm = TRUE)
median(DailySteps$total_steps, na.rm = TRUE)

```
The mean per day is 9354.23
The median per day is 10395

## What is the average daily activity pattern?
``` {r}
library(ggplot2)
interval <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))

ggplot(interval, aes(x=interval, y=steps)) + geom_line(color = "blue")
interval[which.max(interval$steps),]
```
Interval 835 on average contains the maximum number of steps

##Imputing Missing Values
``` {r}
sum(is.na(activity$steps))

activity2 <- activity
naValues <- is.na(activity2$steps)
avgInterval <- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE, simplify=TRUE)
activity2$steps[naValues] <- avgInterval[as.character(activity2$interval[naValues])]


DailySteps2 <- activity2  %>% 
    group_by(date) %>%
    summarise(total_steps=sum(steps,na.rm=TRUE),na=mean(is.na(steps))) 
    
hist(DailySteps2$total_steps,col="blue",main="Total Steps per Day",xlab="Total Steps")

mean(DailySteps2$total_steps, na.rm = TRUE)
 
median(DailySteps2$total_steps, na.rm = TRUE)
```
2304 total missing values in the dataset
10766.19 is the new mean
10766.19 is the new median 
Both mean and median increased, and are now equal to each other.

## Are there differences in activity patterns between weekdays and weekends

``` {r}
activity2 <- mutate(activity2, weekday = ifelse(weekdays(activity2$date) == "Saturday" | weekdays(activity2$date) == "Sunday", "weekend", "weekday"))
activity2$weekday <- as.factor(activity2$weekday)

Intervals <- activity2 %>%
  group_by(interval, weekday) %>%
  summarise(steps = mean(steps))

PanelPlot <- ggplot(Intervals, aes(x=interval, y=steps, color = weekday)) +
  geom_line() +
  facet_wrap(~weekday, ncol = 1, nrow=2)
print(PanelPlot)
```