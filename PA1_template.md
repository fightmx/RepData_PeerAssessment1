---
title: "PA1_template.Rmd"
author: "RODRIGO JUAREZ"
date: "12 de marzo de 2017"
output: html_document
---

##Loading and preprocessing the data

1. Load the data

```{r}
data <- read.csv("activity.csv")
head(data)

```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```{r}
data$date <- as.Date(strptime(data$date, "%Y-%m-%d"))
```

##What is mean total number of steps taken per day?

####For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```{r}
totalStepsByDay <- with(data, tapply(steps, date, sum, na.rm = T))
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```{r}
hist(
  totalStepsByDay, 
  main = "Histogram", 
  xlab= "Total number of steps taken per day", 
  ylab = "Number of Days", 
  col = "red", 
  breaks = c(0, 2500, 5000, 7500, 10000, 
             12500, 15000, 17500, 20000, 
             22500, 25000))
```

3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
mean(totalStepsByDay)
median(totalStepsByDay)
```

##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
averageDailyActivity <- aggregate(steps ~ interval, data = data, FUN = mean)
plot(
  averageDailyActivity, 
  type = "l", 
  xlab = "5-minute Intervals", 
  ylab = "Number of steps (Average)", 
  main = "Average steps per day"
  )
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
totalDailyActivity <- aggregate(steps ~ interval, data = data, FUN = sum)
totalDailyActivity[totalDailyActivity$steps == max(totalDailyActivity),]
```

##Imputing missing values

####Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
sum(is.na(data$steps))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
filledData <- data
for(i in 1:nrow(filledData)){
  if(is.na(filledData[i,1])){
    filledData[i,1] <- averageDailyActivity[
        averageDailyActivity$interval == filledData[i,3], 2
        ]
  }
}

filledTotalStepsByDay <- 
  with(filledData, tapply(steps, date, sum, na.rm = T))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}

mean(filledTotalStepsByDay)
median(filledTotalStepsByDay)

hist(
  filledTotalStepsByDay, 
  main = "Imputed Histogram", 
  xlab= "Total number of steps taken per day", 
  ylab = "Number of Days", 
  col = "red", 
  breaks = c(0, 2500, 5000, 7500, 10000, 
             12500, 15000, 17500, 20000, 
             22500, 25000))

```

##Are there differences in activity patterns between weekdays and weekends?

####For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
library(timeDate)
filledData$day_of_week <- 
  ifelse(isWeekday(data$date) == TRUE, "weekday", "weekend")

```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
library(lattice)
xyplot(steps ~ interval | day_of_week, data=filledData, type="l", layout = c(1, 2))

```


