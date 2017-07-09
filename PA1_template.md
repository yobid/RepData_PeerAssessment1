# Reproducible Research: Peer Assessment 1
'''{r setoptions, echo=FALSE}
opts_chunkset(echo=TRUE, results="asis")
'''
## Loading and preprocessing the data
As the data is in our working directory we will read it and preprocess it.  
‘‘‘{r preprocess echo=TRUE}  
unzip("activity.zip")  
data <- read.csv("activity.csv")  
‘‘‘   

## What is mean total number of steps taken per day?
- Calculate the total number of steps per day  
'''{r daymean step}  
library(dplyr)  
dataDate <- group_by(data, date)  
sum <- summarize(dataDate, steps=sum(steps, na.rm=TRUE))  
'''  

- Histogram of the total number of steps per day
'''{r histogram steps}  
library(ggplot2)  
with(sum, qplot(steps, binwidth=2500, main="Total number of steps per day"))  
'''

- Mean and median of total number of steps per day  
'''{r steps}  
summary(sum$steps)  
'''  

## What is the average daily activity pattern?
Time series plot of the average steps for 5 min interval for each day.  
'''{r daily activity}  
mean <- summarize(dataDate, steps=mean(steps, na.rm=TRUE))  
with(mean, plot(date, steps, main="average daily 5 min interval steps over time"))  
with(mean, lines(date, steps))  
'''  

Maximum number of steps, by daily average :   
'''{r steps max}  
filter(mean, steps==max(steps, na.rm=TRUE))  
'''  

## Imputing missing values
Number of missing values in the data, will appear in the TRUE column  
'''{r NAs}  
table(!complete.cases(data))  
'''  

Function to replace the NA with the mean of steps, and create a new dataset : cleanData with no NAs.  
'''{r functionNA}  
cleanData <- data  
temp <- mean(mean$steps, na.rm=T)  
cleanData[is.na(cleanData$steps),1] <- temp  
'''  

Histogram of the total number of steps taken each day  
'''{r total steps}  
cleanDate <- group_by(cleanData, date)  
cleanSum <- summarize(cleanDate, steps=sum(steps, na.rm=TRUE))  
with(cleanSum, qplot(steps, binwidth=2500, main="Total number of steps per day"))  
'''  
The plot differs, we don't have the peak on the 0.  

Mean and median of the total steps.  
'''{r steps summary}  
summary(cleanSum$steps)  
'''  
The median and the mean differs slightly.  

## Are there differences in activity patterns between weekdays and weekends?
Create the factor variable wDays and plot the mean steps per interval for each day in a two panel weekday vs weekend plot.  
'''{r weekdays}  
library(lubridate)  
cleanData$day <- wday(as.Date(cleanData$date))  
weekdays <- 2:6  
cleanData$wDay <- factor(cleanData$day %in% weekdays, levels=c(TRUE, FALSE), labels=c("weekday", "weekend"))  
dData <- group_by(cleanData, wDay, date)  
mean2 <- summarize(dData, steps=mean(steps))  
mean2$date <- as.Date(as.character(mean2$date))  
xyplot(steps~date|wDay, mean2, layout=c(1,2), type="l")  
'''  
