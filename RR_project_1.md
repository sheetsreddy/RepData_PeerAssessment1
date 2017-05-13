# RR_Project 1
Sheetal  
May 12, 2017  





```r
unzip("activity.zip")

# Question 1: Reading and processing data
data <- read.csv("activity.csv",stringsAsFactors=FALSE,na.strings="NA")

str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
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
# converting date variable into Date format
data$date <- as.Date(data$date,format="%Y-%m-%d")

# For the first few questions we will be excluding NAs and hence filtering data
newdata <- filter(data,complete.cases(data))

# Question 2 plotting a histogram of average number of steps taken in a day
newdatasum <- ddply(newdata,.(date),summarize,sum=sum(steps))

hist(newdatasum$sum,breaks=20,xlab="Number of steps taken by day",col="green",main="Histogram of number of steps taken by day")
```

![](RR_project_1_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
# Question 3 mean and median of number of steps taken each day
# mean
round(mean(newdatasum$sum))
```

```
## [1] 10766
```

```r
#median
round(median (newdatasum$sum))
```

```
## [1] 10765
```

```r
summary(newdatasum$sum)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

```r
#Question 4 time series plot of number of steps taken every 5 minutes

newdata <- newdata %>% group_by(interval) %>% arrange(interval) %>% mutate(mean=mean(steps))

newdata$mean <- as.integer(round(newdata$mean))

plot(newdata$interval,newdata$mean,type="l",col="black",xlab="Interval",ylab="Average number of steps",main="Time series plot of average number of steps taken by 5 min interval")
```

![](RR_project_1_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
# Question 5, time series interval that on average contains the maximum number of steps

x <- newdata[newdata$mean==max(newdata$mean),3]
unique(x)
```

```
## Source: local data frame [1 x 1]
## Groups: interval [1]
## 
##   interval
##      <int>
## 1      835
```

```r
# Question 6: Code to describe strategy for imputing
# For this question we will replace missing values i.e. NAs by a mean of steps for that 5-minute interval

data1 <- data %>% group_by(interval) %>% arrange(interval) %>% mutate(mean=mean(steps,na.rm=TRUE))

data1$mean <- as.integer(round(data1$mean))

data1$steps <- ifelse(is.na(data1$steps),data1$mean,data1$steps)



#Question 7: Histogram of imputed data

imputedata <- ddply(data1,.(date),summarize,sum=sum(steps))

hist(imputedata$sum,breaks=20,xlab="Number of steps taken by day",col="green",main="No of steps taken by day after replacing missing value with 5-minute mean")
```

![](RR_project_1_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
# mean of imputed data
round(mean(imputedata$sum))
```

```
## [1] 10766
```

```r
# median of imputed data

round(median(imputedata$sum))
```

```
## [1] 10762
```

```r
#summary of imputed data
summary(imputedata$sum)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10762   10766   12811   21194
```

```r
# As seen from the output, there is no difference in the means with and without imputing data. There is a difference in the median of the two outputs and the median of imputed data has decreased

#Question 8: Panel plot comparing average number of steps taken for 5 minute interval across weekdays and weekends

data1$day <- "weekday"

weekend <- weekdays(imputedata$date) %in% c("Saturday","Sunday")

data1$day[weekend == TRUE] <- "weekend"

xyplot(data1$mean ~ data1$interval | data1$day,type="l",layout=c(1,2),xlab="Time Intervals",ylab="Means",main="Plot of means of weekdays and weekends vs time intervals")
```

![](RR_project_1_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

