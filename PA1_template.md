# Reproducible Research: Peer Assessment 1

 
 ```r
 library(plyr)
 library(lattice)
 ```
 
 ```r
 ## Loading and preprocessing the data
 ```
 
 ```r
 data <- read.csv("activity.csv", header = TRUE)
 cleaned <- data[which(data$steps != "NA"), ]
 ```

 ## What is mean total number of steps taken per day?
 
 ```r
 
 daily <- ddply(cleaned, .(date), summarise, steps = sum(steps))
 hist(daily$steps, main = "Number of Steps", xlab = "Total number of steps taken each day", 
     col = "green")
 ```
 
 ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
 
 ```r
 
 mn <- mean(daily$steps)
 md <- median(daily$steps)
 ```

 The mean is 1.0766 &times; 10<sup>4</sup> and the median is 10765
 
 ## What is the average daily activity pattern?
 
 ```r
 avgdaily <- ddply(cleaned, .(interval), summarise, steps = mean(steps))
 plot(avgdaily$interval, avgdaily$steps, type = "l", col = "red", xlab = "5-minute interval", 
     ylab = "Average number of steps taken", main = "Average daily activity pattern")
 ```
 
 ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
 
 ```r
 
 ```

 ## Entering missing values
 
 ```r
 
 # Total number of missing values in the dataset
 
 missing <- sum(is.na(data$steps))
 ```

 
 The total number of missing values is 2304
 
 ```r
 
 # Which 5-minute interval, on average across all the days in the dataset,
 # contains the maximum number of steps?
 
 avgdaily[avgdaily$steps == max(avgdaily$steps), ]
 ```
 
 ```
 ##     interval steps
 ## 104      835 206.2
 ```
 
 ```r
 colnames(avgdaily)[2] <- "avgint"
 merged <- arrange(join(data, avgdaily), interval)
 ```
 
 ```
 ## Joining by: interval
 ```
 
 ```r
 
 # Create a new dataset that is equal to the original dataset but with the
 # missing data filled in.
 
 merged$steps[is.na(merged$steps)] <- merged$avgint[is.na(merged$steps)]
 
 # Histogram
 
 dailynew <- ddply(merged, .(date), summarise, steps = sum(steps))
 hist(dailynew$steps, main = "Number of Steps", xlab = "Total number of steps taken each day", 
     col = "green", )
 ```
 
 ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 
 
 ```r
 
 # mean and median total number of steps taken per day
 
 mnnew <- mean(dailynew$steps)
 mdnew <- median(dailynew$steps)
 ```

 The new mean is 1.0766 &times; 10<sup>4</sup> and the median is 1.0766 &times; 10<sup>4</sup>
 
 ## Are there differences in activity patterns between weekdays and weekends?
 
 ```r
 
 weekdays <- weekdays(as.Date(merged$date))
 wddata <- transform(merged, day = weekdays)
 wddata$wk <- ifelse(wddata$day %in% c("Saturday", "Sunday"), "weekend", "weekday")
 avgwk <- ddply(wddata, .(interval, wk), summarise, steps = mean(steps))
 
 xyplot(steps ~ interval | wk, data = avgwk, layout = c(1, 2), type = "l", col = "red")
 ```
 
 ![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 
