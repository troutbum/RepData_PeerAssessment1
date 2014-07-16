# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

1. Read in activity monitoring dataset.

Set working directory:

```r
setwd("~/CourseraHW/RepData_PeerAssessment1")
```
Create a data subdirectory if it does not exist:

```r
if (!file.exists("data")) {
        dir.create("data")
}
```
Download file and unzip if it isn't already there:

```r
filePath <- "./data/"
fileZipName <- "repdata-data-activity.zip"
fileName <- "activity.csv"
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

ZipFile <- paste0(filePath, fileZipName)        # path to zipped data file
if (!file.exists(ZipFile)) {
        download.file(fileUrl, destfile = ZipFile, method ="curl")
        unzip(ZipFile, exdir=filePath)
        dateDownloaded <- date()
}
raw <- read.csv(paste0(filePath, fileName),colClasses = c("integer", "character", "integer"))
```

2. Process/transform the data into a format suitable for analysis

```r
clean <- na.omit(raw)
clean$date <- as.factor(clean$date)
clean$interval <- as.factor(clean$interval)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

```r
dailysteps <- with(clean, tapply(steps, date, sum))
myhist <- hist(dailysteps, xlab='steps', main='Histogram of Daily Steps')
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day

```r
echo = TRUE
cat("Mean Number of Daily Steps = ", mean(dailysteps))
```

```
## Mean Number of Daily Steps =  10766
```

```r
cat("Median Number of Daily Step =", median(dailysteps))
```

```
## Median Number of Daily Step = 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervalsteps <- with(clean, tapply(steps, interval, mean))
times <- as.integer(rownames(intervalsteps))
avgdaily <- cbind(times,intervalsteps)

myplot <- plot(avgdaily, type="l", 
           xlab='Time interval', ylab='Number of Steps',
           main='Average Number of Steps')
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
echo = TRUE
row <- which(avgdaily[,"intervalsteps"]==max(avgdaily[,"intervalsteps"]))
cat("The 5-minute Interval with the Maximum Steps is: ", avgdaily[row,"times"])
```

```
## The 5-minute Interval with the Maximum Steps is:  835
```

```r
cat("The Maximum number of steps in this interval =", max(avgdaily[,"intervalsteps"]))
```

```
## The Maximum number of steps in this interval = 206.2
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

```r
echo = TRUE
cat("Total number of rows with NAs =", (nrow(raw) - nrow(na.omit(raw))))
```

```
## Total number of rows with NAs = 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
echo = TRUE
cat("My strategy is to replace all missing values with the mean value for the corresponding 5-minute interval")
```

```
## My strategy is to replace all missing values with the mean value for the corresponding 5-minute interval
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
fixed <- raw
for (i in 1:nrow(raw)) {
        if (is.na(raw[i,"steps"])) {
                x <- raw[i,"interval"]
                filler <- avgdaily[which(avgdaily[,"times"] == x), "intervalsteps"]
                fixed[i, "steps"] <- filler
        }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
fdailysteps <- with(fixed, tapply(steps, date, sum))
myhist2 <- hist(fdailysteps, xlab='steps', main='Histogram of Daily Steps with Missing Values Estimated')
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

```r
cat("Mean Number of Daily Steps (with missing values estimated) = ", mean(fdailysteps))
```

```
## Mean Number of Daily Steps (with missing values estimated) =  10766
```

```r
cat("Median Number of Daily Step (with missing values estimated) =", median(fdailysteps))
```

```
## Median Number of Daily Step (with missing values estimated) = 10766
```


## Are there differences in activity patterns between weekdays and weekends?
