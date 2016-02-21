## ============================================================================
## CREATE REPRODUCIBLE RESEARCH
## ============================================================================
## The purpose of the analysis is to perform some analysis and to create a
## a "clean" methodology in order to obtain the same results.
## All the analysis is done by the code contained in the script monitoringData.R.
## ----------------------------------------------------------------------------
## The script consists of the following steps:
##
##      [1]. Loading and preprocessing the data.
##      [2]. Compute mean total number of steps taken per day.
##      [3]. Average daily activity pattern.
##      [4]. Imputing missing values.
##      [5]. Differences in activity patterns between weekdays and weekends.
##
## ============================================================================


## Set the working directory and create other necessary folders
##  =============================================================
path.dir <- getwd()

if (!is.null(path.dir)){
    path.dir <- paste(path.dir, "RepData_PeerAssessment1", sep = "/");
    setwd(path.dir)
}

## Source scripts | Load libraries
if (!require("plyr")) {
    install.packages("plyr")
    library(plyr)
    }

if (!require("ggplot2")) {
        install.packages("ggplot2")
        library(plyr)
}

if (!require("lubridate")) {
        install.packages("lubridate")
        library(plyr)
}

if (!require("chron")) {
        install.packages("chron")
        library(plyr)
}

if (!require("knitr")) {
        install.packages("knitr")
        library(plyr)
}

## ====================================================================
## [1]. Loading and preprocessing the data.
## ====================================================================

# Loading the raw data from files - located in the "data" folder
# Use stringsAsFactors = FALSE to read data without the Levels
# --------------------------------------
origData <- read.csv(file = "./data/activity.csv",
                     header=TRUE, sep=",", stringsAsFactors = FALSE)


# Analyse the data from the csv
# --------------------------------------
head(origData)
#   steps       date interval
# 1    NA 2012-10-01        0
# 2    NA 2012-10-01        5
# 3    NA 2012-10-01       10
# 4    NA 2012-10-01       15
# 5    NA 2012-10-01       20
# 6    NA 2012-10-01       25

str(origData)
# 'data.frame':	17568 obs. of  3 variables:
# $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
# $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

size <- dim(origData)
# [1] 17568     3



## Transform data to be suitable for the analysis
## ----------------------------------------------
# Transform 'date' to date format
origData$date <-as.Date(origData$date)

# Add column 'day' as factor
#origData$day <- as.factor(format(origData$date, '%d'))


## ====================================================================
## [2]. Compute mean total number of steps taken per day.
## ====================================================================

# Calculate the total number of steps taken per day
synthData <- ddply(origData, .(date), summarize,
                   total = sum(steps),
                   mean = round(mean(steps)),
                   median = median(steps),
                   maximum = max(steps))
summary(synthData)

# complete cases of data ~ eliminate "NA"
synthData <- synthData[complete.cases(synthData),]

# Histogram of total # of steps/day -  creating a histogram and saving in .png format
png("histStepsbyDay.png", width = 480, height = 480)

hist(synthData$total,
     main="Histogram ~ Total steps per day",
     breaks=seq(min(synthData$total), max(synthData$total), by=((max(synthData$total) - min(synthData$total))/8)),
     xlab="Number of steps",
     border="lightcoral",
     col="lightblue2",
     xaxt='n'
     )
axis(side=1, at=seq(0,max(synthData$total),2000), labels=seq(0,max(synthData$total),2000))

dev.off()

qplot(synthData$total,
      geom="histogram",
      binwidth = 1000,
      main = "Histogram for Total Steps",
      xlab = "Total steps per day",
      fill=I("blue"),
      col=I("red"),
      alpha=I(.2),
      xlim=c(min(synthData$total),max(synthData$total)))

ggplot(data=synthData, aes(synthData$total)) +
        geom_histogram(breaks=seq(min(synthData$total), max(synthData$total), by=((max(synthData$total) - min(synthData$total))/8)),
                       col="red",
                       fill="blue",
                       alpha = .2) +
        labs(title="Histogram for Total Steps") +
        labs(x="Age", y="Count") +
        xlim(c(min(synthData$total),max(synthData$total))) +
        ylim(c(0,20))


png("instructions_fig/hist_StepsPerDay.png", width = 480, height = 480)
ggplot(data=synthData, aes(synthData$total)) +
        geom_histogram(breaks=seq(0, 25000, by=1700),
                       col="red", aes(fill=..count..)) +
        labs(x = "Steps per day", y = "Count") +
        labs(title="Histogram for Mean total steps per day")
dev.off()


## ====================================================================
## [3]. Average daily activity pattern.
## ====================================================================

aveInterval <- aggregate(steps ~ interval, data = origData, FUN = mean)

# Transform interval to "Time" representation
dt <- sprintf("%04d", aveInterval$interval); dt
dt <- strptime(dt, format="%H%M"); dt


aveInterval$time <- strptime(sprintf("%04d", aveInterval$interval),
                             format="%H%M")
aveInterval$time <- as.POSIXct(aveInterval$time, format="%H:%M:%S")

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
maxSteps <- max(aveInterval$steps);
idx <- which(aveInterval$steps==maxSteps, arr.ind=TRUE); idx
minsVal <- aveInterval$interval[idx]; minsVal

# Create the time series plot and same as 'png'
text <- paste("(",minsVal,",",round(maxSteps),")")
ggplot(aveInterval, aes(interval, steps)) +
        geom_line(colour="#CC0000") +
        #scale_x_date(date_labels = "%H:maxSteps%M") +
        #scale_x_continuous(breaks=1:10)+
        xlim(0, 2500)+
        xlab("Minutes") +
        ylab("Average daily steps")+
        geom_hline(yintercept = maxSteps)+
        geom_point(aes(x=minsVal, y=maxSteps), colour="blue", size =1.5) +
        geom_text(aes(label = text, x = minsVal + 20, y=maxSteps-2,
                      vjust = "inward", hjust = "inward"))
        #geom_abline(aes(intercept=max(aveInterval$steps), slope=90),colour = "blue", size = 1)
ggsave("instructions_fig/plot_TimeSeries_5min.png", width = 2.5, height = 1)

## ====================================================================
## [4]. Imputing missing values.
## ====================================================================

# Simply count the NAs for one column
# ------------------------------------------
missingNA <- sum(is.na(origData$steps))
isNA <- is.na(origData$steps)
str(summary(origData))

# Count of NAs values for each column
# ------------------------------------------
na_count <-sapply(origData, function(x) sum(length(which(is.na(x)))))
na_count <- data.frame(na_count)
na_count
# steps        2304
# date            0
# interval        0

# Create a new dataset with missing data filled in
# ------------------------------------------

# Use the  mean for the 5-minute interval to fill in the missing values
imputedData <- ddply(origData, .(interval), function(df) {
        df$steps[is.na(df$steps)] <- round(mean(df$steps, na.rm=TRUE))
        return(df)
        })
head(imputedData)


# Calculate the total number of steps taken per day
synth <- ddply(imputedData, .(date), summarize, total=sum(steps),
               mean = round(mean(steps)),
               median = median(steps),
               maximum = max(steps))

ggplot(data=synth, aes(synth$total)) +
        geom_histogram(breaks=seq(0, 25000, by=1500),
                       col="red",
                       aes(fill=..count..)) +
        labs(x = "Steps per day", y = "Count") +
        labs(title="Histogram for Total steps per day")
ggsave("instructions_fig/hist_StepsPerDays_imputedNA.png", width = 2.5, height = 1)

## ====================================================================
## [5]. Differences in activity patterns between weekdays and weekends.
## ====================================================================
# Determine type of day - weekend | weekday
orig <- imputedData
idx <- chron::is.weekend(orig$date)
orig$daytype <- chron::is.weekend(orig$date)

orig$daytype[idx] <- 'weekend'
orig$daytype[!idx] <- 'weekday'
orig$daytype <- as.factor(orig$daytype)

# Divide the dataset by type of day in two datasets
dat <- split(orig, orig$daytype)

# Calculate the average number of steps taken, averaged across all weekday days
orig_wday <- as.data.frame(dat[1]); names(orig_wday) <- names(orig)
orig_wday_ave <- aggregate(steps ~ interval+daytype, data = orig_wday, FUN = mean)

# Calculate the average number of steps taken, averaged across all weekend days
orig_wend <- as.data.frame(dat[2]); names(orig_wend) <- names(orig)
orig_wend_ave <- aggregate(steps ~ interval+daytype, data = orig_wend, FUN = mean)

# Put together the resulting datasets with the average values
dat <- rbind(orig_wday_ave,orig_wend_ave)

# See the mean, median and maximum values by day type
summ <- ddply(dat, .(daytype), summarize,
               mean = round(mean(steps)),
               median = median(steps),
               maximum = max(steps))

# Panel plot of the datasets
ggplot(dat, aes(x = interval, y = steps, colour = days)) +
        geom_line(colour="#CC0000") +
        xlim(0, 2500) +
        xlab("Minutes") +
        ylab("Average daily steps") +
        facet_grid(facets = daytype ~ .)
ggsave("instructions_fig/plot_TimeSeries_wdays.png")
       #, width = 2.5, height = 1)






aveInterval <- aggregate(steps ~ interval, data = origData, FUN = mean)

# Transform interval to "Time" representation
dt <- sprintf("%04d", aveInterval$interval); dt
dt <- strptime(dt, format="%H%M"); dt


aveInterval$time <- strptime(sprintf("%04d", aveInterval$interval),
                             format="%H%M")
aveInterval$time <- as.POSIXct(aveInterval$time, format="%H:%M:%S")

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
maxSteps <- max(aveInterval$steps);
idx <- which(aveInterval$steps==maxSteps, arr.ind=TRUE); idx
minsVal <- aveInterval$interval[idx]; minsVal

# Create the time series plot and same as 'png'
text <- paste("(",minsVal,",",round(maxSteps),")")
ggplot(aveInterval, aes(interval, steps)) +
        geom_line(colour="#CC0000") +
        #scale_x_date(date_labels = "%H:maxSteps%M") +
        #scale_x_continuous(breaks=1:10)+
        xlim(0, 2500)+
        xlab("Minutes") +
        ylab("Average daily steps")+
        geom_hline(yintercept = maxSteps)+
        geom_point(aes(x=minsVal, y=maxSteps), colour="blue", size =1.5) +
        geom_text(aes(label = text, x = minsVal + 20, y=maxSteps-2,
                      vjust = "inward", hjust = "inward"))
#geom_abline(aes(intercept=max(aveInterval$steps), slope=90),colour = "blue", size = 1)
ggsave("instructions_fig/plot_TimeSeries_5min.png", width = 2.5, height = 1)
