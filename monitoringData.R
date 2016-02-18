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



## ====================================================================
## [1]. Loading and preprocessing the data.
## ====================================================================

# Read the raw data from files - located in the "data" folder
# Use stringsAsFactors = FALSE to read data without the Levels
# --------------------------------------
origData <- read.csv(file = "./data/activity.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)


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
origData$day <- as.factor(format(origData$date, '%d'))


## ====================================================================
## [2]. Compute mean total number of steps taken per day.
## ====================================================================

# Calculate the total number of steps taken per day
synthData <- ddply(origData, .(date), summarize
                   , mean = round(mean(steps))
                   , maximum = max(steps)
                   , total=sum(steps)
                   )
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

## ====================================================================
## [3]. Average daily activity pattern.
## ====================================================================

intervData <- subset( origData, select = -day )

intervData <- ddply(intervData, .(interval), transform, days = length(date), average = 0)
test <- aggregate(steps ~ interval + days, data = intervData, FUN = sum)
test$average <- test$steps/test$days

unique(within(intervData, {
        steps <- ave(steps, interval, FUN = sum)
        average <- ave(average, interval, FUN = mean)
}))


## ====================================================================
## [4]. Imputing missing values.
## ====================================================================

# Simply count the NAs for one column
# ------------------------------------------
missingNA <- sum(is.na(origData$steps))
isNA <- is.na(origData$steps)


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

orig <- origData[which(origData$interval == '5'),]

# Use the  mean for the 5-minute interval to fill in the missing values
imputedData <- ddply(origData, .(interval), function(df) {
        df$steps[is.na(df$steps)] <- round(mean(df$steps, na.rm=TRUE))
        return(df)
        })

imputedData[which(imputedData$day == '01'),]
str(imputedData)


# Calculate the total number of steps taken per day
synth <- ddply(imputedData, .(date), summarize
                   , mean = round(mean(steps))
                   , maximum = max(steps)
                   , total=sum(steps)
)

png("histImputedData.png", width = 480, height = 480)

hist(synth$total,
     main="Histogram ~ Total steps per day",
     breaks=seq(min(synth$total), max(synth$total), by=((max(synth$total) - min(synth$total))/6)),
     xlab="Number of steps",
     border="lightcoral",
     col="lightblue2",
     xaxt='n'
)
axis(side=1, at=seq(0,max(synth$total),2000), labels=seq(0,max(synth$total),2000))

dev.off()

## ====================================================================
## [5]. Differences in activity patterns between weekdays and weekends.
## ====================================================================



