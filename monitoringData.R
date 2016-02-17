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

# Count of NA values
na_count <-sapply(origData, function(x) sum(length(which(is.na(x)))))
na_count <- data.frame(na_count)
na_count
# steps        2304
# date            0
# interval        0


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
synthData <- ddply(origData, .(day), summarise
                   , mean = round(mean(steps))
                   , median = round(median(steps))
                   , maximum = max(steps)
                   , total=sum(steps)
                   )
summary(synthData)

# complete cases of data ~ eliminate "NA"
aux <- synthData[complete.cases(synthData),]

# Histogram of total # of steps/day -  creating a histogram and saving in .png format
png("histStepsbyDay.png", width = 480, height = 480)

hist(aux$total,
     main="Histogram ~ Total steps per day",
     breaks=seq(min(aux$total), max(aux$total), by=((max(aux$total) - min(aux$total))/6)),
     xlab="Number of steps",
     border="lightcoral",
     col="lightblue2",
     xaxt='n'
     )
axis(side=1, at=seq(0,max(aux$total),2000), labels=seq(0,max(aux$total),2000))

dev.off()


## ====================================================================
## [3]. Average daily activity pattern.
## ====================================================================


