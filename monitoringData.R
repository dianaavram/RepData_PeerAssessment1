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


## Libraries | Source scripts
library(plyr)
source("path.R")



## ====================================================================
## [1]. Read the raw data from files
##      - located in the "data" folder
## ====================================================================
origData <- read.csv(file = "./data/activity.csv", header=TRUE, sep=",")
head(origData)

# Convert the list into a data.frame
activData <- do.call(cbind.data.frame, aux)

quantile(restData$councilDistrict, na.rm = TRUE)



