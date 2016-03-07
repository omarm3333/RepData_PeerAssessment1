# Set working directory
setwd("~/Documents/Cisco/6) DataScience/Coursera/DataScience Specialization/5. Reproducible Research/W1/Assignment")
require(dplyr)
require(knitr)

########################################################
# Loading and preprocessing the data
########################################################
activity <- read.csv("activity.csv")

# Format date
activity$date <- as.Date(activity$date, "%Y-%m-%d")

########################################################
# What is mean total number of steps taken per day?
########################################################
# Get Daily activity grouped and summirized, removing NAs
daily <- activity %>%
        group_by(date) %>%
        summarize(
                total=sum(steps, na.rm = TRUE), 
                mean=mean(steps, na.rm = TRUE),
                median=median(steps, na.rm = TRUE)
                ) %>%
        filter(complete.cases(.))

# Plot total daily step histogram
hist(daily$total,
     main = "Total Daily Steps per Interval",
     xlab = "Daily 5-min interval",
     ylab = "Total steps frequency"
     )

# Report Total, mean and median steps per day
par(mfrow = c(3, 1), mar = c(0, 4, 2, 1), cex.main=2)

with(daily, plot (date, total, type="l",
                  main = "Total, Mean and Median Daily Steps",
                  ylab = "Total steps",
                  xaxt="n"
                  )
     )
par(mar = c(0, 4, 0, 1))
with(daily, plot (date, mean, type="l",
                  ylab = "Average steps",
                  xaxt="n"
                  )
     )

par(mar = c(4, 4, 0, 1))

with(daily, plot (date, median, type="l",
                  ylab = "Median steps",
                  xlab = "Date"
                  )
     )

########################################################
# What is the average daily activity pattern?
########################################################

# Get the average steps per 5-minute interval
average <- activity             %>%
        group_by(interval)      %>%
        summarize(avg=mean(steps, na.rm = TRUE))

# Get the interval where average steps is maximum
maxInterval <- average[average$avg == max(average$avg),]$interval

# Report average steps per 5-minute interval
par(mfrow = c(1,1), mar = c(4,4,4,1))

with(average, plot (interval, avg, type="l",
                    main = "Average Steps per 5-min Interval",
                    ylab = "Average Steps",
                    xlab = "5-min Interval"
                    )
     )
abline(v=maxInterval, lwd = 2, col="green")

########################################################
# Imputing missing values
########################################################
# Total number of missing values (NAs)
missingValues <- sum(is.na(activity$steps))

# Days missing 8 days with missing values
activity %>% 
        group_by(date) %>% 
        filter(is.na(steps)) %>% 
        summarize(total_missing=n())

# Calculate the means for each interval
intervalAvg <- activity %>%
        group_by(date,interval) %>%
        summarize(intervalMean=mean(steps)) %>%
        
        # Add a Join key (date+interval) to append to master data
        mutate (keyInterval=paste(as.character(date), as.character(interval),sep="-"))

# Append previous day intervals' means column
intervalAvg$prevDayIntervalMean <- 
        c(rep(NA,288), intervalAvg[1:(nrow(intervalAvg)-288),]$intervalMean)

# Append next day intervals' means column
intervalAvg$nextDayIntervalMean <- 
        c(intervalAvg[289:nrow(intervalAvg),]$intervalMean,rep(NA,288))

# Create values to impute plus join key (date+interval)
impute <- activity %>%
        mutate(keyInterval=paste(as.character(date), as.character(interval),sep="-")
               ) %>%
        
        # Left join interval's means
        left_join(intervalAvg, by="keyInterval")        

# Impute NA steps first with previous day interval means
impute[is.na(impute$steps),]$steps <- 
        impute[is.na(impute$steps),]$prevDayIntervalMean

# Impute NA steps secondly with next day interval means
impute[is.na(impute$steps),]$steps <- 
        impute[is.na(impute$steps),]$nextDayIntervalMean

# Clean up new dataset
impute <- select(impute, date=date.x, interval=interval.x, steps)

# Calculate total, mean and median values
dailyI <- impute %>%
        group_by(date) %>%
        summarize(
                total=sum(steps), 
                mean=mean(steps),
                median=median(steps)
        )

# Plot histogram of daily steps with old and new dataset
par(mfrow = c(1,2))

par(mar = c(4,4,4,1))
hist(daily$total,
     main = "Original Activity Data",
     xlab = "5-min Interval",
     ylab = "Frequency"
     )

par(mar = c(4,1,4,4))
hist(dailyI$total,
     main = "Imputed Activity Data",
     xlab = "5-min Interval",
     ylab = NULL,
     yaxt = "n"
     )

# Report Total, mean and median steps per day
par(mfrow = c(3, 2))

par(mar = c(0,4,2,1))
with(daily, plot (date, total, type="l",
                  main = "Original Data",
                  xaxt = "n",
                  ylab = "Total Steps"                  
                  )
     )
par(mar=c(0,0,2,1))
with(dailyI, plot (date, total, type="l",
                  main = "Imputed Data",
                  xaxt="n",
                  yaxt="n"
                )
        )

par(mar = c(0,4,0,1))
with(daily, plot (date, mean, type="l",
                  xaxt = "n",
                  ylab = "Mean Steps"                  
                  )
     )

par(mar=c(0,0,0,1))
with(dailyI, plot (date, mean, type="l",
                   xaxt="n",
                   yaxt="n"
                   )
     )

par(mar = c(4,4,0,1))
with(daily, plot (date, median, type="l",
                  ylab = "Median Steps",
                  xlab = "Date"
                  )
     )

par(mar=c(4,0,0,1))
with(dailyI, plot (date, median, type="l",
                   yaxt="n",
                   xlab = "Date"
                   )
     )

############################################################################
# Are there differences in activity patterns between weekdays and weekends?
############################################################################

daysPattern <- impute                   %>%
        mutate(day=as.factor(ifelse(weekdays(date) == "Saturday" | weekdays(date) == "Sunday",
                              "weekend",
                              "weekday"))
               )                        %>%
        group_by(day, interval)         %>%
        summarize(mean=mean(steps))

# Plot week patterns
par(mfrow = c(2, 1))

par(mar = c(0, 4, 2, 1))
with(daysPattern[daysPattern$day == "weekend",], 
     plot(interval, mean, type = "l",
          main = "Weekend",
          xaxt="n",
          ylab = "Mean Steps",
          ylim=c(0, 200)
          )
     )

par(mar=c(4,4,2,1))
with(daysPattern[daysPattern$day == "weekday",],  
     plot(interval, mean, type = "l",
          main = "Weekday",
          ylab = "Mean Steps",
          xlab = "5-min Interval",
          ylim=c(0, 200)
          )
     )

