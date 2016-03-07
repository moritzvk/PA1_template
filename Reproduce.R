# set working directory to the file where your download data is
# setwd("~/...data")

library(plyr)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)

# Read activity data
data <- read.csv("activity.csv", header = TRUE, sep = ",")

# Aggregate the number of steps per date
aggdata <- aggregate(steps ~ date,data=data,FUN="sum")
x <- hist(aggdata$steps, breaks = 100,labels = T, border = "dark blue",col = "light blue", main="Histogram steps/day")
rug(aggdata$steps)


aggmean <- aggregate(steps ~ date,data=data,FUN="mean")
hist(aggmean$steps, breaks = 100,labels = T,border = "dark blue",col = "light blue", main="Histogram mean/day")
rug(aggmean$steps)


aggmedian <- aggregate(steps ~ date,data=data,FUN="median")
print(aggmedian)


 # what is the average daily activity pattern? 
 # average steps per interval
intermean<- aggregate(steps~interval, data=data, FUN = "mean")
qplot(interval,steps, data=intermean, geom="line", xlim=c(0,2355),main="Average daily activity",xlab = "5 minutes interval (0 a.m.-23.55 p.m)", ylab = "average steps taken/interval")
i <- order(intermean$steps) # create order index
iorder <- intermean[i,] # create the ordered vector
print("Maximum number of steps across whole dataset")
print(top_n(iorder,1)) # choose top interval and steps (dplyr)

# Imputing missing values
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
print("Total number of missing values")
print(sum(is.na(data))) # count the total number of missing values

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

new <- data

for (i in 1:nrow(new)) {
        
        if (is.na(new$steps[i])== TRUE) {
                y <- subset(intermean,interval==new[i,3]) # ersetzt NAs durch den Mittelwert pro interval
                new[i,1] <- y[,2] 
                print(new[i,])
        }
}

# total number of steps taken per day 
newagg <- aggregate(steps ~ date,data=new,FUN="sum")
n <- hist(newagg$steps, breaks = 100,labels = T,border = "dark blue",col = "light blue", main="Histogram mean/day imputed data" )
rug(newagg$steps)
abline(v = median(newagg$steps), col = "magenta", lwd = 4)
abline(v = mean(newagg$steps), col = "green", lwd = 1)

# mean total number of steps taken per day
newmean <- aggregate(steps ~ date,data=new,FUN="mean")
# median total number of steps taken per day
newmedian <- aggregate(steps ~ date,data=new,FUN="median")

# Comparison of original data set and imputed data set (steps/day)
hist(aggdata$steps, breaks = 100,labels = T,border = "dark blue",col = "light blue", main="Histogram steps/day")
rug(aggdata$steps)
abline(v = median(aggdata$steps), col = "magenta", lwd = 4)
abline(v = mean(aggdata$steps), col = "green", lwd = 1)

hist(newagg$steps, breaks = 100,labels = T,border = "dark blue",col = "light blue", main="Histogram mean/day imputed data" )
rug(newagg$steps)
abline(v = median(newagg$steps), col = "magenta", lwd = 4)
abline(v = mean(newagg$steps), col = "green", lwd = 1)


#-----------------------------------------------------------
# Change to POSIXct format with lubridate package
new$date <- ymd(as.character(new$date))
# Add a facotr variable "day" to the dataset "new"
new$day<- weekdays(new$date,abbreviate = TRUE)
# Calculate the mean per interval + day
daymean <- aggregate(steps ~ interval+day,data=new,FUN="mean")
# Plot time series accordingly
qplot(interval,steps, data=daymean, geom="line", xlim=c(0,2355),main="Average daily activity",xlab = "5 minutes interval (0 a.m.-23.55 p.m)", ylab = "average steps taken/interval")
#-------------------------------------------------------------






