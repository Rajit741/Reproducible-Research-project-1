#Loading and preprocessing the data

act <- read.csv("~/Reproducible Research/week2/activity.csv")
act$date <- as.Date(act$date)
p <- as.POSIXct(as.Date(act$date))
d <- unclass(as.POSIXlt(act$date))
act$yday <- d$yday


#What is mean total number of steps taken per day

steps_dist <- tapply(act$steps, act$yday, sum)

#1.Make a histogram of the total number of steps taken each day
hist(steps_dist, breaks = 10, main = "number of steps taken daily", xlab = "number of steps")
summary(steps_dist)

#2.Calculate and report the mean and median total number of steps taken per day
#The mean and median total steps taken per day are1.07610^{4} and 1.07710^{4}respectively.


#What is the average daily activity pattern?

#1.Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

mean_steps <- tapply(act$steps, act$interval, mean, na.rm = TRUE)
plot(mean_steps,type ="l")
abline(h=206.1698, col ="blue")
abline(v=105, col = "blue")

#2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

mean_steps[mean_steps>200]

head(dimnames(mean_steps)[[1]],14)

maxpos <- sum(as.numeric(dimnames(mean_steps)[[1]])<=835)
maxpos


max <- mean_steps[[maxpos]]
max


#Imputing missing values

sum(is.na(act$steps))

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Step 1:determine which element of original dataframe is ‘NA’ in the for-loop,
#Step 2:determine the 5-minutes interval it falls under.
#Step 3: The ‘NA’ is then replaced by the average steps computed above for that particular interval period.
#Create a new dataset that is equal to the original dataset but with the missing data filled in

for(i in 1:length(act$steps)) 
{
  if (is.na(act$steps[i])) ## which element 'i'  of original dataframe
  {act$steps[i] <- mean_steps[as.character(act$interval[i])] ## replace 'NA' based on that interval
  }
}

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

steps_dist <- tapply(act$steps, act$yday, sum)
hist(steps_dist, breaks = 10, main = "number of steps taken daily",xlab = "number of steps", ylim = c(0,25))

summary(steps_dist)

#the mean and median total steps taken per day are1.07710^{4} and 1.07710^{4}respectively. Compared to the earlier plot, there is no difference in the mean, but a slight increase in the median. There is a signficant increase in the frequency of the median.


#Are there differences in activity patterns between weekdays and weekends?



#1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
act$wday<- weekdays(p)
for (i in 1:length(act$steps))
{
  if(act$wday[i]=="Saturday"|act$wday[i]=="Sunday")
  {
    act$day[i] <- "weekend"
  } else  
  {
    act$day[i] <- "weekday"
  }
}

table(act$day)

#Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

act_split <- split(act, act$day)
ave_steps_weekday <- with(act_split$weekday,tapply(steps, interval, mean))
ave_steps_weekday <- data.frame(ave = ave_steps_weekday, interval = as.numeric(dimnames(ave_steps_weekday)[[1]]),day="weekday")
ave_steps_weekend <- with(act_split$weekend,tapply(steps, interval, mean))
ave_steps_weekend <- data.frame(ave = ave_steps_weekend, interval = as.numeric(dimnames(ave_steps_weekend)[[1]]),day="weekend")
ave_steps <- rbind(ave_steps_weekend,ave_steps_weekday)

library(lattice)
y<- ave_steps$ave
x<- ave_steps$interval
f<-ave_steps$day
xyplot(y~x|f, layout=c(1,2), type="l", xlab = "5-minutes Interval", ylab = "Number of Steps")

