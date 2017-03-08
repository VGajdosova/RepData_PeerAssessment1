
library(ggplot2)
library(plyr)
library(dplyr)

### THIS IS JUST R CODE SEPARATELY STORED IN A FILE, FOR COMPREHENSION, MD FILE IS TO BE READ. ###

## Loading and preprocessing the data
activity <- read.csv("activity.csv")

#Looking at data
head(activity)
summary(activity)
str(activity)
dim(activity)
tail(activity)

#Formatting to date format
class(activity$date)
activity$date<- as.Date(activity$date)
class(activity$date)

#Subsetting a cleaner data set without NAs
clean_data <- activity[!is.na(activity$steps),]
summary(clean_data)
dim(clean_data)

#Total daily steps calculation
TotalDailySteps <- tapply(clean_data$steps, clean_data$date, sum)
hist(TotalDailySteps, breaks=5, xlab="Number of steps", main="Total number of steps taken daily")

#Calculation of mean and median
mean_steps <- as.integer(mean(TotalDailySteps))
median_steps <- as.integer(median(TotalDailySteps))


library(ggplot2)
library(plyr)
clean_data <- activity[!is.na(activity$steps),]
#Average number of steps per interval
mes_interval<- ddply(clean_data, .(interval), summarize, Avg = mean(steps))
#Time series plot of the average number of steps per interval
ggplot(mes_interval, aes(x =interval , y=Avg)) +
        geom_line(color="red", size=1) +
        labs(title = "Average number of steps", x = "Measurement Interval", y = "Average number of steps per interval")

# Identification of the 5-minute interval that contains (on average across all the days in the dataset) #the maximum number of steps


maximum_steps <- max(mes_interval$Avg)
#maximum_steps <- round(maximum_steps)
top_interval<- mes_interval[mes_interval$Avg==maximum_steps,1]
top_interval

# Calculation of the total number of missing values in the dataset 
nrow(activity[is.na(activity$steps),])

# Filling in of the missing values. Creating a new dataset with the missing data filled in.
#The strategy used to impute the missing values is by using the mean number of steps in the correspondin#g 5-minute interval. 

#creating a new dataset identical to the original one
activity_new <- activity
#finding the NAs, mean and filling in the mean into the missing values
missing_values <- is.na(activity_new$steps)
avg_interval<- tapply(activity_new$steps, activity_new$interval, mean, na.rm=TRUE, simplify = TRUE)
activity_new$steps[missing_values] <- avg_interval[as.character(activity_new$interval[missing_values])]
#checking if there are no more NAs
sum(is.na(activity_new))
#column arrangment
activity_new<- activity_new[, c("date", "interval", "steps")]
head(activity_new)

# Histogram of the total number of steps taken each day and calculation of the mean and median total #number of steps taken per day. 

TotalDailySteps_full <- activity_new %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarize(steps = sum(steps)) %>%
        print

ggplot(TotalDailySteps_full, aes(x = steps)) +
        geom_histogram(fill = "red", binwidth = 1000) +
        labs(title = "Histogram of Steps per day after imputing missing values", x = "Total steps per day", y = "Frequency")

#Now, we will calculate the mean and median after imputing the data and have a look if these differ from the estimates from the first part of the assignment

mean_steps_full <- mean(TotalDailySteps_full$steps)
mean_steps_full
median_steps_full <- median(TotalDailySteps_full$steps)
median_steps_full

#We will create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

activity_new<- activity_new %>%
        mutate(weektype= ifelse(weekdays(activity_new$date)=="Saturday" |    weekdays(activity_new$date)=="Sunday", "Weekend", "Weekday"))

### Panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
mes_interval2<- ddply(activity_new, .(interval, weektype), summarize, Avg = mean(steps))
head(mes_interval2)

plot<- ggplot(mes_interval2, aes(x =interval , y=Avg, color=weektype)) +
        geom_line() +
        labs(title = "Avgerage Daily Steps by Weektype", x = "Interval", y = "Number of Steps") +
        facet_wrap(~weektype, ncol = 1, nrow=2)
print(plot)