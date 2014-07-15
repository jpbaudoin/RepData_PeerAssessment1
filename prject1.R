# Load data
data <- read.csv(file="activity.csv")

#1 What is mean total number of steps taken per day?
total_steps_by_date <- aggregate(data$steps, by=list(Category=data$date), FUN=sum )
names(total_steps_by_date) <- c("date", "total_steps")

par(mfrow=c(1,2))
hist(total_steps_by_date$total_steps)
hist(total_steps_by_date$total_steps, freq=F)

steps_na <- is.na(total_steps_by_date$total_steps)
total_steps_by_date[steps_na, ]

steps_mean <- mean(total_steps_by_date$total_steps, na.rm=T)
steps_median <- median(total_steps_by_date$total_steps, na.rm=T)
summary(total_steps_by_date)

sum(is.na(data[data$date == "2012-10-01", "steps" ]))
sum(!is.na(data[data$date == "2012-10-01", "steps" ]))

#2 What is the average daily activity pattern?
avg_steps_by_interval <- aggregate(data$steps, by=list(Category=data$interval), FUN=mean, na.rm=T )
names(avg_steps_by_interval) <- c("interval", "mean_steps")

par(mfrow=c(1,1))
plot(x= avg_steps_by_interval$interval, y= avg_steps_by_interval$mean_steps, 
     type="l", xlab = "5 minute interval", ylab = "Steps mean")

max_steps <- max(avg_steps_by_interval$mean_steps)
intervel_for_max <- avg_steps_by_interval[avg_steps_by_interval$mean_steps == max_steps,
                      "interval"]
abline(v=intervel_for_max, untf = FALSE, col = "red")

#3 Input missing values

 # Total of missing values

 # Filling missing values

 # Histogram