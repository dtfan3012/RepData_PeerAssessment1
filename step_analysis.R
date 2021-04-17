# Install and call the libraries, dplyr, tidyr, lubridate, ggplot2
install.packages("dplyr","tidyr","lubridate","ggplot2")
library(dplyr); library(tidyr); library(lubridate); library(ggplot2)

setwd(getwd())

# Store the weblink as "url" to access the data for cleaning and analysis
url <-  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# Download the file and unzip the file in the working directory
    download.file(url, destfile = "./activity.zip", method = "curl")
    unzip("activity.zip")
    
#read the csv file
  dt1 <- read.csv("activity.csv", header = TRUE, sep = ",")
#change char data to lubridate form %Y%M%D
    dt1$date <- ymd(dt1$date)



#Group data by day and find step stats for each day
dt1_by_day <- dt1 %>%
  group_by(date) %>% 
  summarize(day_sum = sum(steps), 
            day_mean = mean(steps), 
            day_median = median(steps))
# Make the histogram plot of sum of the steps over 2 months 
# in each interval
png("hist_day_step_sum.png")
hist(dt1_by_day$day_sum, col = "red", breaks = 25,
     xlab = "Total Steps/Day",
     main = "Histogram of Steps Each Day")
dev.off()

    mean_va <- mean(dt1_by_day$day_mean, na.rm = TRUE) # Print Mean
    median_va <- median(dt1_by_day$day_median, na.rm = TRUE) # Print Median

    print(paste("The mean of the total steps each day equals", mean_va, "and the median equals", median_va, sep = " "))   

    



#Set the interval to a "0000" fixed format for time format
  dt1$interval <- sprintf("%04d", dt1$interval)
  
#Combine the date and interval column to date_time column
  dt1 <- dt1 %>% mutate(date_time = paste(date, interval, sep = " "), .before = 1)
  
#Call Lubridate parse_date-time on the combined column
  dt1$date_time <- parse_date_time(dt1$date_time, "ymdHM", tz = "UTC")

interval_stats <- dt1 %>%
  group_by(interval) %>%
  summarize(date_time, 
            interval_avg = mean(steps, na.rm = TRUE))

# Simple call base plotting of the interval windows and plot the average
# steps in each interval across the 2 months 

plot(interval_stats$interval,interval_stats$interval_avg, 
     type = 'l', lwd = 2, col = 'blue', 
     xlab = "Interval Windows 00:00 24:00 Hrs", ylab = "Avg Steps",
     main = "Average Steps in Each Interval Window Across 2 Month Study")

#Find the max average steps interval to store in "max_intv"
#Format the answer to military time of 08:35 HRS

max_intv <- which.max(interval_stats$interval_avg)
timemax <- interval_stats[max_intv,"interval"]

print(paste0("The time interval that showed the highest average 
             across the 2 months was ", 
             substr(timemax,2,2), ":", 
             substr(timemax,3,4), " AM"))



#Find total number of NA values by summing the boolean function is.na
  total_nas <- sum(is.na(dt1$steps))
  print(paste0("The dataset contains a total of ", total_nas, " NA values for step counts"))
# Store the indices of the NAs in steps in "na_values"
  na_values <- which(is.na(dt1$steps))
#Set the NA steps to the mean value of steps/interval and store in 
#new copy of "dt1" called "no_na_dt1"
  no_na_dt1 <- dt1
  no_na_dt1[na_values,2] <- mean(dt1_by_day$day_mean, na.rm = TRUE)

Grouped_no_na_dt1 <-  no_na_dt1 %>%
      group_by(date) %>%
      summarize(day_sum = sum(steps), 
            day_mean = mean(steps), 
            day_median = median(steps))
#
hist(Grouped_no_na_dt1$day_sum, col = "red", breaks = 25,
     xlab = "Total Steps/Day",
     main = "Histogram of Steps Each Day No NAs")
#
group_mean <- mean(Grouped_no_na_dt1$day_mean)
group_median <- median(Grouped_no_na_dt1$day_median)
print(paste0("The mean and median for the data with replaced NAs are ", group_mean," and ",group_median,"."))

#Compare Summaries of assigning NAs to the 
summary(Grouped_no_na_dt1)
summary(dt1_by_day)



#
no_na_dt1 <- no_na_dt1 %>% 
  mutate(day = wday(date_time, abbr = FALSE, label = TRUE))
#
no_na_dt1 <- no_na_dt1 %>% 
  mutate(wkdy_wknd = ifelse(day %in% c("Sunday","Saturday"),
                            "Weekend","Weekday"))
#
dt_fin <- no_na_dt1 %>%
  group_by(wkdy_wknd, interval, .add = TRUE) %>%
  summarize(int_avg = mean(steps))
#
dt_fin$interval <- as.numeric(as.character(dt_fin$interval))
#
g <- ggplot(dt_fin, aes(interval, int_avg), group = 1)
g + geom_line(col = "blue", lwd = 1) +
    labs(x = "Interval 0000 - 2355 HRS", y = "# Steps Avg") + 
    facet_grid(.~wkdy_wknd)

