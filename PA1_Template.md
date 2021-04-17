This markdown file contains the work up and initial anaylsis of the
Activity steps data as part of the Reproducible Data Course in the Data
Science Specialization Track at Coursera.org using the R programming
language. The code below requires the following packages: dplyr, tidyr,
lubridate, and ggplot2. If there are not installed in your installation
of R, they must added with using the code below in the comments

The file that contains the data is linked to a url below. The code sets
a working directory and then downloads, unzips, and read the data

in as “activity.csv”. The date variable is transformed using the ymd()
function in the lubridate package.

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

The analysis first asks for total number of steps taken each day and
what are the mean and median number over each day? The data is grouped
by the date and then summarized finding the total sum, the mean, and the
median for each day. The histogram is plotted and then the mean and
median of all days is printed below the graph.

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

    ## quartz_off_screen 
    ##                 2

    hist(dt1_by_day$day_sum, col = "red", breaks = 25,
         xlab = "Total Steps/Day",
         main = "Histogram of Steps Each Day")

![](PA1_Template_files/figure-markdown_strict/histcode-1.png)

    print(paste("The mean of steps each day =", mean_va, "and the median =", median_va, sep = " "))  

    ## [1] "The mean of steps each day = 37.3825995807128 and the median = 0"

Next the analysis looks at the the average steps recorded during each
time interval across the 2 months. Columns are formatted and combined to
produce a complete date\_time variable and then grouped by the 5-min
interval and summarized to find mean for each interval.

This is plotted as a blue line graph.

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

    ## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.

    # Simple call base plotting of the interval windows and plot the average
    # steps in each interval across the 2 months 
    png("interval_avg_lineplot.png")
    plot(interval_stats$interval,interval_stats$interval_avg, 
         type = 'l', lwd = 2, col = 'blue', 
         xlab = "Interval Windows 00:00 - 24:00 Hrs", ylab = "Avg Steps",
         main = "Average Steps in Each Interval Window Across 2 Month Study")
    dev.off()

    ## quartz_off_screen 
    ##                 2

    plot(interval_stats$interval,interval_stats$interval_avg, 
         type = 'l', lwd = 2, col = 'blue', 
         xlab = "Interval Windows 00:00 - 24:00 Hrs", ylab = "Avg Steps",
         main = "Average Steps in Each Interval Window Across 2 Month Study")

![](PA1_Template_files/figure-markdown_strict/intv_line-1.png)

    #Find the max average steps interval to store in "max_intv"
    #Format the answer to military time of 19:35 HRS
        max_intv <- which.max(interval_stats$interval_avg)
        timemax <- interval_stats[max_intv,"interval"]
    print(paste0("The highest average interval across the 2 months was ", substr(timemax,2,2), ":", substr(timemax,3,4), " AM"))

    ## [1] "The highest average interval across the 2 months was 8:35 AM"

Observing the dataset carefully shows that there are a number of NAs
listed for step counts across many of the time intervals. The code below
finds the total number of NAs for the entire dataset.

    #Find total number of NA values by summing the boolean function is.na
      total_nas <- sum(is.na(dt1$steps))
      print(paste0("The dataset contains a total of ", total_nas, " NA values for step counts"))

    ## [1] "The dataset contains a total of 2304 NA values for step counts"

The next analysis asks how the dataset stats might change if the NA
values were replaced with statistical value. Each NA was replaced with
the mean value of the average of each day with the next few lines of
code and then a new histogram was created in this new set of data.

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
    png("No_NAs_hist.png")
    hist(Grouped_no_na_dt1$day_sum, col = "red", breaks = 25,
         xlab = "Total Steps/Day",
         main = "Histogram of Steps Each Day No NAs")
    dev.off()

    ## quartz_off_screen 
    ##                 2

    hist(Grouped_no_na_dt1$day_sum, col = "red", breaks = 25,
         xlab = "Total Steps/Day",
         main = "Histogram of Steps Each Day No NAs")

![](PA1_Template_files/figure-markdown_strict/hist2-1.png)

    #
    group_mean <- mean(Grouped_no_na_dt1$day_mean)

    group_median <- median(Grouped_no_na_dt1$day_median)

    print(paste0("The mean and median with replaced NAs are ", group_mean," and ",group_median,"."))

    ## [1] "The mean and median with replaced NAs are 37.3825995807128 and 0."

The histogram has now become more centered with a larger number of days
falling in the 10k - 11k window. Let’s compare the general statistics of
the two datasets, the original set “dt1”, sorted by days, and the
“no\_na\_values” set, also sorted by days.

    #Compare Summaries of replacing NAs with a statistical value
    summary(Grouped_no_na_dt1)

    ##       date               day_sum         day_mean         day_median    
    ##  Min.   :2012-10-01   Min.   :   41   Min.   : 0.1424   Min.   : 0.000  
    ##  1st Qu.:2012-10-16   1st Qu.: 9819   1st Qu.:34.0938   1st Qu.: 0.000  
    ##  Median :2012-10-31   Median :10766   Median :37.3826   Median : 0.000  
    ##  Mean   :2012-10-31   Mean   :10766   Mean   :37.3826   Mean   : 4.903  
    ##  3rd Qu.:2012-11-15   3rd Qu.:12811   3rd Qu.:44.4826   3rd Qu.: 0.000  
    ##  Max.   :2012-11-30   Max.   :21194   Max.   :73.5903   Max.   :37.383

    summary(dt1_by_day) #Note that the NAs are completely related to only 8 days out of the ~60 days of the study.

    ##       date               day_sum         day_mean         day_median
    ##  Min.   :2012-10-01   Min.   :   41   Min.   : 0.1424   Min.   :0   
    ##  1st Qu.:2012-10-16   1st Qu.: 8841   1st Qu.:30.6979   1st Qu.:0   
    ##  Median :2012-10-31   Median :10765   Median :37.3785   Median :0   
    ##  Mean   :2012-10-31   Mean   :10766   Mean   :37.3826   Mean   :0   
    ##  3rd Qu.:2012-11-15   3rd Qu.:13294   3rd Qu.:46.1597   3rd Qu.:0   
    ##  Max.   :2012-11-30   Max.   :21194   Max.   :73.5903   Max.   :0   
    ##                       NA's   :8       NA's   :8         NA's   :8

The final analysis of the data asks to compare the steps statistics
between weekdays and weekends to see if there are any noticeable
differences. The code below creates additional columns by extracting the
name of the day from the date\_time variable and then creating a factor
that is conditioned on whether the day is “Sunday” or “Saturday”
otherwise it is set to “Weekday”. The data set is then grouped by the
“interval” and “wkdy\_wknd”. This is plotted using the facet\_grid
function in ggplot.

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

    ## `summarise()` has grouped output by 'wkdy_wknd'. You can override using the `.groups` argument.

    #
    dt_fin$interval <- as.numeric(as.character(dt_fin$interval))
    #
    png("wday_wknd_compare.png")
    g <- ggplot(dt_fin, aes(interval, int_avg), group = 1)
    g + geom_line(col = "blue", lwd = 1) +
        labs(x = "Interval 0000 - 2355 HRS", y = "# Steps Avg") + 
        facet_grid(.~wkdy_wknd)
    dev.off()

    ## quartz_off_screen 
    ##                 2

    g <- ggplot(dt_fin, aes(interval, int_avg), group = 1)
    g + geom_line(col = "blue", lwd = 1) +
        labs(x = "Interval 0000 - 2355 HRS", y = "# Steps Avg") + 
        facet_grid(.~wkdy_wknd)

![](PA1_Template_files/figure-markdown_strict/dt_fin-1.png)

Thank you for reading my attempt at the assignment and this analysis and
I hope everything (code & comments) was clear enough for you to follow!
