# ----- B1705 Week 6 | Exploratory Time Series Analysis | 20.02.2024 -----

# ----- Pre-Lecture Work -----
# ----- 1. Loading Data & Libraries -----

# Load packages
library(forecast)
library(tseries)
library(lubridate)
library(zoo)
library(xts)

# Generate a Synthetic Time Series Dataset
# Set seed for reproducibility
set.seed(1234)

# Generate Date Sequence
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2023-01-01")
dates <- seq.Date(start_date, end_date, by="month")

# Generate Synthetic Time Series Data
# We'll create data with a trend, seasonality, and random noise
trend <- seq_along(dates) 
seasonality <- sin(seq_along(dates) * 2 * pi / 12) * 50 # Annual seasonality
noise <- rnorm(length(dates), mean = 0, sd = 5)

# Combine to Create Final Time Series Data
time_series_data <- round(trend + seasonality + noise,1)
time_series_data <- abs(time_series_data)

# Create a Data Frame
data <- data.frame(date = dates, value = time_series_data)

# Add Missing Values for Realism
set.seed(5678)
missing_indices <- sample(1:length(data$value), 5)
data$value[missing_indices] <- NA

# Clean environment

rm(dates, end_date, noise, seasonality, start_date, time_series_data, trend)

# Print head of the dataset
head(data,10)


# ----- 2. Handling Missing Values -----
##### 2.1. Check for missing values #####
missing_values_exist <- any(is.na(data$value))

missing_count <- sum(is.na(data$value))

print(paste("Number of missing values currently in data$value:", missing_count))

##### 2.2. Impute for Missing Values #####
# Handling missing values by imputing the mean value and replacing the missing value with the mean.
data$value[is.na(data$value)] <- mean(data$value, na.rm = TRUE) # Imputation
missing_values_exist <- any(is.na(data$value))
missing_count <- sum(is.na(data$value))
print(paste("There are now", missing_count, "missing values in data$value."))

rm(missing_count, missing_indices, missing_values_exist) # clear environment


# ----- 3. Creating Time Series -----

data_old <- data # I'm storing the original dataset in case I need it later
data$date <- NULL  # delete the `date` variable in `data`

# Creating ts Objects
ts_data <- ts(data$value, frequency=12, start(2020,0)) # For monthly data
print(ts_data)


# ----- 4. Plotting Time Series -----
# Time Series Plots
plot(ts_data)

# ----- 5. Seasonal and Trend Decomposition -----
# Seasonal and Trend Decomposition
decomposed_data <- decompose(ts_data)
plot(decomposed_data)

# ----- 6. Working w/Dates & Times -----
# Code to extract todays/current date
today <- Sys.Date()
print(today)

# Creating a specific Date object
specific_date <- as.Date("2021-12-31")
print(specific_date)

# ----- 7. Working with P0SIXct & P0SIXlt
# POSIXct and POSIXlt are used for date-time data.

# POSIXct represents the (date-time) as the number of seconds 
# since the beginning of 1970 (known as the Unix epoch), 
# whereas POSIXlt is a list that contains detailed information 
# about the date-time

# Current date-time
now <- Sys.time()
print(now)

# Creating a POSIXct object
specific_datetime <- as.POSIXct("2021-12-31 23:59:59")
print(specific_datetime)

# ----- 8. Converting Date & Time Formats -----
# Convert a character string to a Date
date_from_string <- as.Date("2022-01-01", format="%Y-%m-%d")
print(date_from_string)

# Convert a character string to POSIXct
datetime_from_string <- as.POSIXct("2022-01-01 12:00:00", format="%Y-%m-%d %H:%M:%S")
print(datetime_from_string)


# ----- 9. Dealing with Different Formats -----

# Different date format
date_euro_format <- as.Date("01/02/2022", format="%d/%m/%Y") # Day/Month/Year
print(date_euro_format)

# Time in 12-hour format
datetime_12hr <- as.POSIXct("01/02/2022 01:30:00 PM", format="%d/%m/%Y %I:%M:%S %p")
print(datetime_12hr)

# ----- 10. Extracting Components from date-time -----

# Extracting components
year <- format(specific_datetime, "%Y")
month <- format(specific_datetime, "%m")
day <- format(specific_datetime, "%d")
hour <- format(specific_datetime, "%H")
minutes <- format(specific_datetime, "%M")
seconds <- format(specific_datetime, "%S")

print(paste("Year:", year, "- Month:", month, "- Day:", day, "- Hour:", hour, "- Minutes:", minutes, "- Seconds:", seconds))

# ----- 11. Operations on Date-time ----
# Adding days to a date
future_date <- specific_date + 30
print(future_date)

# Subtracting time from a datetime
past_datetime <- specific_datetime - as.difftime(1, units="hours")
print(past_datetime)

# Calculating differences between times 
# Difference in days
date_diff <- as.Date("2022-02-01") - as.Date("2022-01-01")
print(date_diff)

# Difference in seconds
time_diff <- as.POSIXct("2022-01-01 13:00:00") - as.POSIXct("2022-01-01 12:00:00")
print(as.numeric(time_diff, units="secs"))

#Time Zones:
# Creating a POSIXct object with a specific time zone
datetime_ny <- as.POSIXct("2022-01-01 12:00:00", tz="America/New_York")
datetime_london <- as.POSIXct("2022-01-01 12:00:00", tz="Europe/London")

# Comparing times
print(datetime_ny)
print(datetime_london)

# ----- 12. Advanced Date-Time Manipulation using Lubridate -----

# load lubridate
library(lubridate)

# Easy parsing of dates
ymd("20220101")
## [1] "2022-01-01"
mdy("01/02/2022")
## [1] "2022-01-02"
dmy("02-01-2022")
## [1] "2022-01-02"

# Arithmetic with lubridate
date1 <- ymd("2022-01-01")
date1 %m+% months(1) # Add a month
## [1] "2022-02-01"
date1 %m-% months(1) # Subtract a month
## [1] "2021-12-01"

# Extracting components
year(date1)
## [1] 2022
month(date1)
## [1] 1
day(date1)
## [1] 1


# Rounding dates
round_date(datetime_ny, unit="day")
## [1] "2022-01-02 EST"
floor_date(datetime_ny, unit="hour")
## [1] "2022-01-01 12:00:00 EST"
ceiling_date(datetime_ny, unit="minute")
## [1] "2022-01-01 12:00:00 EST"

# Dealing with Duration and Period
# Duration: exact time spans
duration_one_day <- ddays(1)
duration_one_hour <- dhours(1)
datetime_ny + duration_one_day

# Period: human-readable time spans
period_one_month <- months(1)
date1 + period_one_month

# Before daylight saving time
dt1 <- as.POSIXct("2022-03-13 01:59:59", tz="America/New_York")

# After daylight saving time
dt2 <- dt1 + dhours(1)

print(dt1)
print(dt2)


# ----- Lecture Work -----
# ----- 13. Preparation and Data Creation -----
rm(list=ls()) # clear environment

# Load necessary libraries
library(ggplot2)
library(forecast)
library(tseries)
library(ggplot2)
library(zoo)

# Average running speed of an athlete over 52 weeks
set.seed(123) # For reproducibility
weeks <- 1:52
speed <- round(10 + rnorm(52, mean = 0, sd = 0.4),2) # Average speed with some random variation
sports_data <- data.frame(Week = weeks, Speed = speed)
rm(speed)
rm(weeks)

head(sports_data)

# ----- 14. Creating Time-Series Object -----
# Convert to time series using the ts function
ts_data <- ts(sports_data$Speed, frequency = 52)

# Convert time series to data frame
ts_df <- data.frame(Time = time(ts_data), Value = as.vector(ts_data))

# Calculate the moving average
ts_df$MovingAvg <- rollmean(ts_df$Value, k = 12, fill = NA, align = "center")

# Plot using ggplot2
ggplot(ts_df, aes(x = Time)) + 
  geom_line(aes(y = Value), color = "blue") +  # Original time series
  geom_line(aes(y = MovingAvg), color = "red") +  # Moving average
  labs(x = "Time", y = "Value", title = "Time Series with Moving Average") +
  theme_minimal()

# Convert to time series
ts_data_02 <- ts(sports_data$Speed, frequency = 5)

# Plotting the time series with 'Week' on the x-axis
plot(ts_data_02, type = "o", col = "blue", main = "Five Observations per Week", xlab = "Week", ylab = "Value")

# Apply a simple moving average
# Calculate the moving average over a window of 12 periods
moving_avg <- rollmean(ts_data_02, k = 10, fill = NA, align = "center")
lines(moving_avg, col="red")

# for clarity, I've added a vertical line at each week
for(i in seq(1, length(ts_data_02), by = 1)) {
  abline(v = i, col = "gray", lty = 2)
}

# ----- 15. Examining Trends and Seasonality -----
# Time Series Decomposition
decomposed <- stl(ts_data_02, s.window = "periodic")
plot(decomposed)

# ----- 16. TSA Practice -----
# Creating dataset
df <- read.csv('https://www.dropbox.com/scl/fi/nh221nxwv0fp0fy773i2j/golf_data.csv?rlkey=666lxnhwazjpg70j9wgowog8x&dl=1')
df <- df[1:156,]
df$Date <- NULL
df$Score <- round(df$Score,0)

# Creating ts dataset
golfdata <- ts(df$Score, frequency=52, start(2021,0)) # For monthly data
print(golfdata)

# Time Series Plots
plot(golfdata)

# Converting time series to dataframe
tsgolf <- data.frame(Time = time(golfdata), Value = as.vector(golfdata))

# Calculate the moving average
tsgolf$MovingAvg <- rollmean(tsgolf$Value, k = 12, fill = NA, align = "center")

ggplot(tsgolf, aes(x = Time)) + 
  geom_line(aes(y = Value), color = "blue") +  # Original time series
  geom_line(aes(y = MovingAvg), color = "red") +  # Moving average
  labs(x = "Time", y = "Value", title = "Golf Scores with Moving Average") +
  theme_minimal()

# Convert to time series
golfdata02 <- ts(df$Score, frequency = 52)

# Plotting the time series with 'Week' on the x-axis
plot(golfdata02, type = "o", col = "blue", main = "52 Observations per 3-Year", xlab = "Year", ylab = "Score")

# Apply a simple moving average
# Calculate the moving average over a window of 12 periods
moving_avg <- rollmean(golfdata02, k = 10, fill = NA, align = "center")
lines(moving_avg, col="red")

# for clarity, I've added a vertical line at each week
for(i in seq(1, length(golfdata02), by = 1)) {
  abline(v = i, col = "gray", lty = 2)
}

decomposed <- stl(golfdata02, s.window = "periodic")
plot(decomposed)


# Check over to see what actual code is 


