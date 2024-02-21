# ----- B1705 Week 6 | Time Series Analysis | 20.02.2024 -----

# ----- Pre-Lecture Work -----
# ----- 1. Loading Data -----

data <- read.csv('https://www.dropbox.com/scl/fi/755z2zrppejfazkun5h7t/tsa_01.csv?rlkey=e5welqld5idyeb0ccwa44whfj&dl=1')
head(data)

# ----- 2. Converting Data to Time-Series Object -----
# Convert the data to a time series object called ts_data
ts_data <- ts(data$Value, start = c(2018, 1), frequency = 12)

# ----- 3. Plot and Apply Simple Moving Average to Data -----
# Plot the time series data
plot(ts_data, main="Time Series Data", xlab="Time", ylab="Value")

# Apply a simple moving average
library(stats)
moving_avg <- filter(ts_data, rep(1/5, 5), sides = 2)
lines(moving_avg, col="red")

# ----- 4. Forecasting with Arima -----
# Forecasting using ARIMA
library(forecast)
arima_model <- auto.arima(ts_data)
forecasted_data <- forecast(arima_model, h=12) # forecast for the next year

# Plot the forecast
plot(forecasted_data)

# ----- Lecture Work -----
# ----- 5. Converting Date and Times -----
# Convert a character string (2023-12-11) to a Date
date_from_string <- as.Date("2023-12-11", format="%Y-%m-%d")
print(date_from_string)

# Convert a character string to POSIXct
datetime_from_string <- as.POSIXct("2022-01-01 12:00:00", format="%Y-%m-%d %H:%M:%S")
print(datetime_from_string)


# ----- 6. Dealing with Various Formats -----
# Different date formats
date_euro_format <- as.Date("01/02/2022", format="%d/%m/%Y") # Day/Month/Year
print(date_euro_format)

# Time in 12-hour format
datetime_12hr <- as.POSIXct("01/02/2022 01:30:00 PM", format="%d/%m/%Y %I:%M:%S %p")
print(datetime_12hr)

# Dataset with date and time in different formats
date_time_data <- data.frame(
  date_string = c("2023-12-19", "19-Dec-2023", "12/19/2023", "20231219", 
                  "2023/12/19 14:20", "19-Dec-2023 14:20", "12/19/2023 14:20", "202312191420"),
  format = c("YYYY-MM-DD", "DD-MMM-YYYY", "MM/DD/YYYY", "YYYYMMDD",
             "YYYY/MM/DD HH:MM", "DD-MMM-YYYY HH:MM", "MM/DD/YYYY HH:MM", "YYYYMMDDHHMM")
)

# Show original dataset
print("Original Dataset with Various Date Formats")
print(date_time_data)

# Convert date strings to Date objects using as.Date()
date_time_data$date_as_date <- c(
  as.Date(date_time_data$date_string[1], format = "%Y-%m-%d"),
  as.Date(date_time_data$date_string[2], format = "%d-%b-%Y"),
  as.Date(date_time_data$date_string[3], format = "%m/%d/%Y"),
  as.Date(date_time_data$date_string[4], format = "%Y%m%d"),
  as.Date(date_time_data$date_string[5], format = "%Y/%m/%d"),
  as.Date(date_time_data$date_string[6], format = "%d-%b-%Y"),
  as.Date(date_time_data$date_string[7], format = "%m/%d/%Y"),
  as.Date(date_time_data$date_string[8], format = "%Y%m%d")
)

# Convert date strings to POSIXct datetime objects using as.POSIXct()
date_time_data$datetime_as_posix <- c(
  as.POSIXct(date_time_data$date_string[1], format = "%Y-%m-%d"),
  as.POSIXct(date_time_data$date_string[2], format = "%d-%b-%Y"),
  as.POSIXct(date_time_data$date_string[3], format = "%m/%d/%Y"),
  as.POSIXct(date_time_data$date_string[4], format = "%Y%m%d"),
  as.POSIXct(date_time_data$date_string[5], format = "%Y/%m/%d %H:%M"),
  as.POSIXct(date_time_data$date_string[6], format = "%d-%b-%Y %H:%M"),
  as.POSIXct(date_time_data$date_string[7], format = "%m/%d/%Y %H:%M"),
  as.POSIXct(date_time_data$date_string[8], format = "%Y%m%d%H%M")
)

# Show the dataset with converted date and datetime columns
print("Dataset with Converted Date and DateTime Columns")
print(date_time_data)

# ----- 7. Extracting Components from Date-Time -----
specific_datetime <- as.POSIXct("2023-12-11 20:59:59")

# Extracting components
year <- format(specific_datetime, "%Y")
month <- format(specific_datetime, "%m")
day <- format(specific_datetime, "%d")
hour <- format(specific_datetime, "%H")
minutes <- format(specific_datetime, "%M")
seconds <- format(specific_datetime, "%S")

print(paste("Year:", year, "- Month:", month, "- Day:", day, "- Hour:", hour, "- Minutes:", minutes, "- Seconds:", seconds))

# ----- 8. Operations on Date-Time -----
# Adding days to a date
future_date <- specific_date + 30
print(future_date)

# Subtracting time from a datetime
past_datetime <- specific_datetime - as.difftime(1, units="hours")
print(past_datetime)

# Difference in days
date_diff <- as.Date("2022-02-01") - as.Date("2022-01-01")
print(date_diff)

# Difference in seconds
time_diff <- as.POSIXct("2022-01-01 13:00:00") - as.POSIXct("2022-01-01 12:00:00") 
print(as.numeric(time_diff, units="secs"))

# ----- 9. Times Zones -----
# Creating a POSIXct object with a specific time zone
datetime_ny <- as.POSIXct("2023-01-01 12:00:00", tz="America/New_York")
datetime_london <- as.POSIXct("2023-01-01 12:00:00", tz="Europe/London")

# Comparing times
print(datetime_ny)
print(datetime_london)


# ----- 10. Using the Lubridate Package -----

library(lubridate)

# Easy parsing of dates

ymd("20220101")

mdy("01/02/2022")

dmy("02-01-2022")

# Arithmetic with lubridate
date1 <- ymd("2022-01-01")
date1 %m+% months(1) # Add a month

date1 %m-% months(1) # Subtract a month

# Extracting components
year(date1)

month(date1)

day(date1)

# Rounding off date and time to the nearest day, hour, etc.
round_date(datetime_ny, unit="day")

floor_date(datetime_ny, unit="hour")

ceiling_date(datetime_ny, unit="minute")

# Dealing with duration and period - understanding the difference between duration (exact time spans) and period (human-readable time spans).
# Duration: exact time spans
duration_one_day <- ddays(1)
duration_one_hour <- dhours(1)
datetime_ny + duration_one_day

# Period: human-readable time spans
period_one_month <- months(1)
date1 + period_one_month

# Handling Daylight Saving Time Dealing with complexities due to changes in daylight saving time.
# Before daylight saving time
dt1 <- as.POSIXct("2022-03-13 01:59:59", tz="America/New_York")

# After daylight saving time
dt2 <- dt1 + dhours(1)
print(dt1)
print(dt2)













