# ----- B1705 Week 6 | Stationarity and Differencing | 20.02.2024 -----

# ----- Pre-Lecture Work -----
# ----- 1. Loading Data & Libraries -----
# Load libraries
library(forecast)
library(ggplot2)
library(tseries)

# Function to generate a stationary time series
generate_stationary_ts <- function(n) {
  arima.sim(model = list(ar = 0.5), n = n)
}

# Generate time series data
stationary_ts <- generate_stationary_ts(100)

# Plot the stationary time series
p <- ggplot() + geom_line(aes(x = 1:100, y = stationary_ts), color = "blue") +
  ggtitle("Stationary Time Series") +
  xlab("Time") + ylab("Value")
print(p)

# Running ADF Test 
# Conduct Augmented Dickey-Fuller Test
adf_test <- adf.test(stationary_ts, alternative = "stationary")

# Output the result of the ADF test
print(adf_test)

# ----- 2. Example of Non-stationary Time Series -----
# Load libraries
library(forecast)
library(ggplot2)
library(tseries)

# Function to generate a non-stationary time series
generate_non_stationary_ts <- function(n) {
  cumsum(rnorm(n))
}

# Generate time series data
non_stationary_ts <- generate_non_stationary_ts(100)

# Plot the time series
p <- ggplot() + geom_line(aes(x = 1:100, y = non_stationary_ts), color = "red") +
  ggtitle("Non-Stationary Time Series") +
  xlab("Time") + ylab("Value")
print(p)

# Conduct Augmented Dickey-Fuller Test
adf_test <- adf.test(non_stationary_ts, alternative = "stationary")

# Output the result of the ADF test
print(adf_test)

# ----- Lecture Work -----
# ----- 3. Examining Stationarity Demonstration -----
# Creating new dataset and loading libraries
rm(list=ls()) # clear environment

# Load necessary libraries
library(forecast)
library(ggplot2)
library(tseries)

# Function to generate a stationary time series
generate_stationary_ts <- function(n) {
  # AR(1) process with a coefficient that ensures stationarity (absolute value < 1)
  arima.sim(model = list(ar = 0.5), n = n)
}

# Generate time series data
stationary_ts <- generate_stationary_ts(120)
rm(generate_stationary_ts)


# ----- 4. Preparing Datset -----
# Convert to ts object
stationary_ts <- ts(stationary_ts, frequency = 12, start = c(2010, 1)) # This includes data starts from Jan 2010

# Plot the generated time series data
plot(stationary_ts, main = "Plot of Time Series Data (mean in red)", xlab = "Time (year)", ylab = "Value")

# add the mean value
# Calculate mean of the time series
mean_value <- mean(stationary_ts, na.rm = TRUE)  # na.rm = TRUE ensures NA values are ignored

# Add a horizontal line at the mean value
abline(h = mean_value, col = "red", lwd = 2)  # 'h' spe

# ----- 5. Testing for Stationarity -----
library(urca)

# Perform the Augmented Dickey-Fuller Test
adf_test <- ur.df(stationary_ts, type = "drift", lags = 1)

# View the summary of the test, including critical values
summary(adf_test)

# ----- 6. Examining Stationarity: Demonstration 2 -----
rm(list=ls())

# Load necessary library
library(forecast)

# Set seed for reproducibility
set.seed(1234)

# Generate Time Series Data
# Creating a time series with trend and seasonality
time_points <- 120 # e.g., 120 months (10 years of monthly data)
trend_component <- seq(1, time_points, by = 1)
seasonal_component <- sin(seq(1, time_points, by = 1) * 2 * pi / 12) * 5
random_noise <- rnorm(time_points, mean = 0, sd = 2)

# Combine components to create final time series data
ts_data <- trend_component + seasonal_component + random_noise
rm(random_noise, seasonal_component, time_points, trend_component)

# Convert to ts object
ts_data <- ts(ts_data, frequency = 12, start = c(2010, 1)) # This includes data starts from Jan 2010

# Plot time series data
plot(ts_data, main = "Plot of Time Series Data", xlab = "Time (year)", ylab = "Value")

# ----- 7. Testing for Stationarity -----
library(urca)

# Perform the Augmented Dickey-Fuller Test
adf_test <- ur.df(ts_data, type = "drift", lags = 1)

# View the summary of the test, including critical values
summary(adf_test)

# We cannot reject null as test-statistic is above 5% value and p > 0.05

# ----- 8. Differencing the Time-Series -----
diff_ts_data <- diff(ts_data, differences = 1) # create new object with differencing

# Plot the differenced data
plot(diff_ts_data, main="First Differenced Data", xlab="Time", ylab="Differenced Value")

library(urca)

# Perform the Augmented Dickey-Fuller Test
adf_test <- ur.df(diff_ts_data, type = "drift", lags = 1)
# View the summary of the test, including critical values
summary(adf_test)

# ----- 10. Additional: Seasonal Differencing -----
s <- 12 # Example for monthly data with an annual cycle 
seasonal_diff_ts_data <- diff(ts_data, lag = s, differences = 1)

# Plot the seasonally differenced data
plot(seasonal_diff_ts_data, main="Seasonally Differenced Data", xlab="Time", ylab="Seasonally Differenced Value")

# ----- 11. Stationarity and Differencing: Worked Practice -----
df <- read.csv('https://www.dropbox.com/scl/fi/sw8e1k7lknsg4tcq1pgtc/ns_ts.csv?rlkey=jqtaokuw76uct9gcajzcumt3j&dl=1')

# Step 1: Remove, Convert and Prepare Data
# remove time column
df$Time <- NULL

# convert dataframe to time-series
df_ts <- ts(df, frequency = 12)

# plot df_ts
plot(df_ts, type = "o", col = "blue", main = "Time Series", xlab = "Time", ylab = "Value")

# Step 2: Test for Stationarity
# test for stationarity
library(urca)

# Perform the Augmented Dickey-Fuller Test
adf_test <- ur.df(df_ts, type = "drift", lags = 1)

# View the summary of the test, including critical values
summary(adf_test)

# IF THE TEST STATISTIC IS GREATER THAN THE CRITICAL VALUE AT 5%, WE CANNOT REJECT THE NULL HYPOTHESIS (WHICH IS AN ASSUMPTION OF NON-STATIONARITY)


# Step 3: If necessary, utilise differencing
# if needed, apply differencing and plot 
diff_df_ts <- diff(df_ts, differences = 1) # create new object with differencing

# Plot the differenced data
plot(diff_df_ts, main="First Differenced Data", xlab="Time", ylab="Differenced Value")


# check new data with ADF
adf_test <- ur.df(diff_df_ts, type = "drift", lags = 1)

# View the summary of the test, including critical values
summary(adf_test)

# Step 4: Decompose Time-Series after Stationarity Achievement
# if stationarity has been achieved, decompose the time-series
# Seasonal and Trend Decomposition
decomposed_data <- decompose(diff_df_ts)
plot(decomposed_data)

