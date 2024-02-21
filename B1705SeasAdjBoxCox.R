# ----- B1705 Week 6 | Seasonal Adjustment and Box Cox Transformation | 20.02.2024 -----
# ----- Seasonal Adjustment Tutorial -----

##### Step 1: Start with an empty workspace #####
rm(list=ls())

##### Step 2: Install and load seasonal package #####
library(seasonal)

##### Step 3: Load and Plot Dataset #####
# use the R dataset AirPassengers. This dataset 
# contains the classic Box & Jenkins airline data. 
# Monthly totals of international airline 
# passengers, from 1949 to 1960.
plot(AirPassengers)

##### Step 4: Seasonal Adjust the AirPassengers dataset #####
# seasonal adjust time series using X11. 
sa_series <- seas(AirPassengers,x11 = "")

##### Step 5: Plot the 'original' and seasonally adjusted series #####
# plot original and seasonally adjusted series.
plot(AirPassengers)
lines(final(sa_series),col=2)

# ----- Box Cox Transformation Tutorial -----

# We use the Box Cox Transformation to improve the accuracy of predictions made using linear regression

##### Step 1: Load Libraries and Create Dataset #####
library(MASS)

#create data
y=c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 6, 7, 8)
x=c(7, 7, 8, 3, 2, 4, 4, 6, 6, 7, 5, 3, 3, 5, 8)

##### Step 2: Fit Linear Regression model #####
#fit linear regression model
model <- lm(y~x)

##### Step 3: Find optimal lambda for Box-Cox transformation #####
bc <- boxcox(y ~ x)
(lambda <- bc$x[which.max(bc$y)])


##### Step 4: Fit new linear regression model using the Box-Cox transformation #####
new_model <- lm(((y^lambda-1)/lambda) ~ x)


##### Step 5: Defining plot parameters #####
op <- par(pty = "s", mfrow = c(1, 2))


##### Step 6: Creating our Q-Q plot of original model #####
qqnorm(model$residuals)
qqline(model$residuals)

##### Step 7: Creating our Q-Q plot of Box-Cox transformed model
qqnorm(new_model$residuals)
qqline(new_model$residuals)

#As a rule of thumb, if the data points fall along a straight 
# diagonal line in a Q-Q plot then the dataset likely follows a normal distribution.

#Notice how the box-cox transformed model produces 
# a Q-Q plot with a much straighter line than the original regression model.

#This is an indication that the residuals of the box-cox transformed
# model are much more normally distributed, which satisfies one of the assumptions of linear regression.






#display both Q-Q plots
par(op)













