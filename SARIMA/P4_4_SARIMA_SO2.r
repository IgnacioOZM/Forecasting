#################################################################################
##############        Forecasting:     SARIMA         ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function

## Set working directory ---------------------------------------------------------------------------------------------
x <-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(x)
rm(list = ls())
cat("\014")

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read_excel("SARIMA.xls")
# Convert to time series object
y <- ts(fdata$SO2, frequency=7)
autoplot(y)
ggtsdisplay(y,lag.max = 50)

## Identification and fitting frocess -------------------------------------------------------------------------------------------------------

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y,7)
# Lambda <- BoxCox.lambda(y) #other option
z <- BoxCox(y,Lambda)
autoplot(z)

# Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(z,lag.max = 100)

# If differencing is needed
Bz <- diff(z,differences = 1)
ggtsdisplay(Bz,lag.max = 100) #differences contains the order of differentiation

# Seasonal Differentiation
# If differencing is needed
B12Bz <- diff(Bz, lag = 7, differences = 1)

# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(B12Bz,lag.max = 100)

# Fit seasonal model with estimated order
arima.fit <- Arima(y,
                   order=c(2,1,1),
                   seasonal = list(order=c(0,1,1), period=7),
                   lambda = Lambda,
                   include.constant = FALSE)
arima.fit <- Arima(y,
                   order=c(2,1,1),
                   seasonal = list(order=c(0,1,1), period=7),
                   lambda = Lambda,
                   include.constant = FALSE)
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
autoplot(arima.fit) # root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100, lag=50)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 100)


# Check fitted forecast
autoplot(y, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")


# Perform future forecast
y_est <- forecast(arima.fit, h=12)
autoplot(y_est)
