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
fdata<-read.table("ARIMA_1.dat", sep = "")
TT=12
y <- ts(fdata, frequency = TT)
ggtsdisplay(y,lag.max = 50)

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y,TT)
z <- BoxCox(y,Lambda)

# Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(z,lag.max = 100)

# If differencing is needed
Bz <- diff(z,differences = 1)
ggtsdisplay(Bz,lag.max = 100) #differences contains the order of differentiation

# Seasonal Differentiation
# If differencing is needed
B12Bz <- diff(Bz, lag = TT, differences = 1)
B12 <- diff(z, lag = TT, differences = 1)

# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(B12Bz,lag.max = 100)
ggtsdisplay(B12,lag.max = 100)


# Fit seasonal model with estimated order
arima.fit <- Arima(y,
                   order=c(2,1,0),
                   seasonal = list(order=c(0,0,1), period=TT),
                   lambda = Lambda,
                   include.constant = FALSE)
#summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
autoplot(arima.fit) # root plot
# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100, lag=120)


# Check fitted forecast
autoplot(y, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")


# Perform future forecast
y_est <- forecast(arima.fit, h=12)
autoplot(y_est)
