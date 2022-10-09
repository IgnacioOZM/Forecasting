#################################################################################
##############           S3 ARIMA II Example         ############################
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
fdata <- read_excel("ARIMA_II.xls")
# Convert to time series object
fdata_ts <- ts(fdata)
# index to select a time series
y <- fdata_ts[,3]

## Load custom functions ---------------------------------------------------------------
# source("ForecastingTools.R")


## Identification and fitting frocess -------------------------------------------------------------------------------------------------------
autoplot(y)

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y,10)
# Lambda <- BoxCox.lambda(y) #other option
z <- BoxCox(y,Lambda)
autoplot(z)

# Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(z,lag.max = 25)
# Alternative test
adf.test(z, alternative = "stationary") # Si el pvalor es mayor de 0.05 significa que tu serie no es estacionaria. Diferencias
ndiffs(z) # Te dice el orden de diferencia automaticamente
# If differencing is needed
Bz <- diff(z,differences = 1)  

# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(Bz,lag.max = 25)

# Fit model with estimated order
arima.fit <- Arima(y,
                   order=c(2,1,1), 
                   lambda = Lambda,
                   include.constant = FALSE)
summary(arima.fit) #summary of training errors and estimated coefficients
coeftest(arima.fit) #statistical significance of estimated coefficients
autoplot(arima.fit) #root plot
# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100) # Como el pvalor es mayor que 0.05 podemos decir que es ruido blanco
# If residuals are not white noise, change order of ARMA

#######

#Check  forecast
autoplot(y, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")

#Perform future forecast
y_est <- forecast(arima.fit, h=5)
autoplot(y_est)
autoplot(y_est, include = 100)


