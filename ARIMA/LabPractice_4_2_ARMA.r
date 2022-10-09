#################################################################################
##############       Forecasting:      ARMA          ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest) #contains coeftest function

## Set working directory -------------------------------------------------------------------------
x <-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(x)
rm(list = ls())
cat("\014")

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read_excel("ARMA_series.xls")
# Convert to time series object
fdata_ts <- ts(fdata)
# index to select a time series
y <- fdata_ts[,2]


## Identification and fitting frocess -------------------------------------------------------------------------------------------------------
# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(y,lag.max = 25)
# Fit model with estimated order=c(AR,0,MA)
arima.fit <- Arima(y, order=c(0,0,1), include.mean = FALSE)
summary(arima.fit) # summary of training errors and estimated coefficients
# Si pvalor menor de 0.05 el coeficiente es muy significativo.
coeftest(arima.fit) # statistical significance of estimated coefficients
autoplot(arima.fit) # root plot

# Check residuals
# Si pvalor es mayor de 0.05 damos por bueno el modelo porque no podemos rechazar la hipótesis de ruido blanco.
CheckResiduals.ICAI(arima.fit, bins = 100, lag=20)

# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 25)

#######

# Check fitted forecast
autoplot(y, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")

#Perform future forecast
y_est <- forecast(arima.fit, h=5)
autoplot(y_est)


## Simulate ARMA time series -------------------------------------------------------------------------------------------------------
sim_ts <- arima.sim(n = 250, 
                 list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
                 sd = sqrt(0.1796))
