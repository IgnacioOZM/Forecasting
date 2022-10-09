#################################################################################
##############      Lab 4.6:   Transfer Function     ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(TSA)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(Hmisc) # for computing lagged variables

## Set working directory ---------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("TFS1.dat",header = TRUE, sep = "")
# Convert to time series object
fdata_ts <- ts(fdata)
autoplot(fdata_ts, facets = TRUE)
ggtsdisplay(fdata_ts[,2], lag=100)

# Create time series and scale values 
#Non-seasonal time series
#x <- ts(fdata$x)/1
#y <- ts(fdata$y)/1
#Seasonal time series
x <- ts(fdata$x,frequency = 12)/1
y <- ts(fdata$y, frequency = 12)/1
ggtsdisplay(y, lag=100)


## Identification and fitting process -------------------------------------------------------------------------------------------------------

#### Fit initial FT model with large s
# This arima function belongs to the TSA package
TF.fit <- arima(y,
                order=c(1,0,0),
                seasonal = list(order=c(1,0,0),period=12),
                xtransf = x,
                transfer = list(c(0,15)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.

# Check numerator coefficients of explanatory variable
TF.Identification.plot(x,TF.fit)


#### Fit arima noise with selected
xlag = Lag(x,0)   # b
xlag[is.na(xlag)]=0
arima.fit <- arima(y,
                   order=c(0,0,0),
                   seasonal = list(order=c(0,0,0),period=12),
                   xtransf = xlag,
                   transfer = list(c(0,0)), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima.fit, lag=50)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 50)


### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x)
########

# Check fitted
autoplot(y, series = "Real")+
  forecast::autolayer(fitted(arima.fit), series = "Fitted")

# Perform future forecast
x_fut <- x[1:12]
val.forecast_h12 <- TF.forecast(y.old = as.matrix(y), #past values of the series
                                x.old = as.matrix(xlag), #Past values of the explanatory variables
                                x.new = as.matrix(x_fut), #New values of the explanatory variables
                                model = arima.fit, #fitted transfer function model
                                h=12) #Forecast horizon
val.forecast_h12

