#################################################################################
##############      Lab 8:   Transfer Function       ############################
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
fdata <- read.table("Seasonal_TF_1.dat",header = TRUE, sep = "")
colnames(fdata) <- c("Y","X")
# Convert to time series object
fdata_ts <- ts(fdata, frequency = 24)
autoplot(fdata_ts, facets = TRUE)
# Create time series and scale values 
y <- fdata_ts[,1]/10000
x <- fdata_ts[,2]/10000

ggtsdisplay(y, lag=120)


## Identification and fitting process -------------------------------------------------------------------------------------------------------

#### Fit initial FT model with large s
# This arima function belongs to the TSA package
TF.fit <- arima(y,
                order=c(1,0,0),
                seasonal = list(order=c(1,0,0),period=24),
                xtransf = x,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 200)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.

# Check numerator coefficients of explanatory variable
TF.Identification.plot(x,TF.fit)


#### Fit arima noise with selected
xlag = Lag(x,0)   # b
xlag[is.na(xlag)]=0
arima.fit <- arima(y,
                   order=c(0,0,1),
                   seasonal = list(order=c(1,1,0),period=24),
                   xtransf = xlag,
                   transfer = list(c(0,0)), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima.fit, lag=75)


### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x)
########

# Check fitted
autoplot(y, series = "Real")+
  forecast::autolayer(fitted(arima.fit), series = "Fitted")

# Perform future forecast
x_fut <- x[1:24]
val.forecast_h24 <- TF.forecast(y.old = as.matrix(y), #past values of the series
                                x.old = as.matrix(xlag), #Past values of the explanatory variables
                                x.new = as.matrix(x_fut), #New values of the explanatory variables
                                model = arima.fit, #fitted transfer function model
                                h=24) #Forecast horizon
val.forecast_h24
