#################################################################################
##############      Lab 8: Transfer Function         ############################
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
x <-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(x)
rm(list = ls())
cat("\014")

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("Nonseasonal_TF.dat",header = TRUE, sep = "")
colnames(fdata) <- c("Y","X")
# Convert to time series object
fdata_ts <- ts(fdata)
autoplot(fdata_ts, facets = TRUE)
# Create time series and scale values 
y <- fdata_ts[,1]/10000
x <- fdata_ts[,2]/10000



## Identification and fitting process -------------------------------------------------------------------------------------------------------

#### Fit initial FT model with large s
# This arima function belongs to the TSA package
TF.fit <- arima(y,
                order=c(1,0,0),
                #seasonal = list(order=c(1,0,0),period=24),
                xtransf = x,
                transfer = list(c(0,9)), #List with (r,s) orders
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
                   order=c(0,0,1),
                   #seasonal = list(order=c(0,0,0),period=24),
                   xtransf = xlag,
                   transfer = list(c(2,0)), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima.fit, lag=25)

### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x)
########

# Check fitted
autoplot(y, series = "Real")+
  forecast::autolayer(fitted(arima.fit), series = "Fitted")

