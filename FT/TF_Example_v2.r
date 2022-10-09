#################################################################################
##############        Transfer Function Examples     ############################
##############     ------------------------------    ############################
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

#### SIMULATE SERIES with random x -------------------------------------------------------
# Generate random explanatory variable
N <- 500
x1 <- rnorm(N)
x1 <- ts(x1)
ggtsdisplay(x1,lag.max = 100, main = "x1")

#### Read autocorrelated x -------------------------------------------------------
# Generate random explanatory variable
x2 <- read.table("TEMP.dat", sep = "", header = TRUE)
x2 <- ts(x2$TEMP)
ggtsdisplay(x2,lag.max = 100, main = "x2")

# Simulate ARMA noise
set.seed(100)
ar_noise <- arima.sim(n = N, 
                    list(ar = c(0.8)),
                    sd = sqrt(0.25))
ggtsdisplay(ar_noise,lag.max = 100, main = "noise")

# Create outputs
y1 <- 0.7*x1 + ar_noise
y2 <- 0.7*x2 + ar_noise
y3 <- 70 *x1 + ar_noise

# Create data.frames with variables
fdata1 <- data.frame(x=as.numeric(x1), y=as.numeric(y1), n=as.numeric(ar_noise))
fdata2 <- data.frame(x=as.numeric(x2), y=as.numeric(y2), n=as.numeric(ar_noise))
fdata3 <- data.frame(x=as.numeric(x1), y=as.numeric(y3), n=as.numeric(ar_noise))

#Plot series and ACF
autoplot( ts(fdata1), facets = TRUE)
autoplot( ts(fdata2), facets = TRUE)
autoplot( ts(fdata3), facets = TRUE)

ggtsdisplay(y1,lag.max = 100, main = "y1")
ggtsdisplay(y2,lag.max = 100, main = "y2")
ggtsdisplay(y3,lag.max = 100, main = "y3")

# Cross-correlation function
x1_lag1 <- Lag(x1, shift = 1)
x1_lag1[is.na(x1_lag1)] <- 0
ccf(y = x1, x = x1_lag1)

#### Cross correlation y - x
ccf(y = y1, x = x1)
ccf(y = y3, x = x1)
ccf(y = y2, x = x2)


#### Fit regression model ---------------------------------------
lm1.fit <- lm(y ~ x -1, data = fdata1 )
summary(lm1.fit)
CheckResiduals.ICAI(lm1.fit, lag=100)
ccf(y = residuals(lm1.fit), x = x1)

lm2.fit <- lm(y ~ x -1, data = fdata2 )
summary(lm2.fit)
CheckResiduals.ICAI(lm2.fit, lag=100)
ccf(y = residuals(lm2.fit), x = x2)

#### Fit arima noise with regression 
arima1.fit <- arima(y1,
                   order=c(1,0,0),
                   #seasonal = list(order=c(0,0,0),period=24),
                   xtransf = x1,
                   transfer = list(c(0,0)),
                   include.mean = FALSE,
                   method="ML")

summary(arima1.fit) # summary of training errors and estimated coefficients
coeftest(arima1.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima1.fit, lag=100)
ccf(y = residuals(arima1.fit), x = x1)

arima2.fit <- arima(y2,
                    order=c(1,0,0),
                    #seasonal = list(order=c(0,0,0),period=24),
                    xtransf = x2,
                    transfer = list(c(0,0)),
                    include.mean = FALSE,
                    method="ML")

summary(arima2.fit) # summary of training errors and estimated coefficients
coeftest(arima2.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima2.fit, lag=100)
ccf(y = residuals(arima2.fit), x = x2)



