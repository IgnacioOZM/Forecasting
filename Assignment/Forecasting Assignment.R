#################################################################################
##############       Forecasting:    Assignment      ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest) #contains coeftest function
library(tseries) #contains adf.test function
library(TSA)
library(NeuralSens)
library(caret)
library(kernlab)
library(nnet)
library(NeuralNetTools)

## Set working directory -------------------------------------------------------
x <-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(x)
rm(list = ls())
cat("\014")

## Load dataset ----------------------------------------------------------------
fdata <- read.csv("DailyPrice_Load_Wind_Spain_2019_2020.csv",sep = ";")
fdata_tr <- fdata[fdata$Date <= '2020-09-30',]
fdata_ts <- fdata[fdata$Date > '2020-09-30',]

# Convert to time series object
TT = 7
y_tr <- ts(fdata_tr$Price, start = 2019, frequency = 365)
y <- ts(fdata$Price, start = 2019, frequency = 365)
x_tr <- ts(fdata_tr$Wind, start = 2019, frequency = 365)
x <- ts(fdata$Wind, start = 2019, frequency = 365)
x_ts <- ts(fdata_ts$Wind, start = 2020, frequency = 365)

#Plot time series
autoplot(y_tr) +
  ggtitle("Price of electricity") +
  xlab("Year") + ylab("Price")
# Plot ACF % PACF
ggtsdisplay(y_tr,lag.max = 50)

## Analysis dataset-------------------------------------------------------------
# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y_tr,TT)
z <- BoxCox(y,Lambda)

# Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(z,lag.max = 100)

# If differencing is needed
Bz <- diff(z,differences = 1)
ggtsdisplay(Bz,lag.max = 100) #differences contains the order of differentiation


# Seasonal Differentiation
# If differencing is needed
B7Bz <- diff(Bz, lag = TT, differences = 1)
B7 <- diff(z, lag = TT, differences = 1)
ggtsdisplay(B7Bz,lag.max = 100)
ggtsdisplay(B7,lag.max = 100)


#################################################################################
################################    SARIMA   ####################################
#################################################################################

# Fit seasonal model with estimated order
arima.fit <- Arima(y_tr,
                   order=c(1,1,2),
                   seasonal = list(order=c(0,1,1), period=TT),
                   include.constant = FALSE)
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
autoplot(arima.fit) # root plot
# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100, lag=100)
ggtsdisplay(residuals(arima.fit),lag.max = 100)

# Forecast with ARIMA
y.SA.est <- y*NA
for (i in seq(length(y_tr)+1, length(y), 1)){# loop for validation period
  y.SA.est[i] <- forecast(subset(y,end=i-1), # y series up to sample i
                          model = arima.fit, # Model trained (Also valid for exponential smoothing models)
                          h=92)$mean # h is the forecast horizon
}

#Plot series and forecast
autoplot(ts(y[600:730]))+
  forecast::autolayer(ts(y.SA.est[600:730]))

#Error measurements
accuracy(fdata_ts[,4],y.SA.est[640:731])

#################################################################################
###########################    Transfer function   ##############################
#################################################################################

#### Fit initial FT model with large s
# This arima function belongs to the TSA package
xlag = Lag(x_tr,0)   # b
xlag[is.na(xlag)]=0
set.seed(150)
TF.fit <- arima(y_tr,
                order=c(1,1,1),
                seasonal = list(order=c(0,1,1),period=TT),
                xtransf = xlag,
                transfer = list(c(0,1)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
CheckResiduals.ICAI(TF.fit, lag=75)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.

# Check numerator coefficients of explanatory variable
TF.Identification.plot(x_tr,TF.fit)

# Perform future forecast
x_fut <- x[1:92]
val.forecast_h92 <- TF.forecast(y.old = as.matrix(y_tr), #past values of the series
                                x.old = as.matrix(x_tr), #Past values of the explanatory variables
                                x.new = as.matrix(x_ts), #New values of the explanatory variables
                                model = TF.fit, #fitted transfer function model
                                h=92) #Forecast horizon
val.forecast_h92

y.TF.est <- y*NA
for (i in seq(length(y_tr)+1, length(y), 1)){# loop for validation period
  y.TF.est[i] <- val.forecast_h92[i-length(y_tr)]
}

#Plot series and forecast
autoplot(ts(y[600:730]))+
  forecast::autolayer(ts(y.TF.est[600:730]))

#Error measurements
accuracy(fdata_ts[,4],y.TF.est[640:731])
  

#################################################################################
##################################    MLP   #####################################
#################################################################################

###Include lagged variables
#This can be done using the lag() function from the stats package but it works with time series objects
#The code is cleaner using the Lag() function from Hmisc package
library(Hmisc)
fdata_tr$WD_lag1 <- Lag(fdata_tr$Wind,1)
fdata_tr$Price_lag1 <- Lag(fdata_tr$Price,1)
fdata_tr$DEM_lag1 <- Lag(fdata_tr$Demand,1)

#Remove missing values
fdata_tr <- na.omit(fdata_tr)

## Initialize trainControl -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          savePredictions = TRUE)              #save predictions


#------------ Neural network --------------------------------
set.seed(150) #For replication
mlp.fit = train(form = Price~Demand+Wind+WD_lag1+Price_lag1+DEM_lag1, #Use formula method to account for categorical variables
                data = fdata_tr, 
                method = "nnet",
                linout = TRUE,
                # tuneGrid = data.frame(size =5, decay = 0),
                tuneGrid = expand.grid(size = seq(2,5,length.out = 4), decay=c(10^(-7),10^(-6),10^(-5),0.0001,0.001,0.01,0.1,1,10)),
                maxit = 3000,
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
mlp.fit #information about the resampling settings
ggplot(mlp.fit)+scale_x_log10()
plotnet(mlp.fit$finalModel) #Plot the network
SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis


#Predict training data
mlp_pred = predict(mlp.fit,  newdata = fdata_tr)  

PlotModelDiagnosis(fdata_tr[,-1], fdata_tr[,4], mlp_pred, together = TRUE)

#Error measurements
accuracy(fdata_tr[,4],mlp_pred)

plot(fdata_tr[,4],type="l")
lines(mlp_pred,col = "red")


## Forecast for new data with h = 90
fdata <- read.csv("DailyPrice_Load_Wind_Spain_2019_2020.csv",sep = ";")
fdata_tr <- fdata[fdata$Date <= '2020-09-30',]
fdata_ts <- fdata[fdata$Date > '2020-09-30',]

fdata.Reg.new <- fdata_ts[,c(2,3,4)]

#create auxiliary output variable
fdata.Reg.new$Price <- rep(NA,length(fdata.Reg.new[,1]))


#join the datasets
fdata.join <- rbind(fdata_tr[,c(2:4)],fdata.Reg.new)

#create lagged variables
fdata.join$WD_lag1 <- Lag(fdata.join$Wind,1)
fdata.join$Price_lag1 <- Lag(fdata.join$Price,1)
fdata.join$DEM_lag1 <- Lag(fdata.join$Demand,1)



#loop for forecasting
tstart = dim(fdata_tr)[1]
for (i in (tstart+1):(tstart+92)){
  #Predict and substitute in output variable so it can be used in following forecasts
  fdata.join$Price[i] <- predict(mlp.fit,  newdata = fdata.join[i,]) 
  #Recalculate lagged variables using the estimated value
  fdata.join$Price_lag1 <- Lag(fdata.join$Price,1)
}

#forecasts
fdata.join$Price[(tstart+1):(tstart+92)]

y.MLP.est <- y*NA
for (i in seq(length(y_tr)+1, length(y), 1)){# loop for validation period
  y.MLP.est[i] <- fdata.join$Price[i]
}

#Plot series and forecast
autoplot(ts(y[600:730]))+
  forecast::autolayer(ts(y.MLP.est[600:730]))

autoplot(y)+
  forecast::autolayer(y.MLP.est)

#Error measurements
accuracy(fdata_ts[,4],y.MLP.est[640:731])

#### Comparison
autoplot(ts(y[600:730]))+
  forecast::autolayer(ts(y.SA.est[600:730]))+
  forecast::autolayer(ts(y.TF.est[600:730]))+
  forecast::autolayer(ts(y.MLP.est[600:730]))

