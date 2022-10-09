#################################################################################
##############              Decompostition               ###########################
#################################################################################

library(fpp2)
library(ggplot2)


## Set working directory -------------------------------------------------------------------------
x <-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(x)
rm(list = ls())
cat("\014")

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("Unemployment.dat",header = TRUE, stringsAsFactors = FALSE)

#Convert to time series object
#start -> year
#frequency = 12 -> monthly data
#frequency = 3 -> quarterly data
y <- ts(fdata$TOTAL,start = 2010, frequency = 12)


#Plot time series
autoplot(y) +
  ggtitle("Unemployment in Spain") +
  xlab("Year") + ylab("Number unemployed")


#################################################################################
# Decomposition methods
#################################################################################


## Classical additive decomposition
y_dec_add <- decompose(y,type="additive")
autoplot(y_dec_add) + xlab("Year") +
  ggtitle("Classical additive decomposition")


## Classical Multiplicative decomposition
y_dec_mult <- decompose(y,type="multiplicative")
autoplot(y_dec_mult) + xlab("Year") +
  ggtitle("Classical multiplicative decomposition")


## SEATS
library(seasonal)
y_dec_seas <- seas(y)
autoplot(y_dec_seas)+ xlab("Year") +
  ggtitle("SEATS decomposition")


#Use seasonal(), trendcycle() and remainder() functions to extract the individual components.
#Use seasadj() to compute the seasonally adjusted time series.

#Compare seasonal components
autoplot(seasonal(y_dec_mult), series = "Multiplicative") +
  forecast::autolayer(seasonal(y_dec_seas),series = "SEATS")


#Compare seasonal adjustment components (i.e. substracting the seasonal component from the raw series)
autoplot(seasadj(y_dec_add), series = "Additive")+
  forecast::autolayer(seasadj(y_dec_mult), series = "Multiplicative") +
  forecast::autolayer(seasadj(y_dec_seas),series = "SEATS")

autoplot(seasadj(y_dec_seas),series = "SEATS")

#Seasonal subseries plot
ggsubseriesplot(seasonal(y_dec_add)) 
ggsubseriesplot(seasonal(y_dec_mult)) 
ggsubseriesplot(seasonal(y_dec_seas)) 

