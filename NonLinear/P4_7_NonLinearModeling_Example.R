#################################################################################
##############      Lab 8: Nonlinear forecasting     ############################
##############       Nonlinear modeling example      ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(ggplot2)

#Generate sinthetic data
Temp <- 15+rnorm(500)*2
y <- Temp^2-30*Temp+250+rnorm(500)
dt <- data.frame(y=y,Temp=Temp)
ggplot(dt)+geom_point(aes(x=Temp, y=y))


#Fit regression model
lm.fit <- lm(y~Temp)
dt <- cbind(dt,fit0=lm.fit$fitted.values)
ggplot(dt)+geom_point(aes(x=Temp, y=y))+geom_point(aes(x=Temp, y=fit0),color="blue")



#Divide temperature
T.cold <- sapply(Temp,min,14)
T.hot <- sapply(Temp,max,16)
dt <- cbind(dt,T.cold,T.hot)
ggplot(dt)+geom_point(aes(x=Temp, y=T.cold), color="red")+geom_point(aes(x=Temp, y=T.hot),color="blue")


#Fit regression model
lm.fit <- lm(y~T.cold+T.hot,dt)
dt <- cbind(dt,fit=lm.fit$fitted.values)
ggplot(dt)+geom_point(aes(x=Temp, y=y))+geom_point(aes(x=Temp, y=fit),color="blue")


#Add complexity
T.verycold <- sapply(Temp,min,12)
dt <- cbind(dt,T.verycold)
ggplot(dt)+
  geom_point(aes(x=Temp, y=T.cold), color="red")+
  geom_point(aes(x=Temp, y=T.hot),  color="blue") + 
  geom_point(aes(x=Temp, y=T.verycold), color="green")

lm.fit <- lm(y~T.cold+T.hot+T.verycold,dt)
dt <- cbind(dt,fit2=lm.fit$fitted.values)
ggplot(dt)+geom_point(aes(x=Temp, y=y))+geom_point(aes(x=Temp, y=fit2),color="blue")

