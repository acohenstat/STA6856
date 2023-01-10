library("ltsa")
library("itsmr")
library("astsa")
library("car")
library("urca")
library("forecast")
library(aTSA)

JohnsonJohnson = as.numeric(JohnsonJohnson)
plot.ts((JohnsonJohnson),type="o")
plota((JohnsonJohnson))

library(car)
L=powerTransform(JohnsonJohnson)

JJ=log(JohnsonJohnson)-mean(log(JohnsonJohnson))

plota(JJ)
# Training and Testing series sets
datatrain= stats::window(JJ, start =c(1960,1),end = c(1977,4)) 
plot.ts(datatrain,type="o")

datatest= stats::window(JJ, start =c(1978,1))
plot.ts(datatest,type="o")

# Fit a SARIMA Model
test(diff(diff(datatrain,differences = 1, lag = 4)))
adf.test(diff(diff(datatrain,differences = 1, lag = 4)))
# p=0,1
# q=0,1
# P=0,1,6
# Q=0,1
a=arima(datatrain,order = c(0,1,1), seasonal = list(order=c(0,1,0),period=4))
confint(a)
test(a$residuals)
a

a1=auto.arima(datatrain, trace = T,allowdrift = FALSE)
confint(a1)
plot.ts(a$residuals)
test(a1$residuals)
a1=arima(datatrain,order = c(1,0,1), seasonal = list(order=c(1,1,0),period=4))
#Forecast
library(forecast)
fa = aTSA::forecast(a,lead=12)
fa1 = aTSA::forecast(a1,lead =12)

library(Metrics)
mapeA=mape(datatest,fa[,2])*100
mapeA1=mape(datatest,fa1[,2])*100

# The ARAR Algorithm
ararPrd=arar(datatrain,h=12,opt=1)
ARARMODEL=mape(datatest,ararPrd$pred)*100

# The Holt-Winters Algorithm
HoltPrd=HoltWinters(datatrain,seasonal = "add")
fhw=forecast::forecast(HoltPrd,12)
HWMODEL=mape(datatest,fhw$mean)*100

c(A=mapeA,Auto=mapeA1,ARAR=ARARMODEL,HW=HWMODEL)



library(car)
library(astsa)
library(itsmr)
library(aTSA)
library(forecast)

# Flu DATA
mydata=astsa::flu
plot.ts((mydata),type="o")


L=powerTransform(mydata)
Tflu=-1/2*(mydata^(-2)-1)

Tflu = scale(Tflu,center = T,scale = F)
plot.ts(Tflu,type="o")

plota(Tflu)
mean(Tflu)
# eliminate trend/season
dTflu=diff(diff(Tflu,lag = 12))
# iid
test(dTflu)
# stationary
adf.test(dTflu)
plota(dTflu)

#p = 3,4,5,6
#q= 0,4,8
#P= 1,
#Q= 1 

# Auto
a1=auto.arima(flu,trace = TRUE)
confint(a1)
test(a1$residuals)


a=arima(Tflu,
        order = c(5,1,3), 
        seasonal = list(order=c(1,1,1),
        period=12),
        fixed = c(NA,0,0,NA,NA,NA,NA,NA,0,NA))
confint(a)
test(a$residuals)

# SARIMA(5,1,3)(1,1,1)12

par(mfrow=c(1,1))
#Forecast
library(astsa)
f=aTSA::forecast(object = a,lead =12)

farar=arar(Tflu,h=12)

fHW=Holt(Tflu)



# Deaths
Holt(itsmr::deaths)
arar(itsmr::deaths)

