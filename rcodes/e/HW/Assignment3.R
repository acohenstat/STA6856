library(itsmr)
library(astsa)
library(aTSA)
library(tidyverse)
library(forecast)

data("birth")

# plot the data 
plot.ts(birth,type="o")
plota(birth)
R=diff(diff(birth,lag = 12))
mean(R)
R= R- mean(R)

test(R)

adf.test(R)

# auto.arima
birthmc = birth - mean(birth)
bestmodel=auto.arima(birthmc, trace = T)
test(bestmodel$residuals)
confint(bestmodel)

# picking some models
#p  = 0,1,2,3,4,11
#P	= 0,1,2,3
#q	= 0,1,9,11
#Q	= 0,1,2,3
models = arima(birthmc,
               order =  c(1,1,1),
               seasonal = list(order = c(0, 1, 1), period = 12)) 
models
test(models$residuals)
confint(models)

########################
# p d q P D Q residuals iid  AIC
#-----------------------
# 0 1 2 1 1 1  Yes          2419.86 - AutoArima()
# 0 1 2 0 1 1  Yes          2419.87
# 0 1 0 0 1 1  No           2472.2
# 1 1 1 1 1 1  Yes          2419.66 - Best
# 0 0 2 2 0 0  No           2720.24
# 0 1 2 2 1 0  Yes          2458.26

# Forecasting
# Training and Testing series sets
datatrain= stats::window(birth, start =c(1948,1),end = c(1978,7)) 
plot.ts(datatrain,type="o")

datatest= stats::window(birth, start =c(1978,8))
plot.ts(datatest,type="o")

models = arima(datatrain,order =  c(1,1,1),seasonal = list(order = c(0, 1, 1), period = 12)) 
models
test(models$residuals)
confint(models)

bestmodeltrain=auto.arima(datatrain, trace = T)
test(bestmodeltrain$residuals)
confint(bestmodeltrain)


#Forecast
fsarima=aTSA::forecast(models,lead =6)

library(Metrics)
mapeSARIMA=mape(datatest,fsarima[,2])*100

# The ARAR Algorithm
ararPrd=arar(datatrain,h=6,opt=1)
ARARMODEL=mape(datatest,ararPrd$pred)*100

# The Holt-Winters Algorithm
HoltPrd=HoltWinters(datatrain,seasonal = "add")
fhw=forecast::forecast(HoltPrd,6)
HWMODEL=mape(datatest,fhw$mean)*100

c(mapeSARIMA,ARARMODEL,HWMODEL)



