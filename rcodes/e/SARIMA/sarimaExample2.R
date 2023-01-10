library("ltsa")
library("itsmr")
library("astsa")
library("car")
library("urca")
library("fpp")
library(forecast)

ausbeer

plot.ts(ausbeer)
summary(ur.df(ausbeer,type="trend"))










sunspot.month
plot.ts(sunspot.month)
d=diff(sunspot.month)
plota(d)
plot.ts(d)


model=auto.arima(d,trace = T)


model2=arima(sunspot.month,order = c(2,1,2), seasonal = list(order=c(0,0,1),period=12))
test(model2$residuals)



dat=window(sunspot.month,start=1891,end=1990)



