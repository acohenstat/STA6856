library("ltsa")
library("itsmr")
library("astsa")
library(car)

#Lake data
lake
plot.ts(lake,type="o")
a=arima(lake,order = c(1,0,0),xreg = time,include.mean = F)
a
test(a$residuals)
confint(a)

time=seq(1:98)
mlake= lm(lake~ time)
plot(mlake)
adf.test(mlake$residuals)



# AirQuality
df=airquality

# missing values
#LOCF: Last Observation Carried Forward
library(DescTools)
summary(df)
dff=LOCF(df)
summary(dff)

pairs(dff, panel = panel.smooth, main = "airquality data")

plota(dff$Temp)
plot(dff$Temp,type="o")

# Temp
md= lm(Temp ~ Ozone + Wind, data = dff)
summary(md)
a=arima(dff$Temp,order = c(2,1,0),xreg = cbind(dff$Ozone,dff$Wind),include.mean = TRUE)
a
adf.test(md$residuals)
confint(a)



# Sealt Belt Data
UKDriverDeaths
plot(UKDriverDeaths,type="o")

library(car)
L=powerTransform(UKDriverDeaths)
UKD=log(UKDriverDeaths)
plot.ts(UKD,type="o")
plota(UKD)

M=c("diff",12,"diff",1)
r=Resid(UKD,M)
test(r)
library(aTSA)
adf.test(r)


library(forecast)
aa=auto.arima((UKD), trace = T)

a=arima(UKD,
        order = c(3,0,1),
        seasonal = list(order = c(1, 1, 1), period = 12),
        xreg = seq(1:length(UKD)))

test(a$residuals)
confint(a)

summary(lm(UKD~seq(1:length(UKD))))








