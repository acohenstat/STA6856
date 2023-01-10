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
a=arima(dff$Temp,order = c(2,0,3),xreg = cbind(dff$Ozone,dff$Wind),include.mean = TRUE)
a
test(a$residuals)

########################
# p d q residuals iid |
#-----------------------
# 2 1 0 No
# 4 0 1 No
# 2 0 2 No
# 2 0 3 Yes - AIC=942.6 - best
# 2 0 6 Yes - AIC=945.88
# 3 0 3 Yes - AIC=942.64
# 0 1 1 No
adf.test(md$residuals)
plot(md)

confint(a)


amodel= auto.arima(dff$Temp,xreg = cbind(dff$Ozone,dff$Wind),include.mean = TRUE,trace = T)
