# libraries
library(datasets)
library(forecast)
library(urca)
library(car)
library(itsmr)
library(aTSA)
library(ASTSA)


AirPassengers
plot.ts(AirPassengers,type="o")

#Transformation 
L=powerTransform(AirPassengers)
plot.ts(log(AirPassengers),type="o")
acf(log(AirPassengers),lag.max = 50)
plota(log(AirPassengers))

# Seasonality Elimination
d12=diff(x = log(AirPassengers),lag = 12,differences = 1)
plot.ts(d12)

# Trend Elimination
d1=diff(x = d12,lag = 1,differences = 1)
plot.ts(d1)

(1-B)(1-B^12)Xt = Yt
# test stationary
summary(adf.test(d1))
test(d1)
# Possible values of p,q, P, Q
#p:0,1,3,9
#P:0,1
#q:0,1,3,9
#Q:0,1

plota(d1)
# fitting
# SARIMA (p,d,q)x(P,D,Q)s
Xmc=scale(log(AirPassengers),center = T,scale = F)
mean(log(AirPassengers))

fit = arima(log(AirPassengers),
            order = c(0,1,1),
            seasonal = list(order=c(0,1,1),period=12))
test(fit$residuals)
confint(fit)


# Autofit
library(forecast)
bestmodel=auto.arima(log(AirPassengers),trace = TRUE)
test(bestmodel$residuals)
# (1-B)(1-B^12)log(AirPass)= (1-0.402B)(1-.557B^12)Zt

# Forecasting
forecast(object = fit,h =5)
aTSA::forecast(object = fit,lead = 5)


# DATA deaths
#itsmr package
deaths
plot.ts(deaths)
plota(deaths)

d12=diff(x = deaths,lag = 12,differences = 1)
plot.ts(d12)
# Trend Elimination
d1=diff(x = d12,lag = 1,differences = 1)
plot.ts(d1)
summary(adf.test(d1))
plota(d1)
test(d1)

d=d1-mean(d1)
d
# fitting
fit = arima(deaths-mean(deaths), c(0,1,0),seasonal = list(order = c(1, 1, 1), period = 12))
test(fit$residuals)

bestmodeld=auto.arima(deaths, trace = T)


# Confidence interval of the coefficients
fit$coef+(1.96*sqrt(diag(fit$var.coef)))
fit$coef-(1.96*sqrt(diag(fit$var.coef)))

confint(fit)

aTSA::forecast(object = fit,lead = 10)

