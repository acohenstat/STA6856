#install.packages(c("rugarch","quantmod"))

library(rugarch) # run GARCH models
library(quantmod) # upload financial data from Yahoo Finance
library(itsmr)
library(aTSA)

# Google Share Price
getSymbols("GOOG", from = as.Date("2010-01-01"), to = as.Date("2020-12-31"))
op=GOOG$GOOG.Open
plot(op)

R= diff(log(as.numeric(op)))
plot(R,type="l")
plota(R)
plota(R^2)

# Specify the model
spec1 = ugarchspec()
spec1
# Mean Model  AR(1)
spec1 = ugarchspec(mean.model = list(armaOrder=c(1,0)))
spec1

# Mean Model- EWMA
spec2 = ugarchspec(mean.model = list(armaOrder=c(0,0)),
                   variance.model = list(garchOrder =c(2,0)))
spec2

# Fit GARCH
ugfit = ugarchfit(spec = spec2, data = R)
ugfit

# Forecast with GARCH
ugf = ugarchforecast(ugfit, n.ahead = 10)
ugf

plot(ugf@forecast$sigmaFor,type="l")

-5.5132
-5.5133

# nyse
library(astsa)
plot(nyse,type="o")
plota(nyse)
plota(nyse^2)

#Mean model
# p=0,1,2,5
# q=0,1,2,5
#Variance Model
# p=0,1,2,3,5,7
# q=0,1,2,3,5
spec2 = ugarchspec(mean.model = list(armaOrder=c(2,2)),
                   variance.model = list(garchOrder =c(2,1)))
spec2


# Fit GARCH
ugfit = ugarchfit(spec = spec2, data = nyse)
ugfit

#  Rt = 0.11Rt-1 + Zt
#  h_t = 0.11ht-1 + 0.81Z^2t-1
# Forecast with GARCH
ugf = ugarchforecast(ugfit, n.ahead = 10)
ugf

plot(ugf@forecast$sigmaFor,type="l")
plot(ugf@forecast$seriesFor,type="l")
