library(itsmr)

# airpass
??airpass
# plotting
plotc(airpass)
# Upward Trend
# Seasonality 
# Variance changes

# data model: Xt = mt + St + Yt


# ACF
plota(airpass)

# Estimate Trends
m = smooth.ma(airpass,5) # moving average filter
mreg = trend(airpass,1) # polynomial regression
plot(airpass,type="l")
lines(m,col="red")
lines(mreg,col="green")

# Estimate Season
airpass.s = season(airpass,d=12)
plotc(airpass,airpass.s)

# Find residuals and Test iid hypothesis
plotc(log(airpass))
# data model
M=c("log","season",12,"trend",1)
y = Resid(airpass,M)
test(y)

# Eliminate Components using Differencing Operator
mean(diff(diff(airpass,lag = 12))) # Nabla_12 Nabla X_t
ydiff = scale(diff(diff(airpass,lag = 12)),center=TRUE,scale=FALSE)
test(ydiff)

## Fit ARMA(p,q)
# log(X_t) - trend - seasonality = Yt
plota(y)
# AR(p) : look at PACF: p=0,1
# MA(q): look at ACF: q=0,1,...,5

model = arma(y,p = 1,q = 0)
modelbest = autofit(y)
# ARMA(3,2)
# Yt = 0.05Yt-1 -0.16Yt-2 +  0.65Yt-3 + 0.62Zt-1 + 0.85Zt-2 + Zt
armamodel =specify(ar=modelbest$phi,ma = modelbest$theta,sigma2 = modelbest$sigma2)

armaR = Resid(y,M = NULL,a = armamodel)
test(armaR)

######### LAKE
plotc(lake)
# no obvious seasonality or variance changes
# downward trend! straight line
# positive dependence

plota(lake)

# Estimate 
m.lake = smooth.ma(lake,9) # moving average filter
mreg.lake = trend(lake,1) # polynomial regression
plot(lake,type="l")
lines(m.lake,col="red")
lines(mreg.lake,col="green")

# Differencing Operator
plotc(diff(lake))

# compute Residuals
R.Lake = Resid(lake,M=c("trend",1))
test(R.Lake)

test(diff(lake))
# NABLA Xt = Xt - Xt-1 = Zt
# Xt = Xt_1 + Zt -- AR(1)


lakebestmodel = autofit(R.Lake)
# Lake model
#  Xt - trend  = Xt-1 - 0.29Xt-2 + Zt
armalake =specify(ar=lakebestmodel$phi,sigma2 = lakebestmodel$sigma2)
check(armalake)
armaRlake = Resid(lake,M = c("trend",1),a = armamodel)
test(armaRlake)
