library(itsmr)

# Simulate ARIMA models
m1=arima.sim(model = list(ar=c(0.8),ma=c(),order=c(1,0,0)),n = 200)

plot.ts(m1,type="o")
acf(m1, lag.max = 40)


# Simulate ARMA models
#1. Specify the Model using this function
model=specify(ar = c(0.8), ma=c(0), sigma2 = 1) # AR(1)
check(model)
#2. generate ARMA observations with
datasim=sim(model,n = 200)
plot(datasim,type="o")
plota(datasim)
acf(datasim)
#p=2 and q=1
pacf(datasim)



# Estimation purposes
# phi=0.8
# yule-walker estimator
yw(datasim,p = 1)
# burg
burg(x = datasim,p=1)
# Innovations
ia(datasim,q = 1)
# HR
hannan(datasim,1,1)

# MLE
arma(x = datasim,p = 2,q = 0)










# fit with differencing
M=c("diff",1)
r=Resid(m1,M)
test(r)

f=arma(r,p = 1)

f=autofit(r)
tm=specify(ar=f$phi,ma=f$theta,sigma2 = f$sigma2)
rf=Resid(m1,M,tm)
test(rf)
check(tm)

# fit without differencing
M=c("trend",1)
r=Resid(m1,M)
test(r)

f1=arma(r,p = 2)

tm=specify(ar=f1$phi,ma=f1$theta,sigma2 = f1$sigma2)
rf=Resid(m1,M,tm)
test(rf)

#polyroot(c(1,-f1$phi))


#Wine data
# plotting data
plot.ts(wine, type="o")
# Trend mt + Seasonality d=12 + variance changes

### Transform Wine
library(car)
pt=powerTransform(wine)
Twine=(wine^0.5-1)/0.5
plot.ts(Twine,type="o")
####

# data model Xt = mt + St + Yt
plota(wine)
M = c("season",12,"trend",1)
e = Resid(wine,M) # returns a zero mean data:e_t = X_t - S_t - mt - mean(1.347)
plot.ts(e, type="o")
test(e)

# Residuals with a zero mean
# e_t = X_t - S_t - mt - mean(1.347)
# Xt
# X_t = 1.347 + m_t + S_t + 0.18Xt-1 + 0.18Xt-2 + Zt

# pick p and q using AICC
bestmodel = autofit(e,p = 0:2,q=0:2)
# test if Zt is iid?
modelarma = specify(ar=bestmodel$phi,ma = NULL,sigma2 = bestmodel$sigma2)
test(Resid(wine,M,modelarma))

# r = x - m - s 
library(aTSA)
adf.test(e)


test(diff(e))
diff(e,lag = 1,differences = 2)
adf.test(diff(e))
a = arima(Twine,order = c(1,1,1))
test(a$residuals)


# Example Dowj Index data
plot.ts(dowj)
plota(dowj)

test(diff(dowj))
# ARIMA(1,1,1)
a = arima(dowj,order = c(1,1,1))
test(a$residuals)
# PHI(B)(1-B)X_t = THETA(B)Z_t
# (1-0.85B)(1-B)X_t = (1-0.526B)Z_t 

Lphi=a$coef-1.96*sqrt(diag(a$var.coef))
Uphi=a$coef+1.96*sqrt(diag(a$var.coef))


