#Wine data
plot.ts(wine, type="o")
# Trend; seasonality; change in the variance
L=powerTransform(wine) # need car package
L$roundlam
Twine=(wine^0.5 -1)/0.5


plot.ts(Twine)
plota(Twine)
M = c("log","diff",12)
e = Resid(wine,M)
plot.ts(e, type="o")
test(e)


a = arma(e,p = 12,q = 0)

model=specify(ar = c(a$phi) , ma=c(a$theta), sigma2 = a$sigma2)
rm=Resid(e,M=NULL,model)
test(rm)

a$phi+(1.96*a$se.phi)



aopt=arima(e,order = c(12,0,0),fixed = c(NA,0,0,0,NA,0,0,NA,0,0,NA,NA,NA), transform.pars = FALSE)
model=specify(ar = c(aopt$coef[1:12]) ,ma=0, sigma2 = aopt$sigma2)
rm=Resid(e,M=NULL,model)
test(rm)




# test for unit roots
library(urca)
library(aTSA) # ADF test

plota(wine)
plot.ts(wine, type="o")
powerTransform(wine)
d=(wine^(0.413)-1)/0.413
plot.ts(d)
plota(d)
M = c("diff",12,"diff",1)
e = Resid(d,M)
#e=e-mean(e)
plot.ts(e, type="o")
test(e)

t=adf.test(e)

autofit(e,p = 0:3,q=0:1)




# example

m1=arima.sim(model = list(ar=c(0.8),order=c(1,1,0)),n = 200)
par(mfrow=c(1,1))
plot.ts(m1,type="o")
acf(m1, lag.max = 40)


library(aTSA)
adf.test(diff(m1))
