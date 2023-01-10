library("car")
library("ltsa")
library("itsmr")
library("astsa")


# Example Quraterly U.S. GNP Gross National Product
plot(gnp,type="o")
plota(gnp)


pt=powerTransform(gnp)
d=(gnp^(pt$lambda)-1)/pt$lambda
plot.ts(d)

d=log(gnp)-mean(log(gnp))

M=c("log","diff",1)
e = Resid(gnp,M)
plot.ts(e)
test(e)

# Test stationary
library(aTSA)
adf.test(e)

#ARMA(2,1)
a1=arima(x = e,order = c(2,0,1))
itsmr::test(a1$residuals)


abest=auto.arima(d)
test(abest)


#AR(1)
a2=arima(x = e,order = c(1,0,0),include.mean = F)
test(a2$residuals)


#ARMA(1,1)
a3=arima(x = log(gnp),order = c(0,1,2),include.mean = F)
test(a3$residuals)



library(forecast)
autoa=auto.arima(y = log(gnp))
test(autoa$residuals)





# Example Quarterly Time Series of the Number of Australian Residents


plot.ts(austres, type="o")
plota(austres)
     

M=c("diff",1)
e = Resid(austres,M)
test(e)

adf.test(e)

d=austres-mean(austres)
a1=arima(x = d,order = c(2,1,2),include.mean = F)
test(a1$residuals)




library(forecast)
mm=auto.arima(y = austres)







#Wine data
plot(wine, type="o")
powerTransform(wine) # need car package
plota(wine)


M = c("log","diff",12)
e = Resid(wine,M)
plot.ts(e, type="o")
test(e)

# Test stationary
library(aTSA)
adf.test(e)


a = arma(e,p = 12,q = 0)

model=specify(ar = c(a$phi) , ma=c(a$theta), sigma2 = a$sigma2)
rm=Resid(wine,M=M,model)
test(rm)

# Approximate 95% confidence intervals
CI=cbind(a$phi-(1.96*a$se.phi),a$phi+(1.96*a$se.phi))
CI

aopt=arima(e,order = c(12,0,0),fixed = c(NA,0,0,0,NA,0,0,0,0,0,0,NA,NA), transform.pars = FALSE)
model=specify(ar = c(aopt$coef[1:12]) ,ma=0, sigma2 = aopt$sigma2)
rm=Resid(e,M=NULL,model)
test(aopt$residuals)

