
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

# differencing operator
ds= diff(wine,lag = 12,differences = 1)
dt= diff(ds,lag = 1,differences = 1)
MM=c("diff",1,"diff",12)
a = autofit(dt-mean(dt),p=0:3,q=0:3)
modela = specify(ar=a$phi,ma = a$theta,sigma2 = a$sigma2)
test(Resid(wine,MM,modela))


