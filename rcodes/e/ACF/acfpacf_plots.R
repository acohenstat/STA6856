# Examples of ARMA(p,q)
# ARMA(1,1) : X_t - 0.5X_t-1 =Z_t + 0.4 Z_t-1
# ARMA(2,0) : X_t - 0.7X_t-1+ 0.1X_t-2 =Z_t
# ARMA(2,1) : X_t - 0.75X_t-1 + 0.5625X_t-2=Z_t + 1.25 Z_t-1

library(itsmr)
arpara=c(0.3,0.2)
mapara=c(0.8)

# Theoretical ACF and PACF for ARMA Models 

ACF_Theory = ARMAacf(ar=arpara, ma = mapara,lag.max = 20, pacf = F)

PACF_Theory = ARMAacf(ar=arpara, ma = mapara,lag.max = 20, pacf = T)


#1. Specify the Model using this function
model=specify(ar = arpara, ma=mapara, sigma2 = 1)
check(model)
#2. generate ARMA observations with
datasim=sim(model,n = 1000)

par(mfrow=c(2,1))
plot((0:20),ACF_Theory,type="h", xlab = "Lag")
plot((1:20),PACF_Theory,type="h",xlab = "Lag")
a=acf(datasim)
#pacf(datasim)



