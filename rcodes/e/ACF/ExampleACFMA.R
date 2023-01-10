# Examples of MA(1)
# Xt = theta Z{t-1} + Zt
# Gamma(0) = (1+theta^2)sigma^2 
# Gamma(h) = theta sigma^2  h=+/-1

# ACF(h) = theta/(1+theta^2) if h=+/-1
library(itsmr)

# Theoretical ACF for MA Models 
theta= 0.8
ACF_Theory = ARMAacf(ma =theta,lag.max = 20)


#1. Specify the Model using this function
modelMA1=specify(ma=theta, sigma2 = 1)
check(modelMA1)
#2. generate MA observations with
datasim=sim(modelMA1,n = 100)

# Plots
par(mfrow=c(2,1))
plot((0:20),ACF_Theory,type="h", xlab = "Lag")
a=acf(datasim)
#pacf(datasim)



