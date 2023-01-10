# libraries needed
library("ltsa")
library("itsmr")
library("astsa")


# Example Dow Jones Utilies Index 1972
par(mfrow=c(1,1))
#plot
plot.ts(dowj, type="o")
M=c("diff",1)
Y= Resid(dowj,M)
test(Y)
# Order selection, p=1:3, q=1:5
# Yule-Walker AR
ywfit=yw(x = Y,p = 1)
# Innovations MA
iafit=ia(Y,q=1)
# Hannan- Rissanen ARMA
hrfit=hannan(Y,p=1,q=2)
# MLE
mlefit=arma(Y,p=1,q=2)
# auto fit uses MLE
autofit(Y)

# X_t-X_{t-1} = Y_t = 0.133 + 0.447Y_{t-1} + Z_t AR(1) AICC=74.48 not IID

# X_t-X_{t-1} = Y_t = 0.133 + 0.9Y_{t-1} -0.54 {t-1}  -  0.132Z_{t-2}   ARMA(1,2) AICC=76.53 



mlefit
model=specify(ar = mlefit$phi , ma=mlefit$theta, sigma2 = mlefit$sigma2)
Rfit= Resid(x = dowj,M = M,a = model)
test(Rfit)





# EXAMPLE Lake data
plot.ts(lake)
M=c("trend",1)
Y=Resid(lake,M)
test(Y)

mlefit=arma(Y,p=2,q=0)
mlefit
autofit(Y)

model=specify(ar = c(mlefit$phi) , ma=c(mlefit$theta), sigma2 = mlefit$sigma2)
Rfit= Resid(x=lake,M=M,a = model)
test(Rfit)

check(model)

# A simulation study
#1. Specify the Model using this function
model=specify(ar = c(0.8), ma=c(0), sigma2 = 1)
check(model)
#2. generate ARMA observations with
set.seed(5)
datasim=sim(model,n = 200)
plot(datasim,type="o")
Dsim= datasim - mean(datasim)
test(Dsim)

mlefit=arma(Dsim,p=1,q=0)
Estmodel=specify(ar = c(mlefit$phi) , ma=c(mlefit$theta), sigma2 = mlefit$sigma2)
Rfit= Resid(x=Dsim,M=NULL,a = bestmodel)
check(model)
test(Rfit)

autofit(Dsim)

likefitsimAr=arma(Dsim,p=1,q=1)

# For AR processes
yw(Dsim,p = 1)
burg(Dsim,1)

# For MA processes
ia(Dsim,q=10)

# For ARMA processes
hannan(Dsim,p=1,q=1)



model=specify(ar = c(0.5838), ma=c(0), sigma2 = 1)
Rfit= Resid(x=Dsim,M=NULL,a = model)
test(scale(Rfit,center = F, scale = T))

model=specify(ar = c(0.4133), ma=c(0.2631), sigma2 = 0.98)
Rfit= Resid(x=Dsim,M=NULL,a = model)
test(scale(Rfit,center = F, scale = T))















#Specify ARMA MODEL
# model=specify(ar = c(-1,0.24), ma=c(0.4,0.2,0.1), sigma2 = 1)
# check(model)
# #2. generate ARMA observations with
# ma1=sim(model,n = 100)
# plota(ma1)
# plot.ts(ma1)
# forecast(x=ma1[1:50],M = NULL,a = model,h = 10)