library(itsmr)
library(car)

plot(airpass,type="o",main="Original")
acf(airpass,lag.max =50)

## Season of d=12
## trend 1,2
## variance change

## Transformation
L=powerTransform(airpass)
L$roundlam
par(mfrow=c(3,1))
plot(log(airpass),type="o",main="Log Transformed")
Tairpass=(airpass^0.15 - 1)/0.15
plot(Tairpass,type="o",main="Transformed Lambda=0.15")


## Estimation
data.model = c("log","season",12,"trend",1)
r = Resid(airpass,data.model)
test(r)

## Differencing operator
## trend quadratic differences=2 -- nabla^2X_t
## trend straight line differences=1

rdf = diff(diff(log(airpass),lag = 1,differences = 1),lag = 12,differences = 1)
# nabla nabla_12 log(airpass) = residuals
test(rdf)


## ARMA(p,q)
test(r)

# automatic fit
autofit(rdf) # ARMA(p=5,q=4)

# Selecting p and q
## From PACF: p = 1, 3, 9
## From ACF: q = 1, 3, 9
c(arma(rdf,p =1,q=1)$aicc,
arma(rdf,p =1,q=3)$aicc,
arma(rdf,p =1,q=9)$aicc,
arma(rdf,p =3,q=1)$aicc,
arma(rdf,p =3,q=3)$aicc,
arma(rdf,p =3,q=9)$aicc,
arma(rdf,p =9,q=1)$aicc,
arma(rdf,p =9,q=3)$aicc)


bestmodel = arma(rdf,p =9,q=3)

model = specify(ar=bestmodel$phi , ma=bestmodel$theta , sigma2 = bestmodel$sigma2)
rfit = Resid(x = airpass,M = data.model,a = model)
test(rfit)
