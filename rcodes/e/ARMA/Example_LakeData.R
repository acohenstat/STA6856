library(itsmr)

plot(lake,type="o")
acf(lake)
pacf(lake)

D=diff(lake)
plot(D,type="o")
t=1:98
summary(lm(lake~t))

M=c("trend",1)
D=Resid(lake,M)
test(D)

# fit ARMA model to Lake data
Y=lake-mean(lake)
model1=arma(Y,p = 2,q=0)
model1
test(Resid(Y,a = model1))

model2=arma(Y,p = 1,q=0)
model2
test(Resid(Y,a = model2))

model3=arma(Y,p = 1,q=1)
model3
test(Resid(Y,a = model3))


model4=arma(Y,p = 2,q=1)
model4
test(Resid(Y,a = model4))


