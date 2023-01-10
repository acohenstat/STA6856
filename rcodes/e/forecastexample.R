# libraries needed
library("ltsa")
library("itsmr")
library("astsa")


# Forecast Dow Jones Utilies Index 1972
plot.ts(X2017$DA_Demand[1:150], type="o")
plota(X2017$DA_Demand[1:150])
M=c("season",24,"trend",1)
Y= Resid(X2017$DA_Demand[1:150],M)
test(Y)
likefit=arma(Y,p=2,q=0)
model=specify(ar = c(likefit$phi) , ma=c(likefit$theta), sigma2 = likefit$sigma2)
Rfit= Resid(x=X2017$DA_Demand[1:180],M=M,a = model)
test(scale(Rfit,center = F, scale = T))
f=forecast(X2017$DA_Demand[1:180],M,model,h = 20)

# Lake data
plot.ts(lake)
M=c("trend",1)
Y=Resid(lake,M)
test(Y)

likefit=arma(Y,p=2,q=0)

model=specify(ar = c(likefit$phi) , ma=c(likefit$theta), sigma2 = likefit$sigma2)
Rfit= Resid(x=lake,M=M,a = model)
test(Rfit)
f=itsmr::forecast(lake,M,model,h = 5)


#Wine data
plot.ts(wine)
plota(wine)
M = c("season",12,"trend",1)
e = Resid(wine,M)
test(e)
a = arma(e,1,1)
model=specify(ar = c(a$phi) , ma=c(a$theta), sigma2 = a$sigma2)

Rfit= Resid(x=wine,M=M,a = model)
test(Rfit)
forecast(wine,M,a,h = 12)


# sunspot data
sun<- read.table("http://users.stat.umn.edu/~kb/classes/5932/data/sunspots.txt", header = FALSE)
plot.ts(sun$V1)
plota(sun$V1)
ss=scale(sun$V1,center = T,scale = F)
ywf=yw(x = ss,p = 2)


