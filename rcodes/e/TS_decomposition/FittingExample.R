# libraries needed
library("ltsa")
library("itsmr")
library("astsa")

# deaths data
plot.ts(deaths)
acf(deaths,lag.max = 30)
M=c("season",12)
res1=Resid(deaths,M)
test(res1)

ss=season(deaths,12)
plotc(deaths,ss)    

plot.ts(wine)
acf(wine)
M=c("season",12,"trend",1)
res1=Resid(wine,M)
test(res1)




# Model Lake Huron 
B<- read.table("http://users.stat.umn.edu/~kb/classes/5932/data/lake.txt", header = F)
plot.ts(B$V1, type='o', ylab="Level of lake Huron 1875-1972 (feet)")
acf(B$V1)
lines(predict(model1),type = "l",col='red')
t1 <- seq(from=1,to = 98,by=1)

M1=c("trend",1)
test(Resid(B$V1,M1))

model1 <- lm(B$V1 ~ t1)
summary(model1) 
plot(model1$residuals,type="o")

acfres = acf(model1$residuals,lag.max = 40)


plot(B$V1[1:97],B$V1[2:98])
model2 <- lm(B$V1[2:98] ~ B$V1[1:97])
summary(model2) 

acfres2 = acf(model2$residuals,lag.max = 40)




