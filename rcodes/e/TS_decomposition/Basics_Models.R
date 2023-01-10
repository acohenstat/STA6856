
# Simple Models
# White Noise 
w = rnorm(500,0,1)
#plot.ts(w, main="white noise")
plot(w, type="o",
     col="blue",
     main = "White Noise",
     xlab = "Time",
     ylab = "Values"
     )

library(itsmr)
plota(w)

# Random Walk 
 #set.seed(12)
 w = rnorm(200,0,1)
 s = cumsum(w)
 plot(s, main="Random walk",type="o")
 
 plota(s)


# Models with Trends 
 # regression quadratic model: Populations Data
 pop<- read.table("http://users.stat.umn.edu/~kb/classes/5932/data/uspop.txt", header = FALSE)
 plot.ts(pop$V1, type="o", ylab="Population USA")
 t1 = (1:21)
 t2 = t1^2
 
 model1 = lm(pop$V1 ~ t1 + t2)
 summary(model1) 
 acf(model1$residuals)
 
plot(pop$V1, type="o", ylab="Population USA", ylim=range(c(pop$V1,predict(model1))))
lines(predict(model1),type = "l",col='red')
 

library(itsmr)
m=trend(pop$V1,p = 2) # y = a + bt + ct^2
m1=trend(pop$V1,p = 1) # y = a + bt
m3=trend(pop$V1,p = 3) 
plot(pop$V1,type="o",lwd=2)
lines(m,col="red",lwd=2)
lines(m1,col="green",lwd=2)
lines(m3,col="blue",lwd=2)


# Models with Seasonality
plot(deaths,type="o",ylab="Accidental Deaths 1973-1978")
acf(deaths,lag.max = 30)
ss=season(deaths,d = 12)
plotc(deaths,ss)
 
 
 
 
 
 