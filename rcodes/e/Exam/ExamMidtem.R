library(astsa)
library(itsmr)
??chicken
chicken= as.numeric(chicken)
#1. Plots and Examine the series: trend upward, postive dependence, no seasonality
plot(chicken,type="o")

#2. Plots and Examine the ACF: slow decay positive ACF, trend,
acf(chicken,lag.max = 100)

#3. data model : X_t = m_t + z_t

#4. regression 
M=c("trend",2)
plot(chicken,type = "o")
lines(trend(chicken,1),col="red",lwd=2)
lines(trend(chicken,2),col="blue",lwd=2)
lines(trend(chicken,3),col="yellow",lwd=2)
mean(chicken-trend(chicken,2))
R1 = Resid(chicken,M)
test(R1)

#5. differencing
M2=c("diff",1) # Xt- Xt-1
mean(diff(chicken)) # 0.256
R2=Resid(chicken,M2)
acf(R2,lag.max = 100)
test(R2)

#6. Well, we see a seasonality of d=12 after differencing the series.
# X_t  =  m_t + s_t + z_t
M=c("trend",2,"season",12)
R1 = Resid(chicken,M)
test(R1)


R2 = (diff(diff(chicken,lage=12,differences = 1),1,differences = 2))
test(R2)

Bestmodel = autofit(scale(R2,center = T,scale = F))
