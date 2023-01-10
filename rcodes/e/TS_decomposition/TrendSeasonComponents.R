# This R script shows different methods for Trend or Season Estimation
library(itsmr)
s = read.table("http://users.stat.umn.edu/~kb/classes/5932/data/strikes.txt", header = FALSE)

# plot data
plot((1951:1980),s$V1, type="o", ylab="Strikes USA data",xlab="Years")
#Autocorrelation function
acf = acf(s)

# Smoothing Moving Average
ma.trend=smooth.ma(s$V1, 3) # q=3 means averaging 2q+1 values
plot(ma.trend,type="o")
plotc(s$V1,ma.trend)

# Exponential Smoothing
exp.trend=smooth.exp(s$V1, 0.15) # 0.1 is the smoothness parameter
plotc(s$V1,exp.trend)


# Smoothing using FFT - Fourier Series
# 0.5 is cut-off frequency - 0.5 means it passes
# the lowest 50%  of the spectrum.
fft.trend=smooth.fft(s$V1, 0.15) 
plotc(s$V1,fft.trend)

# Polynomial Regression
t=(1:30)
t2=t**2
t3=t**3
m=lm(s$V1~ t + t2 + t3 )
plotc(s$V1,m$fitted.values)

# or you can use trend function
t=trend(s$V1,3) # 3 means polynomial of degree 3
plotc(s$V1,t)

# plot everything
plot(s$V1, type="o", ylab="Strikes USA data")
lines(ma.trend,col='red')
lines(exp.trend,col='green')
lines(fft.trend,col='blue')
lines(t,col='cyan')
legend("topleft",
       legend = c("Data","MA(3)","EXP(0.15)","FFT(0.15)","Poly Regression (3)"),
       col = c("black",'red','green','blue','cyan'),
       lty = rep(1,5))

test(s$V1-t)
shapiro.test(s$V1-t)
# Xt = mt + Yt

# Estimation Seasonality
# Deaths data
library(itsmr)
plot(deaths,type="o")
acf(deaths,lag.max = 40)
d.season=season(deaths,d = 12)
plotc(deaths,d.season)

test(scale(deaths-d.season,center = T))
mean(scale(deaths-d.season,center = T))

# Using Resid function to get the residuals
library(itsmr)
# Xt = St + mt + Yt
# Data Model
M=c("trend",1,"season",12)
# Residuals
R=Resid(deaths,M)
#Test IDD
test(R)

# The differencing operator
# wine data
plot(wine,type="o")
acf(wine,lag.max = 40)
#Eliminate Season
ds=diff(wine,lag = 12,differences = 1)
plot(ds)
# Eliminate Trend
dt=diff(ds,lag = 1,differences = 1)
#plot
plot(dt)
# Test IDD
test(dt)

# OR Data Model
plot.ts(log(wine))
M=c("log","season",12,"trend",1)
R=Resid(wine,M)
test(R)

# Estimation and elimination by subtraction
RR=log(wine)-season(log(wine),12)-trend(log(wine),1)
plot(RR)
test(RR)




# Spencer Filter
#library(signal)
#Strend=spencer(c(rev(s$V1[1:7]),s$V1,rev(s$V1[24:30])))
#sp=c(-0.009375, -0.018750, -0.015625,  0.009375,  0.065625,  0.143750,0.209375,  0.231250,  0.209375 , 0.143750,  0.065625,  0.009375,-0.015625, -0.018750, -0.009375)
#v = stats::filter(s$V1,sp,sides=2)
#plotc(s$V1,Strend[8:37])#Strendpop[!is.na(Strendpop)])




