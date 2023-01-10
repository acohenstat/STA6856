#Packages for time series
library(itsmr)
library(astsa)

pop=read.table("http://users.stat.umn.edu/~kb/classes/5932/data/uspop.txt", header=F)

plot.ts(pop$V1)

Time=seq(from = 1790,to=1990, by = 10)
plot(Time,pop$V1/1000000, 
     type="o", 
     xaxt="n",
     xlim=c(1790, 1990), 
     ylab="Population of the USA (Millions)",
     xlab = "Time(Yearly)")
axis(side = 1,at = Time,labels = Time,las=2)

??gtemp
data("gtemp_land")
plot.ts(gtemp2, type="o", ylab="Surface Air Temperature Deviations")

plot.ts(jj, type="o", ylab="J and J Quarterly Earnings Per Share")

plot.ts(airpass,type="o", ylab="Monthly totals of internaional airline passengers 1949-1961")

B<- read.table("beer.txt")
TB=seq(from = 1,to=422, by = 1)
plot(TB,B$V1, type="o", ylab="Monthly beer production in Austrlia (Megaliters)")

