#ACF Examples
pop<- read.table("http://users.stat.umn.edu/~kb/classes/5932/data/uspop.txt", header = FALSE)
plot.ts(pop$V1, type="o", ylab="Population USA")

acfpop = acf(pop$V1,lag.max = 20)


# International AirPass
A<- read.table("http://users.stat.umn.edu/~kb/classes/5932/data/airpass.txt", header = FALSE)
#Time=seq(from = 1,to=144, by = 1)
plot.ts(A$V1, type="o", ylab="Monthly totals of internaional airline passengers 1949-1961")
acfa = acf(A$V1,lag.max = 100)



