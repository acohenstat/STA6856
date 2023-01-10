# Dow Jones Utilities Index

plot(dowj,type="o")
acf(dowj,lag.max = 50)

M=c("diff",1)
mean(diff(dowj))
R=Resid(dowj,M)
test(R)

m1=arma(R,p=1)
m1
test(Resid(dowj,M,m1))

m2=arma(R,p=1,q=1)
m2
test(Resid(dowj,M,m2))
