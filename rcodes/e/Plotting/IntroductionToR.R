# Library / package
library(datasets)
library(itsmr)

# Calculator 
x=10^2
y=2*x
s= "Hello Everyone"
b = F
45 > 8
5-9
4/8
sin(5)


# Vectors
vec1 = c(1,21,50,80,45,0)
sum(vec1)
mean(vec1)
sd(vec1)
summary(vec1)
var(vec1)

# Matrices
ma1=matrix(data=vec1,nrow = 4)
ma1
# Data frames
df=data.frame(x=rnorm(100),y=rnorm(90,mean = 5))
summary(tblairquality)
x=t$x
y=t$y

# R Datasets
mdata=cars

# Reading variables from a data.frame
with(mdata,mean(speed+dist))
mean(mdata$speed+mdata$dist)

# Structure and Summary
str(mdata)
summary(mdata)
print(mdata)
View(mdata)

# Creating new variables 
mdata$time=mdata$dist/mdata$speed
mdata$speed2=mdata$speed^2

# Rename the variables 
library(reshape)
mdata=reshape::rename(mdata,c("speed"="mspeed", "speed2"="s2"))

# Create categories from continuous variable
mdata$newdata= (mdata$mspeed >=5) + (mdata$mspeed >=10) + (mdata$mspeed >=15)  +(mdata$mspeed >=20) 
mdata$ndata= cut(mdata$mspeed, breaks = c(4,5,10), labels = c("cut0","cut1"), right = FALSE)

# Creating factors
x=c(0,0,1,1,2)
x=factor(x,labels = c("Control","Low Dose","High Dose"))

# Drop or keep variables in a dataset
library(datasets)
library(carData)
mdata=Salaries

submdata=subset(mdata,select = c("discipline","salary"))
submdata1=mdata[,-c(1:4)]
submdata2=subset(mdata,select = c(-2,-3))

# Keep Unique values
uni=unique(mdata)

# Identify duplicated values
dup=duplicated(mdata)

# stack datasets
newdata=cbind(rnorm(50),rnorm(50))
rb=rbind(rnorm(100),rnorm(100))


# Merge datasets
ds1=cars
ds1$s2=cars$speed**2
ds1$dist=NULL
ds=merge(ds1,cars,by = "speed",all=TRUE)


# Prabability distributions
mdata=rnorm(100,mean = 0,sd=1)
hist(mdata,freq = TRUE)
plot(mdata,
     type="o",
     col="green",
     xlim = c(-1,100),
     ylim = c(-4,4),
     xlab = "Random Numbers from Normal Distribution"
     ,ylab = "Values", 
     main = "100 Obs. from Standard Normal Dist.")

dt=rnorm(10000,mean=10,sd=3)
var(dt)
quantile(dt,c(0.25,0.5,0.75,0.99))


library(moments)
skewness(dt)
kurtosis(dt)


hist(dt)
hist(scale(dt))

# Probability distributions
x=rnorm(100,mean=1,sd = 1)
xp=pnorm(1.96)
y=pbinom(2,size = 10,prob = 0.5,lower.tail = FALSE)
q=qnorm(0.975)



set.seed(1)
x=runif(10,min = 0,max = 1)
x2=rnorm(10)

x3=rnorm(10)
rexp(10,rate = 1)


# functions
min(rnorm(100))
max(rnorm(100))
abs(rnorm(100))
sqrt(abs(rnorm(100)))
2**9
2^9
exp(1)
log(1)
log10(10)
log2(2)
log(45,base = 45)

factorial(3)
choose(10,5)


#
#Tidyverse
install.packages("tidyverse")
library(tidyverse)
# dyplr: manipulating data.frame
# purrr: working with functions
# ggplot2: visualization
# tidy data: each row represents one observation and columns represent variables.
install.packages("dslabs")
library(dslabs)
data("murders")

#tidy data
head(murders)
data(co2)
data("ChickWeight")


# Working with Data.frame
# add a variable/column 

murders = mutate(murders, rate=total/population*100000)


# subsetting with filter

filter(murders, rate < 0.7)

new_table = select(murders, state, region, rate)
filter(new_table, rate <0.7)


# The pipe %>%
# With dplyr we can do a series of operations, for example select and then filter 
# using the pipe operator

murders %>% select(state,region,rate) %>% filter(rate<0.7)


murders %>% select(abb, region, rate) %>% filter(rate <0.7)


25 %>% sqrt() %>% log2()
log2(sqrt(25))


## summarizing data
data(heights)
summary(heights)
# computes the average and standard deviation for females:
heights %>% filter(sex=="Female") %>% summarise(avg=mean(height),sdeviation=sd(height))




avg_male = heights %>% filter(sex=="Male") %>%
        summarise(avg=mean(height),sdt=sd(height))

avg_male


heights %>% filter(sex == "Female") %>%
        summarize(range = quantile(height, c(0, 0.5, 1)),avg=mean(height),sdt=sd(height))


# Let's compute the average murder rate of the USA
# Recall that the USA murder rate is not the average of the state murder rates

summarise(murders, mean(rate)) 

US.murder.rate = murders %>% summarise(rate=sum(total)/sum(population)*100000)

# Looking at the results they are data.frame even if it is just one number
murders %>% summarise(rate=sum(total)/sum(population)*100000) %>%
        pull(rate)


## Data Grouping
heights %>% group_by(sex)
# tibble: many tables same columns but not necessarily the same number of rows

heights %>% group_by(sex) %>% summarise(avg= mean(height),std=sd(height))

# let's compute the median murder rate in the four regions of the country:
murders %>% group_by(region) %>% summarise(median.rate=median(rate))  


## Arrange data frame 
murders %>% arrange(desc(rate))

murders %>% 
        arrange(population) %>%
        head()

murders %>% top_n(5, rate)


## purrr : apply functions
# apply the same function to each element of a vector 
compute_s_n <- function(n){ sum(1:n) }
sn = map(c(1,5,10,15,20), compute_s_n)


## purrr : apply functions
# apply the same function to each element of a vector 
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
sn = map(c(1,5,10,15,20), compute_s_n)



## ggplot2
# data visualization
library(ggthemes)
library(ggrepel)


murders %>% ggplot(aes(x=population/10^6,y=total)) +
  geom_point(size=4)+
  geom_text_repel(aes(label=abb))+
  scale_x_log10()+
  scale_y_log10()+
  geom_smooth(method=lm,se=FALSE)+
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") + 
  scale_color_discrete(name = "Region") +
  facet_wrap(~region)
theme_dark()



