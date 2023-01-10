
#Tidyverse
install.packages("tidyverse")
library(tidyverse)
# dyplr: manipulating data.frame
# purrr: working with functions
# ggplot2: visualization
# tidy data: each row represents one observation and columns represent variables.
intall.packages("dslabs")
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

murders %>% select(state, region, rate) %>% filter(rate < 0.5)

murders %>% select(abb, region, rate) %>% filter(rate <0.7)


25 %>% sqrt() %>% log2()
log2(sqrt(25))


## summarizing data
data(heights)
# computes the average and standard deviation for females:
avg_sd = heights %>% filter(sex=="Female") %>%
  summarise(avg=mean(height),sdt=sd(height))

avg_sd


heights %>% 
  filter(sex == "Female") %>%
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
murders %>% arrange(rate)

murders %>% 
  arrange(population) %>%
  head()

murders %>% top_n(5, rate)


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
