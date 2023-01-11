## Brief Intro to Tidyverse  

#Tidyverse
#install.packages("tidyverse")
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

murders %>% 
  select(state,region,rate) %>% 
  filter(rate<0.7)

murders %>% 
  select(state, region, rate) %>% 
  filter(rate < 0.5)

murders %>% 
  select(abb, region, rate) %>% 
  filter(rate <0.7)


25 %>% 
  sqrt() %>% 
  log2()
log2(sqrt(25))


## summarizing data
data(heights)
# computes the average and standard deviation for females:
avg_sd = heights %>% 
  filter(sex=="Female") %>%
  summarise(avg=mean(height),sdt=sd(height))

avg_sd


heights %>% 
  filter(sex == "Female") %>%
  summarize(range = quantile(height, c(0, 0.5, 1)),avg=mean(height),sdt=sd(height))


# Let's compute the average murder rate of the USA
# Recall that the USA murder rate is not the average of the state murder rates

summarise(murders, mean(rate)) 

US.murder.rate = murders %>% 
  summarise(rate=sum(total)/sum(population)*100000)

# Looking at the results they are data.frame even if it is just one number
murders %>% 
  summarise(rate=sum(total)/sum(population)*100000) %>%
  pull(rate)


## Data Grouping
 heights %>% 
   group_by(sex)
# tibble: many tables same columns but not necessarily the same number of rows

heights %>% 
  group_by(sex) %>% 
  summarise(avg= mean(height),std=sd(height))

# let's compute the median murder rate in the four regions of the country:
murders %>% 
  group_by(region) %>% 
  summarise(median.rate=median(rate))  


## Arrange data frame 
murders %>% 
  arrange(rate)

murders %>% 
  arrange(population) %>%
  head()

murders %>% 
  top_n(5, rate)


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

  
  
### Time Series
  # Time series packages in R 
  # tsibble objects
  
  #A time series can be thought of as a list of numbers (the measurements), 
  # along with some information about what times those numbers were recorded (the index).
  #This information can be stored as a tsibble object in R.
  library(tidyverse)
  library(tsibble)
  y = tsibble(
    Year = 2010:2019,
    Observation = c(13, 85, 2502, 200, 50,45,48,49,69,6),
    index = Year
  )
  
  # tsibble extend tibble (tidy data frames) to time series.
  #We have set the time series index to be the Year column
  # Suppose we have more frequent data, say monthly data
  
  z = data.frame( Month = c("2021 Jan","2021 Feb"," 2021 Mar"," 2021 Apr","2021 May"),
                  Observation = c(52, 39, 78, 47, 747))
  
  
  z1 = z %>%   mutate(Month = yearmonth(Month)) %>%
    as_tsibble(index = Month)
  
  # Key variables
  
  # tsibble allows multiple time series to be stored.
  #Let's look at the Olympics data set  containing the fastest running times for 
  # women's and men's track races from 100m to 10000m:
  library(tsibbledata)
  olympic_running
  # we have a tsibble object
  # [4y] indicates that the interval of these obs. is every 4 years.
  # Key line: indicates 14 separate time series
  # The 14 time series are identified by the KEYS: Length and Sex.
  # The distinct() function can be used to show them
  
  olympic_running %>% 
    distinct(Length)
  
  # Working on tsibble objects
  data(PBS)
  # Let's look at the Cost time series
  costa10 = PBS %>% 
    filter(ATC2=="A10")%>% 
    dplyr::select(Cost) %>%
    summarise(TotalC = sum(Cost)) %>%
    dplyr::mutate(CostM=TotalC/1e6)
  
  # Plotting
  library(feasts)
  autoplot(costa10,CostM)  + 
    labs(title = "Monthly Total Cost A10 Prescription",
         y="Total Cost in millions")
  
  # season plot
  costa10 %>% gg_season(CostM,labels = "both") 
  # multiple seasonal periods
  #Data	    	Day	Week	Year
  #Quarters			        4
  #Months		            12
  #Weeks					      52
  #Days				     7	  365
  #Hours			24	168	  8766
  
  # data 1/2 hour electricity demand for Victoria
  vic_elec %>% gg_season(Demand, period = "day") +
    labs(y="MW", title="Electricity demand: Victoria")
  vic_elec 
  # subseries
  costa10 %>% gg_subseries(CostM)
  
  
  ??tourism 
  h=tourism %>% filter(Purpose=="Holiday") %>% 
    group_by(State) %>% summarise(Ttrips = sum(Trips))
  h
  
  autoplot(h,Ttrips)
  #To see the timing of the seasonal peaks
  gg_season(h, Ttrips) +
    labs(y = "Overnight trips",
         title = "Australian holidays")
  
  gg_subseries(h,Ttrips)
  
  
  h %>%
    pivot_wider(values_from=Ttrips, names_from=State) %>%
    GGally::ggpairs(columns = 2:9)
  
  # ACF
  aus_production %>% ACF(Beer) %>% autoplot()
  costa10 %>%
    ACF(CostM, lag_max = 50) %>%
    autoplot() 
  
  
  
  