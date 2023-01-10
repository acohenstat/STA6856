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

olympic_running %>% distinct(Length)

# Working on tsibble objects
data(PBS)
# Let's look at the Cost time series
costa10 = PBS %>% filter(ATC2=="A10")%>% dplyr::select(Cost) %>%
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
