# Run Decomposition
# Xt = mt + St + Yt
library(fpp3)
??us_employment
data(us_employment)
us_retail_employment = us_employment %>%
  filter(year(Month) >= 2000, Title == "Retail Trade") %>%
  select(-Series_ID)
autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

# STL Decomposition

dcmp = us_retail_employment %>%
  model(dstl = STL(Employed))
components(dcmp)

# plot 
components(dcmp) %>% autoplot()
  
# plot trend
components(dcmp) %>% as_tsibble() %>%
  autoplot(Employed, color="blue") +
  geom_line(aes(y=trend), color = "red") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )
 # the seasonally adjusted series can be useful if we are looking for
 # variation that are not due to seasonality
components(dcmp) %>% as_tsibble() %>%
    autoplot(Employed, color="blue") +
    geom_line(aes(y=season_adjust), color = "red") +
    labs(
      y = "Persons (thousands)",
      title = "Total employment in US retail"
    )
  
# Classical decomposition
us_retail_employment %>%
  model(   dclss= classical_decomposition(Employed, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")


