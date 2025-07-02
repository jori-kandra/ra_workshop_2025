library(tidyverse)
library(realtalk)

# set base year value
cpi_base <- c_cpi_u_extended_annual$c_cpi_u_extended[c_cpi_u_extended_annual$year == 2024]

# perform inflation adjustmnt on waged workers
adj_data <- load_basic(2019:2024, year, age, wage) %>% 
  # labor force restrictions + remove missing wages
  #   filter(age >= 16, !is.na(wage)) %>%
  # use *_join() to merge CPI data to wages
  left_join(c_cpi_u_extended_annual, by = "year") %>%
  # inflation-adjusted wages
  mutate(year = year,
    real_wage = wage * (cpi_base/c_cpi_u_extended),
    .keep = "used")

## Using EPI CPS Basic Monthly Extracts, Version 2025.6.11
adj_data %>% head(5)

adj_data %>% 
  summarise(wage = MetricsWeighted::weighted_mean(wage, w = basicwgt/12), 
            real_wage = MetricsWeighted::weighted_mean(real_wage, w = basicwgt/12), 
            .by = year)

