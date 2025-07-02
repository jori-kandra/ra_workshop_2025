library(tidyverse)
library(realtalk)
library(openxlsx2)
library(epiextractr)

# set base year value
cpi_base <- c_cpi_u_extended_annual$c_cpi_u_extended[c_cpi_u_extended_annual$year == 2024]

# perform inflation adjustmnt on waged workers
adj_data <- load_org(2019:2024, year, age, wage, orgwgt) %>% 
  # labor force restrictions + remove missing wages
  filter(age >= 16, !is.na(wage)) %>%
  # use *_join() to merge CPI data to wages
  left_join(c_cpi_u_extended_annual, by = "year") %>%
  # inflation-adjusted wages
  mutate(year = year,
    real_wage = wage * (cpi_base/c_cpi_u_extended),
    wgt = orgwgt/12,
    .keep = "used")

## Using EPI CPS Basic Monthly Extracts, Version 2025.6.11
adj_data %>% head(5)

real_wage <- adj_data %>% 
  summarise(wage = MetricsWeighted::weighted_mean(wage, w = wgt), 
            real_wage = MetricsWeighted::weighted_mean(real_wage, w = wgt), 
            .by = year)

wb <- wb_workbook()

wb$add_worksheet(sheet = "shereal_wage")$
  add_data(x = real_wage)

wb_save(wb, "./output/real_annual_wages.xlsx")
