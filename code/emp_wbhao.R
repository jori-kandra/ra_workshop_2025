library(tidyverse)
library(epiextractr)
library(epidatatools)


basic_data <- load_basic(2023, year, emp, wbhao, basicwgt, age, female,
                         discwork, unemp)

universe <- basic_data |> 
  filter(age >= 16) |> 
  mutate(across(c(wbhao, female), ~ as_factor(.x)),
         wgt = basicwgt/12)

class(universe |> group_by(wbhao) |> ungroup())

universe |> 
  summarise(n = sum(basicwgt/12), .by = wbhao) |> 
  mutate(share = n/sum(n)) |>
  write_csv("./output/shares_wbhao.csv")

class(universe$wbhao)

crosstab(universe, year, wbhao, percent = "row", w = basicwgt/12)
crosstab(universe, year, female, percent = "row", w = basicwgt/12)


emp_shares_fun <- function(x) {
  universe |> 
    summarise(across(c(discwork, emp, unemp), 
                  list(shares = ~ sum(.x * wgt, na.rm = TRUE), 
                       sample = ~ sum(.x, na.rm = TRUE))),
                .by = {{ x }}) |> 
    mutate(across(ends_with("shares"), ~ .x/sum(.x))) |> 
  pivot_longer(cols = -c({{ x }}), names_to = "measure", values_to = "values") |>
  separate_wider_delim(measure, delim = "_", names = c("measure", "names")) |> 
  pivot_wider(id_cols = c({{ x }}, measure), names_from = names, values_from = values)
}


emp_shares_fun(wbhao)

universe[universe$female == 1, ]
filter(universe, female == 1)

emp_shares_fun("wbhao") 
emp_shares_fun("female")

df <- map(.x = c(wbhao, female), .f = ~ emp_shares_fun(.x)) |> 
  reduce(bind_rows) |> 
  unite(col = "demographic", c(wbhao, female), na.rm = TRUE)

