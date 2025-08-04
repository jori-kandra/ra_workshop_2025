library(tidyverse)
library(epidatatools)
library(openxlsx2)

epi_march <- arrow::read_feather("./data/epi_march.feather")
ipums_march <- arrow::read_feather("./data/ipums_march.feather")

# function to write worksheet to excel 
sheets_fun <- function(data, wb, s) {
  wb$add_worksheet(sheet = s)$
     add_data(x = data)$
     set_col_widths(cols = 2:ncol(data), widths = 15)$
     add_cell_style(dims = wb_dims(rows = 1, cols = 2:ncol(data)), 
                    wrap_text = TRUE, horizontal = "center", vertical = "center")
  
  invisible(wb)
}

crosstab(epi_march, year, veteran) |> sheets_fun(wb = wb, s = "epi_veteran")
crosstab(ipums_march, year, vetstat) |> sheets_fun(wb = wb, s = "ipums_vetstat")

crosstab_fun <- function(data, demo) {
  crosstab(data, year, {{ demo }})
}

count_fun <- function(data, demo) {
  data |> filter(!is.na({{ x }})) |> summarise(n = n())
}
crosstab_fun(epi_march, demo = veteran)
crosstab_fun(ipums_march, demo = veteran)

map(c("veteran", "female", "citizen"), ~ crosstab_fun(data = epi_march, demo = !!sym(.x))) |> 
  reduce(full_join)

m_fun <- function(data, var, m = NULL) {
  if (m == "tab") {
    df <- crosstab(data, year, !!rlang::parse_expr(var)) |> 
          rename_with(.cols = -year, ~ paste0(var, "_", .x))
  }
  else if (m == "count") {
    df <- data |> 
      filter(!is.na(!!rlang::parse_expr(var)))|> 
      summarize(!!paste0(var, "_n") := n(), .by = year)
  }
  else {
    message("Invalid method:", m)
    return(tibble())
  }

  return(df)
}

m_fun(epi_march, var = "veteran", m = "tab")
m_fun(epi_march, var = "hrhhid", m = "count")


map(c("veteran", "female", "citizen"), ~ m_fun(epi_march, var = .x, m = "tab")) |> 
  reduce(full_join)

map(c("hrhhid", "hrhhid2", "famid"), ~ m_fun(epi_march, var = .x, m = "count")) |> 
  reduce(full_join)

testing_var_list <- list(
  ipums_tab = c("vetstat", "sex", "citizen"),
  epimd_tab = c("veteran", "female", "citizen")
  #ipums_count = c("hrhhid", "hrhhid2", "famid"),
  #epimd_count = c("hrhhid", "hrhhid2", "famid")
)

testing_fun <- function(x, wb) {
  
  # iterate across each list item
  imap(x, ~ {
    
    s <- .y  # full name of the list item/group, e.g., "ipums_tab"
    m <- str_sub(.y, start = 7)  # extract method (e.g., "tab")
    
    # dynamically assign data
    data_source <- if (str_detect(s, "ipums")) ipums_march else epi_march
    
    
    # iterate across vector of var names
    df <- map(.x, ~ m_fun(data = data_source, var = .x, m = m)) %>%
      # df of variables across a list item
      reduce(full_join, by = "year")
    
    # map to workbook
    sheets_fun(wb = wb, s = s, data = df) 
    
    # return df
    return(df)
  })
  
}

wb <- wb_workbook()

testing_fun(testing_var_list, wb)

list1 <- list(
    ipums_tab = c("vetstat", "sex", citizen),
    ipums_count = c(hrhhid, hrhhid),
    epi_tab = c(veteran, female, citizen),
    epi_count= c(hrhhid, hrhhid)
)

list2 <- list(
    ipums_tab = c(paidhour, empstat, marst, rotate),
    ipums_count = c(famid),
    epi_tab = c(paidhre, empstat, married, minsamp),
    epi_count= c(famid)
)

test_fun(list1, wb)
test_fun(list2, wb)


