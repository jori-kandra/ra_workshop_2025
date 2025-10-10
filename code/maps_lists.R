library(tidyverse)
library(epiextractr)
library(epidatatools)
library(openxlsx2)

### DATA SOURCE ####
epi_march <- arrow::read_feather("epi_march.feather")
ipums_march <- arrow::read_feather("ipums_march.feather") 

### FUNCTIONS ####
# function to write worksheet to excel 
sheets_fun <- function(data, wb, s) {
  wb$add_worksheet(sheet = s)$
     add_data(x = data)$
     set_col_widths(cols = 2:ncol(data), widths = 15)$
     add_cell_style(dims = wb_dims(rows = 1, cols = 2:ncol(data)), 
                    wrap_text = TRUE, horizontal = "center", vertical = "center")
  
  invisible(wb)
}


# function to perform different methods for different groups 
mfun <- function(data, x, m = NULL) {
  
  # cross-tabulation for indicator variables
  if (m == "tab") {
    df <- data %>% 
      crosstab(year, !!rlang::parse_expr(x)) %>% 
      rename_with(.cols = -year, ~ paste0(x, "_", .x, recycle0 = TRUE))
  } 
  
  # count for id variables
  else if (m == "count") {
    df <- data %>% 
      filter(!is.na(!!rlang::parse_expr(x))) %>% 
      summarise(!!paste0(x, "_n") := n(), .by = year)
  } 
  
  # sum for income variables and weights
  else if (m == "sum") {
    df <- data %>% 
      summarise(!!paste0(x, "_sum") := sum(!!rlang::parse_expr(x), na.rm = TRUE), .by = year)
  } 
  
  # average for income variables
  else if (m == "mean") {
    df <- data %>% 
      summarise(!!paste0(x, "_mean") := mean(!!rlang::parse_expr(x), na.rm = TRUE), .by = year)
  }
  
  else {
    message("Invalid method: ", m)
    return(tibble())
  }
  
  return(df)
  
}

# testing function to handle list input
testing_fun <- function(x, wb) {
  
  # iterate across each list item
  imap(x, ~ {
    
    s <- .y  # full name of the list item/group, e.g., "ipums_tab"
    m <- str_sub(.y, start = 7)  # extract method (e.g., "tab")
    
    # dynamically assign data
    data_source <- if (str_detect(s, "ipums")) ipums_march else epi_march
    
    
    # iterate across vector of var names
    df <- map(.x, ~ mfun(data = data_source, x = .x, m = m)) %>%
      # df of variables across a list item
      reduce(full_join, by = "year")
    
    # map to workbook
    sheets_fun(wb = wb, s = s, data = df) 
    
    # return df
    return(df)
  })
  
}

### VAR LISTS ####
## Categories based on Microsoft Planner

# corresponds to vars tagged "green"
#note: mostly agnostic variables
test_var_list <- list(
  ipums_tab = c("vetstat", "paidhour", "marst", "rotate",
                "sex", "empstat", "citizen"),
  ipums_count = c("hrhhid", "hrhhid2", "famid"),
  epimd_tab = c("veteran", "paidhre", "married", "minsamp",
                "female", "emp", "citizen"),
  epimd_count = c("hrhhid", "hrhhid2", "famid")
)

testing_fun(x = test_var_list, wb)


list1 <- list(
  epimd_tab = c("veteran", "paidhre", "married", "minsamp",
                "female", "emp", "citizen"),
  epimd_count = c("hrhhid", "hrhhid2", "famid")
)

list2 <- list(
  epimd_tab = c("veteran", "paidhre", "married", "minsamp",
                "female", "emp", "citizen"),
  epimd_count = c("hrhhid", "hrhhid2", "famid")
)

# quietly iterate over the two parallel vectors
pwalk(
  .l = list(lst  = list(list1, list2),
            file = c("round1_wb.xlsx", "round2_wb.xlsx")),
  .f = function(lst, file) {
    # create wb
    wb <- createWorkbook()
    
    # fill wb with the variables and methods specified
    #note: data source, variables, and methods defined by list item
    #      (e.g., "ipums_tab")
    testing_fun(x = lst, wb = wb) 
    
    # save wb
    saveWorkbook(wb, file, overwrite = TRUE)
  }
)
