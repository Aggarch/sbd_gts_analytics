# Ad hoc

library(tidyverse)
library(lubridate)

nominal <- function(nom){
  
  
  TRM = 3950
  
  result = nom * TRM
  
  return(result)
  
}

money <- "C:/Users/AEG1130/Documents/payrroll"
setwd(money)


# Statement ---------------------------------------------------------------

cT_statement <- openxlsx::read.xlsx("cT_statement.xlsx") %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(date_close = as.Date(closing_time_utc_5, origin = "1899-12-31")) %>% 
  mutate(year = year(date_close)) %>% 
  mutate(period = month(date_close)) %>% 
  mutate(day = day(date_close)) %>% 
  select(-closing_time_utc_5)
  


perf_symbol<- function(){ 
  
  symbol_perf<-cT_statement %>% 
    group_by(year, symbol) %>% 
    summarise(net_usd = sum(net_usd), .groups = "drop") %>% 
    pivot_wider(names_from = year, values_from = net_usd) %>% 
    replace(is.na(.), 0) %>% 
    mutate(overview = rowSums(across(where(is.numeric)))) %>% 
    arrange(overview)
  
  
  return(symbol_perf)
    
  }

  
  

# Transacts ----------------------------------------------------------------

cT_transact <- openxlsx::read.xlsx("cT_transact.xlsx") %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 

    mutate(year = year(date_close)) %>% 
  mutate(period = month(date_close)) %>% 
  mutate(day = day(date_close)) %>% 
  select(-closing_time_utc_5)
