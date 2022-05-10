

library(tidyverse)

setwd("C:/Users/AEG1130/Documents/payrroll")

cT_statement <- openxlsx::read.xlsx("cT_statement.xlsx") %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(net_usd = as.numeric(net_usd)) %>% 
  mutate(date_close = as.Date(closing_time_utc_5, origin = "1899-12-31")) %>% 
  mutate(year = year(date_close)) %>% 
  mutate(period = month(date_close))%>%
  mutate(day = day(date_close))

ranking <- cT_statement %>% 
  group_by(symbol) %>% 
  summarise(net_usd = sum(net_usd), .groups ="drop") %>% 
  arrange(net_usd)


perfm_dod <- cT_statement %>% 
  group_by(year, period, day) %>%
  summarise(net_usd = sum(net_usd),.groups = "drop")


perfm_mom <- cT_statement %>% 
  group_by(year, period) %>%
  summarise(net_usd = sum(net_usd),.groups = "drop")

perfm_yoy <- cT_statement %>% 
  group_by(year) %>%
  summarise(net_usd = sum(net_usd),.groups = "drop")


# Funciton Perf by Symbol -------------------------------------------------


perf_symbol <- function(inputed){ 

symbol_perf<- cT_statement %>% 
  filter(symbol == inputed) %>% 
  group_by(opening_direction, year) %>% 
  summarise(net_usd = sum(net_usd)) %>%
  pivot_wider(names_from = year, values_from = net_usd) 

  return(symbol_perf)  
}

cT_statement %>% distinct(symbol)

# Transact ----------------------------------------------------------------

cT_transact <- openxlsx::read.xlsx("cT_transact.xlsx") %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  filter(status %in% c("Completed", "Approved")) %>% 
  mutate(amount = ifelse(transaction_type == "Deposit", amount*-1,amount))


cT_transact %>% 
  group_by(transaction_type,trading_account) %>%
  summarise(amount = sum(amount), n=n(),.groups="drop") %>% 
    janitor::adorn_totals()
