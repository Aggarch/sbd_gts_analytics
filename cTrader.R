# Ad hoc

library(tidyverse)

nominal <- function(nom){
  
  
  TRM = 3950
  
  result = nom * TRM
  
  return(result)
  
}

money <- "C:/Users/AEG1130/Documents/payrroll"


setwd(money)

marketd <- openxlsx::read.xlsx("history.xlsx") %>% 
  as_tibble() %>% 
  janitor::clean_names()

marketd %>%
  group_by(opening_direction, symbol) %>%
  filter(opening_direction == "Buy")  %>%
  summarise(net = sum(net_usd),
            .groups = "drop") %>%
  arrange(desc(net)) 


# ANALIZE -----------------------------------------------------------------

marketd %>%
  group_by(opening_direction, symbol) %>%
  summarise(net = sum(net_usd),
            .groups = "drop") %>%
  pivot_wider(names_from = opening_direction, values_from = net) %>% 
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>% 
  mutate(net = Buy + Sell) %>% 
  arrange(desc(net))
  
  

# Checking ----------------------------------------------------------------

check <- openxlsx::read.xlsx("checking.xlsx") %>% as_tibble() %>% 
  mutate(FECHA = as.Date(FECHA, origin = "1899-12-30")) %>% 
  janitor::clean_names() %>% 
  mutate(period = lubridate::month(fecha)) %>% 
  mutate(year = lubridate::year(fecha))


check %>% group_by(period, year, descripcion) %>% 
  summarise(valor = sum(valor),.groups = "drop") %>% 
  arrange(desc(valor))
  
