
# <> Organics <>  --------------------------------------------------------


# From Basic Organics structure, {COLLECTION/IMPORTATION}
# Iterative functionality to offer all Ad hocs organics across Regions, SBU, Products. 
# Architecture for BA&R recursive refreshal MetaQuery since all the results

# Metaprocess #1


# 8 Blocks Reference. 

library(tidyverse)
library(lubridate)

organics_data         <- "C:/Users/AEG1130/Documents/organics/"

setwd(organics_data)



# Structured P&L Global ---------------------------------------------------

# Build all the queries needed to extract the last 5 years P&L Data. 
query_meta <- function(observ_year, observ_month){ 
  
  file <- openxlsx::read.xlsx("tidy_organics_query.xlsx") %>% as_tibble()
  
  chasis<- file %>% 
    janitor::clean_names() %>% 
    mutate(region = ifelse(is.na(region),"NA",region)) %>% 
    mutate(observation = observ_year,
           period = observ_month) %>% 
    mutate(year_num = substr(observation, 3,4)) %>% 
    mutate(years = paste0("FY",year_num)) %>% 
    select(-year_num) %>% 
    mutate(index = row_number()+1) 
    
  
  return(chasis)
  
}


# Times & Iterations 
timing <- function(period){ 
  
  times <- tibble(observ_month  = month.name) %>% 
    mutate(observ_year = period)
  
  return(times)
  
}

years   <- tibble(period = 2016  : year(today()))

periods <-  map(years$period, 
                timing) %>% 
  map_dfr(., bind_rows) 





# BA&R Data Pulling  ------------------------------------------------------


organics_hist <- function(){ 
  
  organics_structure <- map2(periods$observ_year, 
                             periods$observ_month,
                             query_meta) %>% 
    
    map_dfr(., bind_rows) %>% 
    arrange(desc(observation)) %>% 
    relocate(.before = observation, index) %>% 
    
    mutate(month = match(period, month.name)) %>%  
    mutate(ref_date = make_date(year = observation, month = month, day = 1L)) %>% 
    mutate(quarter = quarter(ref_date)) %>% 
    mutate(quarter = paste0("Q",quarter)) 
    
    
    
    organics_history <- organics_structure %>% 
      filter(ref_date <= today() %>% rollback(roll_to_first = T)) %>% 
      mutate(index = row_number()+1) %>% 
      mutate(index = as.character(index)) %>% 
      mutate(result = '=HsGetValue("PRD_OAC_RPTSBD01","Entity#"&D2,"Scenario#"&E2,"Years#"&M2,"Period#"&N2,"Account#"&F2,"Currency#"&G2,"Total Product#"&H2,"Total Customer#"&I2,"Function#"&J2,"DTS#"&O2,"Total Ship-to Geography#"&K2,"Total Brand#"&L2)') %>% 
    mutate(result = str_replace_all(result,'[[:digit:]]+',index)) %>% 
    mutate(result = str_replace_all(result,'/[[:digit:]]+',"/1000000")) %>% 
    mutate(result = str_replace_all(result,'PRD_OAC_RPTSBD[[:digit:]]+',"PRD_OAC_RPTSBD01")) %>%  
    relocate(.after = dts, result) %>% 
    select(-value) 
    
    
    organics_history %>% openxlsx::write.xlsx(.,"organics_history.xlsx", overwrite = T)

    
  return(organics_history)
  
}

# Script for writing the metadata, with the query, just historical data,
# and current month, future structures refers to same function where (t >= RToday)
# Note: time window filter must be always before formulation against indices. 
# the output of this function, named "organics_history.xlsx" located at organics_data path
# must be BA&R refreshed to be pulled and trigger the transformation cycle. 



# Transformation & Summarization ---------------------------------------------

# Recursive Table
organics <- openxlsx::read.xlsx("organics_history.xlsx") %>% as_tibble() %>% 
  mutate(ref_date = as.Date(ref_date, origin = "1899-12-30")) %>% 
  filter(ref_date <= rollback(today()%m-%months(1),roll_to_first = T)) %>% 
  mutate(region = ifelse(is.na(region),"NA",region)) %>% 
  select(-index, -ship_to, -currency, -customer,
         -'function', -product, -brand) %>% 
  mutate(geo =  case_when(region == "US" ~ "North America",
                          region == "NA" ~ "North America",
                          region == "Canada" ~ "North America",
                          region == "EMEA ANZ" ~ "Europe & ANZ",
                          region == "LAG" ~ "Latin America",
                          region == "ASIA" ~ "Asia")) %>% 
  relocate(.before = region, geo)


# Summary
org_summ <- organics %>%
  group_by(geo, region, channel,
           type,
           ref_date,observation,month,period,quarter) %>%
  summarise(result = sum(result),.groups = "drop") %>% 
  pivot_wider(names_from = type, values_from = result) %>% 
    rename(forecast_mtd = forecast_10_mtd,
           forecast_qtd = forecast_10_qtd) %>% 
  select(ref_date, observation, month, period, quarter,
         geo, region, channel,
         organic_mtd, sales_actual_mtd, forecast_mtd, OP_mtd, sales_PY_mtd,
         organic_qtd, sales_actual_qtd, forecast_qtd, OP_qtd, sales_PY_qtd
         ) %>% 
  
  mutate(mtd_sales_vfcast = sales_actual_mtd - forecast_mtd,
         mtd_sales_vop    = sales_actual_mtd - OP_mtd,
         mtd_sales_vpy    = sales_actual_mtd - sales_PY_mtd) %>% 
  relocate(.after = forecast_mtd,mtd_sales_vfcast) %>% 
  relocate(.after = OP_mtd, mtd_sales_vop) %>% 
  
  mutate(qtd_sales_vfcast = sales_actual_qtd - forecast_qtd,
         qtd_sales_vop    = sales_actual_qtd - OP_qtd,
         qtd_sales_vpy    = sales_actual_qtd - sales_PY_qtd) %>% 
  relocate(.after = forecast_qtd, qtd_sales_vfcast) %>% 
  relocate(.after = OP_qtd, qtd_sales_vop)

# after wider pivot, total rows= 1,440, there are 10 diff types of observations.



