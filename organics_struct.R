
# <> Organics Structure <>  --------------------------------------------------------


# From Basic Organics structure, {COLLECTION/IMPORTATION}
# Iterative functionality to offer all Ad hocs organics across Regions, . 
# BA&R recursive refreshal as a result of HsGet formula by row. 

# Reference: 
# CFP&A Organic % Analysis (Corporate Ad Hoc). 


library(tidyverse)
library(lubridate)
library(readr)


organics_data         <- "C:/Users/AEG1130/Documents/organics/"

setwd(organics_data)



##########################################################################


organics_data         <- "C:/Users/AEG1130/Documents/Reconcilation"

setwd(organics_data)

data_global = read_csv("SBU_GLOBAL.csv")


data = data_global %>% 
  as_tibble %>% 
  janitor::clean_names()


# saveRDS(data, "data_global")


global_sales = read_rds("data_global")


global_sales

cols = global_sales %>% colnames %>%  as_tibble()


##########################################################################


data_global_billing = read_csv("SBU_GLOBAL_BILLING.csv")


data_billing = data_global_billing %>% 
  as_tibble %>% 
  janitor::clean_names()


#saveRDS(data_billing, "data_global_billing")

global_billing = read_rds("data_global_billing")



##########################################################################


# Structured P&L Global ---------------------------------------------------

# Build all the queries needed to extract the last 5 years P&L Data. 
query_meta <- function(observ_year, observ_month){ 
  
  file <- openxlsx::read.xlsx("ref/tidy_organics_query.xlsx") %>% as_tibble() %>% 
    filter(!grepl("qtd", Type)) %>% filter(!grepl("organic", Type))
    
  
  chasis<- file %>% 
    janitor::clean_names() %>% 
    mutate(region = ifelse(is.na(region),"NAm",region)) %>% 
    select(-years) %>% 
    mutate(observation = observ_year,
           period = observ_month) 
  
  return(chasis)
  
}


# Times & Iterations 
timing <- function(period){ 
  
  times <- tibble(observ_month  = month.name) %>% 
    mutate(observ_year = period)
  
  return(times)
  
}

years   <- tibble(period = 2015  : year(today()))

periods <-  map(years$period, 
                timing) %>% 
  map_dfr(., bind_rows) 




# BA&R Data Pulling  ---------------------------------------------------------

organics <- function(){ 
  
  organics_structure <- map2(periods$observ_year, 
                             periods$observ_month,
                             query_meta) %>% 
    
    map_dfr(., bind_rows) %>% 
    arrange(desc(observation)) %>% 
    mutate(year_num = substr(observation, 3,4)) %>% 
    mutate(year_num = as.double(year_num)) %>% 
    mutate(year_num = ifelse(type == "sales_PY_mtd", year_num-1, year_num)) %>% 
    mutate(year_num = ifelse(type == "sales_PY_qtd", year_num-1, year_num)) %>% 

    mutate(years = paste0("FY",year_num)) %>% 
    mutate(month = match(period, month.name)) %>%  
    mutate(ref_date = make_date(year = observation, month = month, day = 1L)) %>% 
    mutate(quarter = quarter(ref_date)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>% 
    mutate(geo =  case_when(region == "US" ~ "North America",
                            region == "NAm" ~ "North America",
                            region == "Canada" ~ "North America",
                            region == "EMEA ANZ" ~ "Europe & ANZ",
                            region == "LAG" ~ "Latin America",
                            region == "ASIA" ~ "Asia")) %>% 
    relocate(.before = period, years)
  
  
  organics_history <- organics_structure %>% 
    # filter(ref_date <= today()%m-%months(1) %>% rollback(roll_to_first = T)) %>% 
    mutate(index = row_number()+1) %>% 
    mutate(index = as.character(index)) %>% 
    mutate(result = '=HsGetValue("PRD_OAC_RPTSBD01","Entity#"&D2,"Scenario#"&E2,"Years#"&M2,"Period#"&N2,"Account#"&F2,"Currency#"&G2,"Total Product#"&H2,"Total Customer#"&I2,"Function#"&J2,"DTS#"&O2,"Total Ship-to Geography#"&K2,"Total Brand#"&L2)') %>% 
    mutate(result = str_replace_all(result,'[[:digit:]]+',index)) %>% 
    mutate(result = str_replace_all(result,'/[[:digit:]]+',"/1000000")) %>% 
    mutate(result = str_replace_all(result,'PRD_OAC_RPTSBD[[:digit:]]+',"PRD_OAC_RPTSBD01")) %>%  
    relocate(.after = dts, result) %>% 
    select(-value)

  # organics_current <- organics_structure %>% 
  #   filter(ref_date == today()%m-%months(0) %>% rollback(roll_to_first = T)) %>%
  #   mutate(index = row_number()+1) %>% 
  #   mutate(index = as.character(index)) %>% 
  #   mutate(result = '=HsGetValue("PRD_OAC_RPTSBD01","Entity#"&D2,"Scenario#"&E2,"Years#"&M2,"Period#"&N2,"Account#"&F2,"Currency#"&G2,"Total Product#"&H2,"Total Customer#"&I2,"Function#"&J2,"DTS#"&O2,"Total Ship-to Geography#"&K2,"Total Brand#"&L2)') %>% 
  #   mutate(result = str_replace_all(result,'[[:digit:]]+',index)) %>% 
  #   mutate(result = str_replace_all(result,'/[[:digit:]]+',"/1000000")) %>% 
  #   mutate(result = str_replace_all(result,'PRD_OAC_RPTSBD[[:digit:]]+',"PRD_OAC_RPTSBD01")) %>%  
  #   relocate(.after = dts, result) %>% 
  #   select(-value) 
  
  
  organics_history %>% openxlsx::write.xlsx(.,"ref/organics_history.xlsx", overwrite = T)
  # organics_current %>% openxlsx::write.xlsx(.,"ETL/organics_current.xlsx", overwrite = T)
  
  
  return(list(organics_history = organics_history
              # organics_current = organics_current
              
              ))
  
  
}

# Script for writing the metadata, with the query, just historical data,
# and current month, future structures refers to same function where (t >= RToday)
# Note: time window filter must be always before formulation against indices. 
# the output of this function, named "organics_history.xlsx" located at organics_data path
# must be BA&R refreshed to be pulled and trigger the transformation cycle. 
# organics_history was recently refreshed we can go directly to summary section. 

organics()

# organics_history.xlsx contains history + one month 


# Summarization -----------------------------------------------------------

# Refresh BAR/HFM {ref/organics_history.xlsx} B4 running : 

organics_summary <- function(){ 
  
  # Recursive Table
  org_summ <- openxlsx::read.xlsx("ref/organics_history.xlsx") %>% as_tibble() %>% 
    mutate(ref_date = as.Date(ref_date, origin = "1899-12-30")) %>% 
    filter(ref_date <= rollback(today()%m-%months(1),roll_to_first = T)) %>% 
    mutate(region = ifelse(is.na(region),"NAm",region)) %>% 
    group_by(geo, region, channel,
             type,
             ref_date,observation,month,period,quarter) %>%
    summarise(result = sum(result),.groups = "drop") %>% 
    pivot_wider(names_from = type, values_from = result) %>% 
    rename(forecast_mtd = forecast_10_mtd) %>% 
    select(ref_date, observation, month, period, quarter,
           geo, region, channel,
           sales_actual_mtd,
           forecast_mtd, OP_mtd, sales_PY_mtd,
           sales_fx_mtd, sales_acqdiv_mtd) %>%   
    
    mutate(mtd_sales_vfcast = sales_actual_mtd - forecast_mtd,
           mtd_sales_vop    = sales_actual_mtd - OP_mtd,
           mtd_sales_vpy    = sales_actual_mtd - sales_PY_mtd) %>% 
    relocate(.after = forecast_mtd,mtd_sales_vfcast) %>% 
    relocate(.after = OP_mtd, mtd_sales_vop) %>% 
    relocate(.after = sales_PY_mtd, mtd_sales_vpy)%>%
    unite(region_channel, region, channel, sep = " / ")
  
  
  
  # after wider pivot, total rows= 1,440, there are 10 diff types of observations.
  # organize with ui-sec as original. 
  
  ui_sec <- openxlsx::read.xlsx("ref/ui_sec.xlsx") %>% 
    as_tibble() %>% mutate(region = ifelse(is.na(region),"NAm",region))
  
  
  organics_summ <- left_join(ui_sec, org_summ,
                             by = c("region_channel")) %>% 
    relocate(.before = region_channel, geo)
  
  
  organics_summ %>% openxlsx::write.xlsx(.,
                                         "PBI/organics_summ.xlsx",
                                         sheetName = "summary",  overwrite = T)
  
  

  
  
  return(list(organics_summ = organics_summ))
  
}

# Wrangling Algorithm / PBI dashboard Input.

organics_summary()
