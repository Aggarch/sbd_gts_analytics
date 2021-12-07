
# MTD Regional P&L Data. 

# 8 Blocks Reference. 

library(tidyverse)
library(lubridate)

PL_data         <- "C:/Users/AEG1130/Documents/P&L"

setwd(PL_data)



# Structured P&L Global ---------------------------------------------------

# Build all the queries needed to extract the last 5 years P&L Data. 


query_meta <- function(observ_year, observ_month){ 

file <- openxlsx::read.xlsx("P&L_Table.xlsx") %>% as_tibble()

chasis<- file %>% 
  mutate(region = ifelse(is.na(region),"NA",region)) %>% 
  mutate(observation = observ_year,
         period = observ_month) %>% 
  mutate(year_num = substr(observation, 3,4)) %>% 
  mutate(years = paste0("FY",year_num)) %>% 
  select(-year_num, -index) 


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


# Construction of history: 

consolidated_history <- function(){ 

PL_history <- map2(periods$observ_year, 
                   periods$observ_month,
                   query_meta) %>% 
              map_dfr(., bind_rows) %>% 
              arrange(desc(observation)) %>% 
              mutate(index = row_number()+1) %>% 
              mutate(index = as.character(index)) %>% 
  relocate(.before = observation, index) %>% 
  mutate(result = '=@HsGetValue("PRD_OAC_RPTSBD01","Account#"&G2,"Period#"&H2,"Years#"&I2,"Currency#"&J2,"Scenario#"&K2,"Entity#"&F2,"Function#"&L2,"Total Product#"&M2,"Total Customer#"&N2,"Total Ship-to Geography#"&O2,"Total Brand#"&P2,"DTS#"&Q2)/1000000') %>% 
  mutate(result = str_replace_all(result,'[[:digit:]]+',index)) %>% 
  mutate(result = str_replace_all(result,'/[[:digit:]]+',"/1000000")) %>% 
  mutate(result = str_replace_all(result,'PRD_OAC_RPTSBD[[:digit:]]+',"PRD_OAC_RPTSBD01"))%>%  
  mutate(month = match(period, month.name)) %>%  
  mutate(ref_date = make_date(year = observation, month = month, day = 1L)) %>% 
  mutate(quarter = quarter(ref_date)) %>% 
  mutate(quarter = paste0("Q",quarter))


# All Regions Total Products :::
GEO  <- PL_history

# SBUs Across all Regions ::: 
PTG  <- PL_history %>% mutate(product = "PTG")
OPG  <- PL_history %>% mutate(product = "OUT")
HTAS <- PL_history %>% mutate(product = "HTAS")



PL_hist <- GEO %>% 
  bind_rows(PTG) %>% 
  bind_rows(OPG) %>% 
  bind_rows(HTAS)



return(PL_hist)


}



# Consolidation & Population ---------------------------------------------

# Export to fill HsGet SmartView: 

consolidated_history() %>%
  openxlsx::write.xlsx(.,"P&L_History.xlsx", overwrite = T)



# After the refreshal of the History: 
setwd(PL_data)


# Fill the data using BA&R 
  PL_filled <- openxlsx::read.xlsx("P&L_History.xlsx") %>% 
    as_tibble() %>% 
    mutate(region = ifelse(is.na(region),"NA",region)) %>% 
    mutate(ref_date = as.Date(ref_date,origin = "1899-12-30"))
  
  
# PL_filled %>% openxlsx::write.xlsx(.,"PL_filled.xlsx", overwrite = T)
  


# ETL Logical Structure ---------------------------------------------------

# Read Existent data 
  PL_db <- 
    openxlsx::read.xlsx("PL_filled.xlsx") %>% 
    as_tibble() %>% 
    mutate(region = ifelse(is.na(region),"NA",region)) %>% 
    mutate(ref_date = as.Date(ref_date,origin = "1899-12-30")) %>% 
    mutate(result = as.character(result))
  

# Aggregate the new queries to get new Data of new period
  PL_feed <- 
  consolidated_history() %>% 
    filter(ref_date >= lubridate::rollback(today(), roll_to_first = T))
  
  
# Add new data to complete perspective perspective :    
  PL_update <- 
  PL_db %>% bind_rows(PL_feed)
  
  
# Refresh data CTRL F replace Equals & refresh 
  PL_update %>% openxlsx::write.xlsx(.,"PL_filled.xlsx")
  


# P&L Function ------------------------------------------------------------

  # (f○g)(x) = f (g(x)),
  
  
  # Iteration Space:
  ispace <- PL_filled %>% 
    select(ref_date , channel_cluster, product) %>%
    distinct() %>% 
    rename(iref_date = ref_date,
           iproduct = product, 
           ichannel_cluster = channel_cluster)

  
  
  
  # P&L Function: 
  PNL <- function(iref_date, iproduct, ichannel_cluster){ 
  
  
  # Moments   
  date_n0 <- iref_date %>% as.Date()
  date_n1 <- date_n0 %m-% years(1)
  date_n2 <- date_n0 %m-% years(2)
    

  PL_data <- PL_filled %>% select(-currency, -Function,
                                  -ship.to, -DTS, -index,
                                  -entity, -account, -brand) %>% 
    
    filter(ref_date %in% c( date_n0, date_n1, date_n2)) %>% 
    
    filter(product         == iproduct, 
           channel_cluster == ichannel_cluster) %>% 
  
    mutate(indicative = case_when(category == "Net Sales" ~ 1,
                                  category == "Sales FX"  ~ 2,
                                  category == "Acq Div"   ~ 3, 
                                  category == "SGM"       ~ 4, 
                                  category == "OCOS"      ~ 5, 
                                  category == "AGM"       ~ 6, 
                                  category == "SG&A"      ~ 7, 
                                  category == "OM"        ~ 8)) %>% 
    pivot_wider(names_from = scenario, values_from = result)
  
  
  
  # Moment Zero 
   i_observation <- PL_data %>% 
     filter(ref_date == date_n0) %>% 
     mutate(VFCAST_usd = ACTUAL_POST - FCSTCORP_POST,
            VFCAST_perc = VFCAST_usd/FCSTCORP_POST) %>% 
     mutate(VOP_usd  = ACTUAL_POST - BUDGET_POST,
            VOP_perc = VOP_usd/BUDGET_POST) %>% 
     mutate(REF_REAL = ACTUAL_POST) %>% 
     # rename_at(., vars( contains(c("ACTUAL","POST","VFCAST","VOP"))), list( ~paste0(.,"_",year(date_n0)))) %>% 
     select(-years, -month, -ref_date)


  # First Moment
   f_observation <- PL_data %>% 
     filter(ref_date == date_n1 ) %>% 
     select(region, channel_cluster,category,period,ACTUAL_POST) %>%
     rename(PY = ACTUAL_POST)
     
   
  # Second Moment
   s_observation <- PL_data %>% 
     filter(ref_date == date_n2 ) %>% 
     select(region, channel_cluster,category,period,ACTUAL_POST) %>% 
     rename(PPY = ACTUAL_POST) 
   
   
   
     
   PNL_observations <- 
               i_observation %>%
     left_join(f_observation,by = c("region", "channel_cluster", "category","period")) %>% 
     left_join(s_observation,by = c("region", "channel_cluster", "category","period")) %>% 
     mutate(VPY_usd = REF_REAL - PY,
            VPY_perc= VPY_usd/PY) %>%
     mutate(VPPY_usd = REF_REAL - PPY,
            VPPY_perc= VPPY_usd/PPY) %>%
     mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>% 
     select(observation,quarter,period,
            region,channel_cluster,
            customer, product,indicative,category,
            ACTUAL_POST,FCSTCORP_POST,
            VFCAST_usd, VFCAST_perc,
            BUDGET_POST, VOP_usd, VOP_perc,
            PY, VPY_usd, VPY_perc,
            PPY, VPPY_usd, VPPY_perc)
   
   
   
   
   return(PNL_observations)
   
  }
  
  
  
  

# Calculation of Rows  ----------------------------------------------------

   
                                                           
   
     coords <- c("observation", "quarter", "period", 
                 "region", "channel_cluster", "customer", 
                 "product", "indicative", "category")
   
     
     accounts_spread <-    PNL_observations %>%
       select(contains(coords), contains("ACTUAL_POST")) %>% 
       select(-indicative) %>% 
         pivot_wider(names_from = category, values_from = contains("ACTUAL_POST"))
     
     
     
     previous_sales <-     PNL_observations %>% 
       select(contains(coords),"PY" ,"PPY") %>% 
       filter(category == "Net Sales")
       
     
     accounts_spread %>% 
       left_join(previous_sales,
                 by = c("observation", "quarter", "period",
                        "region", "channel_cluster", "customer", "product"))
     
   

   
  
  
   

# Cross Iteration  --------------------------------------------------------


  

  # Explorer script with paralleling process to loop 
    
  PL_PL <- pmap(ispace, PNL) %>% 
    
  PL_condensed <- PL_PL %>%   map_dfr(., bind_rows) 
    
