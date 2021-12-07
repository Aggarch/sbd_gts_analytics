
# P&L Function ------------------------------------------------------------


# (f○g)(x) = f (g(x)),
library(tidyverse)
library(lubridate)


# Importation -------------------------------------------------------------

PL_data         <- "C:/Users/AEG1130/Documents/P&L"
setwd(PL_data)


PL_filled <- openxlsx::read.xlsx("PL_filled.xlsx") %>% 
  as_tibble() %>% 
  mutate(region = ifelse(is.na(region),"NA",region)) %>% 
  mutate(ref_date = as.Date(ref_date,origin = "1899-12-30"))


# Iteration Space:
ispace <- PL_filled %>% 
  select(ref_date, product, channel_cluster) %>%
  distinct() %>% 
  rename(iref_date = ref_date,
         iproduct = product, 
         ichannel_cluster = channel_cluster)



# P&L Function: -----------------------------------------------------------

PNL <- function(iref_date, iproduct, ichannel_cluster){ 
  
  
  # Moments   
  date_n0 <- iref_date %>% as.Date()
  date_n1 <- date_n0 %m-% years(1)
  date_n2 <- date_n0 %m-% years(2)
  date_n3 <- date_n0 %m-% years(3)
  
  
  
  PL_data <- PL_filled %>% select(-currency, -Function,
                                  -ship.to, -DTS, -index,
                                  -entity, -account, -brand) %>% 
    
    filter(ref_date %in% c( date_n0, date_n1, date_n2, date_n3)) %>% 
    
    filter(product         == iproduct, 
           channel_cluster == ichannel_cluster) %>% 
    
    pivot_wider(names_from = scenario, values_from = result)
  
  
  
  # Moment Zero 
  i_observation <- PL_data %>% 
    filter(ref_date == date_n0) %>% 
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
  
  # Third Moment
  t_observation <- PL_data %>% 
    filter(ref_date == date_n3) %>% 
    select(region, channel_cluster,category,period,ACTUAL_POST) %>% 
    rename(PPPY = ACTUAL_POST) 
  
  
  
  
  PNL_observations <- 
    i_observation %>%
    left_join(f_observation,by = c("region", "channel_cluster", "category","period")) %>% 
    left_join(s_observation,by = c("region", "channel_cluster", "category","period")) %>% 
    left_join(t_observation,by = c("region", "channel_cluster", "category","period"))  
  
  
  
  # Calculation of Values  ----------------------------------------------------
  
  
  accounts_spread <- function(scena){                                                        
    
    coords <- c("observation", "quarter", "period", 
                "region", "channel_cluster", "customer", 
                "product", "category")
    
    
    # Scena is equivalent to each of the Columns Scenarios. 
    accounts <-    PNL_observations %>%
      select(contains(coords), all_of(scena)) %>% 
      pivot_wider(names_from = category,
                  values_from = all_of(scena)) %>% 
      mutate(scenario = all_of(scena))
    
    
    
    previous_sales <-     PNL_observations %>% 
      select(contains(coords),"PY","PPY","PPPY") %>% 
      filter(category == "Net Sales") %>% 
      mutate(category = "PY Sales")
    
    
    
    measures <-      accounts %>% 
      left_join(previous_sales,
                by = c("observation", "quarter", "period",
                       "region", "channel_cluster", "customer", "product")) %>% 
      select(-category) %>% 
      
      mutate(Organic_usd = `Net Sales` - PY - `Sales FX` - `Acq Div`) %>% 
      mutate(Organic_usd = ifelse(scena == "PY", 
                                  `Net Sales` - PPY - `Sales FX` - `Acq Div`, Organic_usd)) %>% 
      mutate(Organic_usd = ifelse(scena == "PPY", 
                                  `Net Sales` - PPPY - `Sales FX` - `Acq Div`, Organic_usd)) %>% 
      
      mutate(Organic_perc = (Organic_usd/PY)) %>% 
      mutate(Organic_perc = ifelse(scena == "PY",
                                   Organic_usd/PPY, Organic_perc)) %>% 
      mutate(Organic_perc = ifelse(scena == "PPY",
                                   Organic_usd/PPPY, Organic_perc)) %>% 
      
      mutate(SGM_perc  = (SGM/`Net Sales`) ) %>% 
      mutate(AGM_perc  = (AGM/`Net Sales`) ) %>% 
      mutate("SG&A_perc" = (`SG&A`/`Net Sales`)) %>% 
      mutate(OM_perc  = (OM/`Net Sales`) ) %>% 
      mutate(CM  = (SGM -`SG&A`)) %>% 
      mutate(CM_perc  = (CM/`Net Sales`) ) %>% 
      
      
      
      
      # ALL MEASURES HERE ! 
      
      pivot_longer(!c(observation, quarter, period,scenario,
                      region, channel_cluster, customer, product),
                   names_to = "accounts", values_to = all_of(scena))
    
    
    return(measures)
    
  }
  
  
  
  actuals_post <- accounts_spread("ACTUAL_POST") %>% 
    filter(!accounts %in% c("PPY","PPPY")) %>% 
    mutate(accounts = ifelse(accounts == "PY", "PY_sales", accounts)) %>% 
    select(-scenario)
  
  
  fcst_post    <- accounts_spread("FCSTCORP_POST")%>% 
    filter(!accounts %in% c("PPY","PPPY")) %>% 
    mutate(accounts = ifelse(accounts == "PY", "PY_sales", accounts)) %>% 
    select(-scenario)
  
  
  
  budget_post  <- accounts_spread("BUDGET_POST")%>% 
    filter(!accounts %in% c("PPY","PPPY")) %>% 
    mutate(accounts = ifelse(accounts == "PY", "PY_sales", accounts)) %>% 
    select(-scenario)
  
  
  
  PY           <- accounts_spread("PY")%>% 
    filter(!accounts %in% c("PY","PPPY")) %>% 
    mutate(accounts = ifelse(accounts == "PPY", "PY_sales", accounts)) %>% 
    select(-scenario)
  
  
  
  PPY          <- accounts_spread("PPY")%>% 
    filter(!accounts %in% c("PY","PPY")) %>% 
    mutate(accounts = ifelse(accounts == "PPPY", "PY_sales", accounts)) %>% 
    select(-scenario)
  
  
  
  PNL_horizontals <-
    actuals_post %>% 
    left_join(fcst_post, by = c("observation", "quarter", "period", "region",
                                "channel_cluster", "customer", "product", "accounts")) %>% 
    
    left_join(budget_post, by = c("observation", "quarter", "period", "region",
                                  "channel_cluster", "customer", "product", "accounts")) %>%  
    
    left_join(PY, by = c("observation", "quarter", "period", "region",
                         "channel_cluster", "customer", "product", "accounts")) %>% 
    
    left_join(PPY,by = c("observation", "quarter", "period", "region",
                         "channel_cluster", "customer", "product", "accounts")) 
  
  
  
  
  PNL_verticals <- 
    PNL_horizontals %>% 
    mutate(VFCAST_usd = ACTUAL_POST - FCSTCORP_POST,
           VFCAST_perc = VFCAST_usd/FCSTCORP_POST) %>%
    mutate(VOP_usd  = ACTUAL_POST - BUDGET_POST,
           VOP_perc = VOP_usd/BUDGET_POST) %>%
    
    
    # LAST MEASURES
    
    mutate(VPY_usd = ACTUAL_POST  - PY,
           VPY_perc= VPY_usd/PY) %>%
    mutate(VPPY_usd = ACTUAL_POST  - PPY,
           VPPY_perc= VPPY_usd/PPY) %>%
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>%
    select(observation,quarter,period,
           region,channel_cluster,
           customer, product,accounts,
           ACTUAL_POST,FCSTCORP_POST,
           VFCAST_usd, VFCAST_perc,
           BUDGET_POST, VOP_usd, VOP_perc,
           PY, VPY_usd, VPY_perc,
           PPY, VPPY_usd, VPPY_perc)
  
  
  # Create Accounts Indicatives ~~ 21
  
  THE_PNL <- PNL_verticals %>% 
    
    mutate(indicative = case_when(accounts == "Net Sales"     ~ 1,
                                  accounts == "Sales FX"      ~ 2,
                                  accounts == "Acq Div"       ~ 3,
                                  accounts == "Organic_usd"   ~ 4,
                                  accounts == "PY_sales"      ~ 5,
                                  accounts == "Organic_perc"  ~ 6,
                                  accounts == "SGM"           ~ 7,
                                  accounts == "SGM_perc"      ~ 8,
                                  accounts == "OCOS"          ~ 9,
                                  accounts == "AGM"           ~ 10,
                                  accounts == "AGM_perc"      ~ 11,
                                  accounts == "SG&A"          ~ 12,
                                  accounts == "SG&A_perc"     ~ 13,
                                  accounts == "OM"            ~ 14,
                                  accounts == "OM_perc"       ~ 15,
                                  accounts == "CM"            ~ 16,
                                  accounts == "CM_perc"       ~ 17)) %>% 
    relocate(.before = accounts, indicative) %>%
    mutate(accounts = case_when(
      accounts == "Organic_usd"   ~ "Organic $",
      accounts == "PY_sales"      ~ "PY Sales",
      accounts == "Organic_perc"  ~ "Organic %",
      accounts == "SGM_perc"      ~ "SGM %",
      accounts == "SG&A_perc"     ~ "SG&A %",
      accounts == "AGM_perc"      ~ "AGM %",
      accounts == "OM_perc"       ~ "OM %",
      accounts == "CM_perc"       ~ "CM %",
      TRUE ~ as.character(accounts))) 
  
  
  
  
  return(THE_PNL)
  
}



# Cross Iteration  --------------------------------------------------------


# Explorer with parallel processing to loop 

THE_PNL <- pmap(ispace, PNL)  

Profit_Loss <- THE_PNL %>%
  map_dfr(., bind_rows) %>% 
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.))  %>%
  mutate_if(~ any(is.infinite(.)),~ if_else(is.infinite(.),0,.))


  

