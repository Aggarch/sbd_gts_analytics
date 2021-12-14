
#  REMOTE Functions
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
  
  Completed_PNL <- PNL_verticals %>% 
    
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
  
  
  
  
  return(Completed_PNL)
  
}


#  Query Meta 
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


