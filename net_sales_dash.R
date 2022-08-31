
# <> Net Sales Structure <>  --------------------------------------------------------


# From Basic Organics structure, {COLLECTION/IMPORTATION}
# Iterative functionality to offer all Ad hocs organics across Regions, . 
# BA&R recursive refreshal as a result of HsGet formula by row. 

# Reference: 
# CFP&A Organic % Analysis (Corporate Ad Hoc). 


library(tidyverse)
library(lubridate)

net_sales_data         <- "C:/Users/AEG1130/Documents/Net Sales/"

setwd(net_sales_data)


# Build all the queries needed to extract the last 5 years P&L Data. 
query_meta <- function(observ_year, observ_month, product){ 
  
  file <- openxlsx::read.xlsx("ref/tidy_ns_query.xlsx") %>% as_tibble()
  
  
  chasis<- file %>% 
    janitor::clean_names() %>% 
    select(-years, -period) %>% 
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

years   <- tibble(period = 2018  : year(today()))

periods <-  map(years$period,
                timing) %>% 
  map_dfr(., bind_rows) 


# Products Iteration

product <- tibble(prd = c("Total Product", "PTG", "HTAS"))

fill.prd = function(prd){
  ns_struct %>% 
  mutate(product = prd)
}


nsales <- function(){ 
  
  ns_struct <- map2(periods$observ_year, 
                    periods$observ_month,
                    query_meta) %>% 
               map_dfr(., bind_rows) 
    
  
  ns_structure<-  map(product$prd,fill.prd) %>% 
               map_dfr(., bind_rows) %>% 
    
    arrange(desc(observation)) %>% 
    mutate(year_num = substr(observation, 3,4)) %>% 
    mutate(year_num = as.double(year_num)) %>% 
    mutate(year_num = ifelse(type == "actual_sales_PY", year_num-1, year_num)) %>% 
    mutate(year_num = ifelse(type == "actual_sales_2PY", year_num-2, year_num)) %>% 
    
    mutate(years = paste0("FY",year_num)) %>% 
    mutate(month = match(period, month.name)) %>%  
    mutate(ref_date = make_date(year = observation, month = month, day = 1L)) %>% 
    mutate(quarter = quarter(ref_date)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>%  
    # filter(ref_date <= today()%m-%months(1) %>% rollback(roll_to_first = T)) %>% 
    mutate(index = row_number()+1) %>% 
    mutate(index = as.character(index)) %>% 
    mutate(result = '=HsGetValue("PRD_OAC_RPTSBD01","Entity#"&E2,"Scenario#"&F2,"Years#"&N2,"Period#"&O2,"Account#"&G2,"Currency#"&H2,"Total Product#"&I2,"Total Customer#"&J2,"Function#"&K2,"DTS#"&P2,"Total Ship-to Geography#"&L2,"Total Brand#"&M2)/1000000') %>% 
    mutate(result = str_replace_all(result,'[[:digit:]]+',index)) %>% 
    mutate(result = str_replace_all(result,'/[[:digit:]]+',"/1000000")) %>% 
    mutate(result = str_replace_all(result,'PRD_OAC_RPTSBD[[:digit:]]+',"PRD_OAC_RPTSBD01")) %>%  
    relocate(.after = dts, result) %>%  
    relocate(.after = brand, years) %>% 
    relocate(.after = years, period) %>% 
    mutate(index = as.numeric(index)) %>% 
    mutate(result = ifelse(region_channel == "Retail Other", paste0("=Q",index-7,"-SUM(Q",index-6,":Q",index-1,")"),result)) %>% 
    mutate(result = ifelse(region_channel == "NA Other", paste0("=Q",index-11,"-Q",index-10,"-Q",index-2,"-Q",index-1),result)) %>% 
    mutate(result = ifelse(region_channel == "EMEA ANZ Other", paste0("=Q",index-7,"-SUM(Q",index-6,":Q",index-1,")"),result)) %>% 
    mutate(result = ifelse(region_channel == "LAG Other", paste0("=Q",index-4,"-SUM(Q",index-3,":Q",index-1,")"),result)) %>% 
    mutate(result = ifelse(region_channel == "Asia Other", paste0("=Q",index-5,"-SUM(Q",index-4,":Q",index-1,")"),result))
  
    
    ns_structure %>% openxlsx::write.xlsx("ns_struct.t.xlsx", overwrite = T)
    
    # 595*5*12*3
    
  return(ns_structure)
  
}

  
    # After getting the results from BAR, calculate Others. where:
    # Total Prod - PTG - HTAS == Other. 


    # ns_structure %>% 
    #   select(region_channel, type,
    #          region, product, result,ref_date) %>% 
    #   relocate(.after = product, ref_date)
