
# MTD Regional P&L Data. 

# 8 Blocks Reference. 

library(tidyverse)
library(lubridate)

PL_data         <- "C:/Users/AEG1130/Documents/P&L"

setwd(PL_data)


# Structured P&L Global 

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
OPG  <- PL_history %>% mutate(product = "OPG")
HTAS <- PL_history %>% mutate(product = "HTAS")



return(list(GEO  = GEO, 
            PTG  = PTG,
            OPG  = OPG,
            HTAS = HTAS))


}


# Export to fill HsGet SmartView: 

consolidated_history() %>%
  openxlsx::write.xlsx(.,"P&L_History.xlsx", overwrite = T)



# After the refreshal of the History: 
setwd(PL_data)

PL_filled <- openxlsx::read.xlsx("P&L_History.xlsx") %>% 
  as_tibble() %>% 
  mutate(region = ifelse(is.na(region),"NA",region)) %>% 
  mutate(ref_date = as.Date(ref_date,origin = "1899-12-30"))

PL_filled %>% openxlsx::write.xlsx(.,"PL.xlsx", overwrite = T)
  


