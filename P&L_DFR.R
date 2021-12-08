

# <> P&L Data Feeds <>  --------------------------------------------------------


# {UPDATE/RUN} <- Data feeds & Running, Final update to visualize. 
# Iterative functionality to offer all PNLs Calculations Verticals & Horizontals. 
# All the calculations are existing in the BAR 8 Blocks report. 
# iterative creation of all P&Ls Maths see: ispace below. 
# Just for the updated or last observation, conserve and recycle history. 


# Process  

# P&L Function ------------------------------------------------------------


# (f○g)(x) = f (g(x)),
library(tidyverse)
library(lubridate)


# Data Feed ---------------------------------------------------------------

# Directory
PL_data         <- "C:/Users/AEG1130/Documents/P&L"
setwd(PL_data)

source("P&L_Maths.R")


# Time
moment <- rollback(today(),roll_to_first = T) %m-% months(1)


# Creates new data feed to appendice to stored values.
# Go To BA&R and refresh data feed, ref as PL_to_update
PL_update <- function(){ 
  
  moment <- rollback(today(),roll_to_first = T) %m-% months(1)
  
  PL_filled <- openxlsx::read.xlsx("PL_filled.xlsx") %>% 
    as_tibble() %>% 
    mutate(region = ifelse(is.na(region),"NA",region)) %>% 
    mutate(ref_date = as.Date(ref_date,origin = "1899-12-30")) %>% 
    filter(ref_date < moment) 
  
  
  appendix <- PL_filled %>%
    filter(ref_date == max(ref_date)) %>% 
    mutate(ref_date =  ref_date %m+% months(1)) %>% 
    mutate(quarter = quarter(ref_date)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>% 
    mutate(observation = year(ref_date)) %>% 
    mutate(month = month(ref_date)) %>% 
    mutate(period = month.name[month]) %>% 
    mutate(result = as.character(result)) %>% 
    mutate(result = '=@HsGetValue("PRD_OAC_RPTSBD01","Account#"&G2,"Period#"&H2,"Years#"&I2,"Currency#"&J2,"Scenario#"&K2,"Entity#"&F2,"Function#"&L2,"Total Product#"&M2,"Total Customer#"&N2,"Total Ship-to Geography#"&O2,"Total Brand#"&P2,"DTS#"&Q2)/1000000') 
  
  
  setwd(PL_data)
  
  to_update <- appendix %>% 
    mutate(index  = row_number()+1) %>% 
    mutate(index  = as.character(index)) %>%
    mutate(result = str_replace_all(result,'[[:digit:]]+',index)) %>% 
    mutate(result = str_replace_all(result,'/[[:digit:]]+',"/1000000")) %>% 
    mutate(result = str_replace_all(result,'PRD_OAC_RPTSBD[[:digit:]]+',"PRD_OAC_RPTSBD01")) 
  
  
  
  openxlsx::write.xlsx(to_update,"PL_to_update.xlsx", overwrite = T)
  
  
  
  print("Done, HsGet BA&R Formula needs to be REFRESH:")
  
  print(getwd())
  
  directory <-list.files() %>% as_tibble()
  
  print(directory)
  
  
  return(to_update)
  
  
  
}

# REFRESH SMARTVIEW

# Appendices new data feed into the historical base,
# Execute after refreshal of the PL_update output.
# Recreates the PL_filled based file, binding rows of 
# previous records with the new feed. 
PL_apendice <- function(){
  
  
  PL_filled <- openxlsx::read.xlsx("PL_filled.xlsx") %>% 
    as_tibble() %>% 
    mutate(region = ifelse(is.na(region),"NA",region)) %>% 
    mutate(ref_date = as.Date(ref_date,origin = "1899-12-30")) %>% 
    filter(ref_date < moment) 
  
  
  PL_updated <- openxlsx::read.xlsx("PL_to_update.xlsx") %>% 
    as_tibble() %>% 
    mutate(region = ifelse(is.na(region),"NA",region)) %>% 
    mutate(ref_date = as.Date(ref_date,origin = "1899-12-30"))
  
  
  PNL_updated <- PL_filled %>% 
    bind_rows(PL_updated)  
  
  # Looks fine ...
  # Recreate the PL_filled version to make it recursive/recurrent
  
  
  PNL_updated %>% openxlsx::write.xlsx(.,"PL_filled.xlsx", overwrite = T)
  
  
  
}  


# Importation -------------------------------------------------------------


Profit_Loss_update <- function(){ 

PL_filled <- openxlsx::read.xlsx("PL_filled.xlsx") %>% 
  as_tibble() %>% 
  mutate(region = ifelse(is.na(region),"NA",region)) %>% 
  mutate(ref_date = as.Date(ref_date,origin = "1899-12-30"))


ProfitL <- openxlsx::read.xlsx("Profit_Loss.xlsx") %>% 
  as_tibble() %>% 
  mutate(region = ifelse(is.na(region),"NA",region)) %>% 
  mutate(month = match(period, month.name)) %>%  
  mutate(ref_date = make_date(year = observation, month = month, day = 1L)) %>% 
  filter(ref_date < moment)

PL_filled_last <- PL_filled %>% filter(ref_date == max(ref_date))


ispace <- PL_filled_last %>%
  distinct(ref_date, product, channel_cluster) %>% 
  rename(iref_date = ref_date,
         iproduct = product, 
         ichannel_cluster = channel_cluster)



Consolidated_PNL_Refresh <- pmap(ispace, PNL)  


Profit_Loss_feed <- Consolidated_PNL_Refresh %>%
  map_dfr(., bind_rows) %>% 
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.))  %>%
  mutate_if(~ any(is.infinite(.)),~ if_else(is.infinite(.),0,.)) %>% 
  arrange(observation, period, indicative)



Profit_Loss_updated <- ProfitL %>%
  bind_rows(Profit_Loss_feed)

openxlsx::write.xlsx(Profit_Loss_updated, "Profit_Loss.xlsx", overwrite = T) 


print("P&L Updated")  


return(Profit_Loss_updated)

}



# ProfitL %>% openxlsx::write.xlsx(.,"Profit_Loss.xlsx", overwrite = T) 












