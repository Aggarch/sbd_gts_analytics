
# DS ' Daily Sales ETL ::: 


# Use as Reference GTS Daily Sales 2021 with Daily Shipments AEGA 
# C:\Users\AEG1130\Documents\DS_data\
# BigQuery\DSActuals & FCAST 
# DS_simplif


library(tidyverse)
library(lubridate)

DS_data         <- "C:/Users/AEG1130/Documents/DS_data"

setwd(DS_data)


get# Extract -----------------------------------------------------------------

# Algo \ HsGet SmartView Queries using a tabular structure 4 transact_data
# Dynamically create tibbles easy to debug or traceback 
# Well defined structure for Dashboard production. 
# Data Source: DS_Simplified Model, BA&R & HFM pull. 


# MetaQuery ::: -----------------------------------------------------------



calendar <- openxlsx::read.xlsx("DS_Simplif.xlsx", "calendar") %>% 
  as_tibble() %>% 
  mutate(Report.Date = as.Date(Report.Date,origin = "1899-12-30")) %>% 
  mutate(Data.Date = as.Date(Data.Date,origin = "1899-12-30")) %>% 
  mutate(quarter = quarter(Report.Date)) %>% 
  mutate(quarter = paste0("Q",quarter)) %>% 
  janitor::clean_names()


meta_query <- function(){ 


# DS_Actual :: ScenariaView -----------------------------------------------


actuals_perspective <- function(){ 


tidy_day <- openxlsx::read.xlsx("DS_Simplif.xlsx", "tidy_actual") %>%
  as_tibble() %>% 
  mutate(Region = ifelse(is.na(Region), "NA" , Region)) %>% 
  janitor::clean_names()






# Squeleton of query Function

squeleton <- function(hfm_day){ 
  
  tidy_day %>% mutate(custom_number_2 = hfm_day) %>% 
    mutate(result = '=@HsGetValue("Entity#"&D2&";Scenario#"&E2&";Year#"&F2&";Period#"&G2&";View#"&H2&";Value#"&I2&";ICP#"&J2&";Account#"&K2&";CustomUNO#"&L2&";CustomDOS#"&M2&";CustomTRES#"&N2&";CustomCUATRO#"&O2&";")/1000000') %>% 
    mutate(result = str_trim(result)) %>% 
    set_names(., str_to_title) %>% 
    rename(ICP = Icp, 
           'Custom#1' = Custom_number_1,
           'Custom#2' = Custom_number_2,
           'Custom#3' = Custom_number_3,
           'Custom#4' = Custom_number_4
           
           )
  
}


# first Iteration : 

bigquery <- 
  
  map(calendar$hfm_day, 
      squeleton) %>% 
  map_dfr(., bind_rows) 





# Last 5 years: 

periods = tibble(years = c("2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010"))



# Augmented: 

augmented_perspective <- function(periodo){ 
  
  
  bigquery %>% mutate(Year = periodo)
  
  
}


augmented_bq <- 
  
  map(periods$years,
      augmented_perspective) %>% 
  map_dfr(., bind_rows) 




# Perspective DS Actuals ScenarioView: 

perspective <- augmented_bq %>%
  mutate(index = row_number()+1) %>% 
  mutate(index = as.character(index)) %>%
  relocate(.before = Region, index) %>%
  mutate(Result = str_replace_all(Result,'[[:digit:]]+',index)) %>% 
  mutate(Result = str_replace_all(Result,'/[[:digit:]]+',"/1000000")) %>%  
  mutate(Result = str_replace_all(Result,c('CustomUNO#'    =  'Custom1#',
                                           'CustomDOS#'    =  'Custom2#',
                                           'CustomTRES#'   =  'Custom3#',
                                           'CustomCUATRO#' =  'Custom4#'))) %>% 
  mutate(index = as.numeric(index)) %>% 
  mutate(Result = ifelse(Region == "NA" & Entity == "Plug",
                         paste0("=P",index-4,"-SUM(P",index-3,":P",index-1,")"),Result)) %>% 
  mutate(index = as.character(index))



return(perspective)


}


# Function of Q, (Query of Queries): 

# Scenario: DSActual & View: ScenarioView 
DSActuals <- function(){ 

  
perspective <- actuals_perspective()

  
time_window <- c("2021","2020","2019","2018","2017","2016","2015")


dailyslstot <- perspective %>%
  filter(Year %in% time_window) %>% 
  rename(Actuals = Result)

  
cummean <- perspective %>%
  filter(Year %in% time_window) %>% 
  mutate(Account = "CumMean") %>% 
  rename(QR = Result) %>% 
  mutate(OP = QR)


projected <- perspective %>%
  filter(Year %in% time_window) %>% 
  mutate(Account = "Projected") %>% 
  rename(CP = Result)

lowtarget <- perspective %>%
  filter(Year %in% time_window) %>% 
  mutate(Account = "lowtarget") %>% 
  rename(lowtarget = Result)

meantarget <- perspective %>%
  filter(Year %in% time_window) %>% 
  mutate(Account = "meantarget") %>% 
  rename(meantarget = Result)

hightarget <- perspective %>%
  filter(Year %in% time_window) %>% 
  mutate(Account = "hightarget") %>% 
  rename(hightarget = Result)


return(list(dailyslstot = dailyslstot,
       cummean = cummean, 
       projected = projected,
       lowtarget = lowtarget,
       meantarget = meantarget,
       hightarget = hightarget))

}



# FCAST : Periodic View ---------------------------------------------------


fcast_perspective <- function(){


tidy_fcast <- openxlsx::read.xlsx("DS_Simplif.xlsx", "tidy_fcast") %>%
  as_tibble() %>% 
  mutate(Region = ifelse(is.na(Region), "NA" , Region)) %>% 
  janitor::clean_names()



calendar <- openxlsx::read.xlsx("DS_Simplif.xlsx", "calendar") %>% 
  as_tibble() %>% 
  mutate(Report.Date = as.Date(Report.Date,origin = "1899-12-30")) %>% 
  mutate(Data.Date = as.Date(Data.Date,origin = "1899-12-30")) %>% 
  janitor::clean_names()



# Squeleton of query Function

squeleton_f <- function(month){ 
  
  tidy_fcast %>% 
    mutate(period = month) %>% 
    mutate(result = '=@HsGetValue("PRD_OAC_RPTSBD01","Account#"&e2,"Period#"&f2,"Years#"&g2,"Currency#"&h2,"Scenario#"&i2,"Entity#"&j2,"Function#"&k2,"Total Product#"&l2,"Total Customer#"&m2,"Total Ship-to Geography#"&n2,"Total Brand#"&o2,"DTS#"&p2)/1000000') %>% 
    mutate(result = str_trim(result)) %>% 
    set_names(., str_to_title) %>% 
    rename('Total Product' = Total_product,
           'Total Ship-to Geography' = Total_ship_to_geography,
           'Total Brand' = Total_brand,
           'Total Customer' = Total_customer,
           'DTS' = Dts)
           
  }



# first Iteration : 

bigquery_f <- 
  
  map(unique(calendar$month), 
      squeleton_f) %>% 
  map_dfr(., bind_rows) 


# Last 10 years: 

periods = tibble(years = c("2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010"))


# Augmented: 

augmented_perspective_f <- function(periodo){ 
  
  
  bigquery_f %>% 
    mutate(Year = periodo) %>% 
    mutate(Years = paste0("FY",substr(Year,3,4)))
  
  
}


augmented_bq_f <- 
  
  map(periods$years,
      augmented_perspective_f) %>% 
  map_dfr(., bind_rows) 





# Perspective FCAST Periodic: 

perspective_f <- augmented_bq_f %>%
  mutate(index = row_number()+1) %>% 
  mutate(index = as.character(index)) %>%
  relocate(.before = Region, index) %>%
  mutate(Result = str_replace_all(Result,'[[:digit:]]+',index)) %>% 
  mutate(Result = str_replace_all(Result,'/[[:digit:]]+',"/1000000")) %>% 
  mutate(Result = str_replace_all(Result,'PRD_OAC_RPTSBD[[:digit:]]+',"PRD_OAC_RPTSBD01")) %>% 
    mutate(index = as.numeric(index)) %>% 
  mutate(Result = ifelse(Region == "NA" & Entity == "Plug",
                         paste0("=Q",index-4,"-SUM(Q",index-3,":Q",index-1,")"),Result)) %>% 
  mutate(index = as.character(index))


return(perspective_f)

}


# Scenario: FCST & View: Periodic 
FCAST <- function(){ 
  
  
  perspective_f <- fcast_perspective()
  
  
  time_window <- c("2021","2020","2019","2018","2017","2016","2015")
  
  
  FCASTCorp <- perspective_f %>%
    filter(Year %in% time_window) %>% 
    rename(FCAST10 = Result)
  
  budget_post <- perspective_f %>%
    filter(Year %in% time_window) %>% 
    mutate(Scenario = "budget_Post") %>% 
    rename(budget_post = Result)
  
  
  actual_post_alloc <- perspective_f %>%
    filter(Year %in% time_window) %>% 
    mutate(Scenario = "Actuals Post Allocation") %>% 
    rename(actual_post_alloc = Result)
  
  

  return(list(FCASTCorp  = FCASTCorp,
              budget_post = budget_post,
              actual_post_alloc = actual_post_alloc

              ))
  
}



# Export to get results in SmartView. 


DSActuals() %>% openxlsx::write.xlsx(., "BigQuery/DSActuals.xlsx", overwrite = T)
FCAST()     %>% openxlsx::write.xlsx(., "BigQuery/FCAST.xlsx", overwrite = T)
calendar    %>% openxlsx::write.xlsx(., "BigQuery/calendar.xlsx", overwrite = T)


}



# Transform ---------------------------------------------------------------

# Simulate the original DS Report, using as input the MetaQuery.
# metaQuery data comming directly from BAR & HFM Cubes. 
# All queries instead of orange section in Proj Tracking available. 



# Complete output : 

# Reduction and Time::: 


# # Cosolidated perspective :  --------------------------------------------


consolidated_perspective <- function(){ 


# DSActuals ::: ScenarioView  -------------------------------------------


dailyslstot <-  openxlsx::read.xlsx("BigQuery/DSActuals_filled.xlsx", "dailyslstot") %>%
  as_tibble %>% mutate(Region = ifelse(is.na(Region),"NA",Region)) %>% 
  select(index, Region, Category, Entity, Year, 'Custom#2',Actuals)
  
cummean     <-  openxlsx::read.xlsx("BigQuery/DSActuals_filled.xlsx", "cummean")     %>%
  as_tibble %>% mutate(Region = ifelse(is.na(Region),"NA",Region))%>% 
  select(index, Region, Category, Entity, Year, 'Custom#2',QR,OP)

projected   <-  openxlsx::read.xlsx("BigQuery/DSActuals_filled.xlsx", "projected")   %>%
  as_tibble %>% mutate(Region = ifelse(is.na(Region),"NA",Region))%>% 
  select(index, Region, Category, Entity, Year, 'Custom#2',CP)

lowtarget   <-  openxlsx::read.xlsx("BigQuery/DSActuals_filled.xlsx", "lowtarget")   %>%
  as_tibble %>% mutate(Region = ifelse(is.na(Region),"NA",Region))%>% 
  select(index, Region, Category, Entity, Year, 'Custom#2',lowtarget)

meantarget  <-  openxlsx::read.xlsx("BigQuery/DSActuals_filled.xlsx", "meantarget")  %>%
  as_tibble %>% mutate(Region = ifelse(is.na(Region),"NA",Region))%>% 
  select(index, Region, Category, Entity, Year, 'Custom#2',meantarget)

hightarget  <-  openxlsx::read.xlsx("BigQuery/DSActuals_filled.xlsx", "hightarget")  %>%
  as_tibble %>% mutate(Region = ifelse(is.na(Region),"NA",Region))%>% 
  select(index, Region, Category, Entity, Year, 'Custom#2',hightarget)


DS_actuals <-  dailyslstot %>% 
  left_join(cummean,    by = c("index", "Region", "Category", "Entity", "Year", "Custom#2")) %>% 
  left_join(projected,  by = c("index", "Region", "Category", "Entity", "Year", "Custom#2")) %>% 
  left_join(lowtarget,  by = c("index", "Region", "Category", "Entity", "Year", "Custom#2")) %>% 
  left_join(meantarget, by = c("index", "Region", "Category", "Entity", "Year", "Custom#2")) %>% 
  left_join(hightarget, by = c("index", "Region", "Category", "Entity", "Year", "Custom#2")) %>% 
  left_join(calendar, by = c("Custom#2" = "hfm_day")) 


# openxlsx::write.xlsx(DS_actuals, "/BigQuery/DSActuals_merged.xlsx")




# Forecasts ::: Periodic  -----------------------------------------------


  FCAST10 <-  openxlsx::read.xlsx("BigQuery/FCAST_filled.xlsx", "FCAST10") %>%
    as_tibble %>% mutate(Region = ifelse(is.na(Region),"NA",Region)) %>% 
    select(index, Region, Category, Year, Account, Period, Years, Entity,FCAST10)
  
  budget_post     <-  openxlsx::read.xlsx("BigQuery/FCAST_filled.xlsx", "budget_post")%>%
    as_tibble %>% mutate(Region = ifelse(is.na(Region),"NA",Region)) %>% 
    select(index, Region, Category, Year, Account, Period, Years, Entity,budget_post)
  
  
  actual_post_alloc   <-  openxlsx::read.xlsx("BigQuery/FCAST_filled.xlsx", "actual_post_alloc")   %>%
    as_tibble %>% mutate(Region = ifelse(is.na(Region),"NA",Region)) %>% 
    select(index, Region, Category, Year, Account, Period, Years, Entity, actual_post_alloc)
  
  
  
  forecasted <-
    
    FCAST10 %>%
    left_join(budget_post,       by = c("index","Region","Category","Year","Account","Period","Years","Entity")) %>%
    left_join(actual_post_alloc, by = c("index","Region","Category","Year","Account","Period","Years","Entity")) %>% 
    mutate(month = match(Period, month.abb)) %>%  
    mutate(ref_date = make_date(year = Year, month = month, day = 1L)) %>% 
    mutate(quarter = quarter(ref_date)) %>% 
    mutate(quarter = paste0("Q",quarter))
    

  
 # openxlsx::write.xlsx(forecasted, "/BigQuery/forecasted_merged.xlsx")
  
  



return(list(DS_actuals = DS_actuals,
            forecasted = forecasted,
            calendar   = calendar))


}




# Aggregation -------------------------------------------------------------


# Create a function to filter by day a build all the aggregations, 
# Calculated fields, and structure of the original report, 
# Inspiration in DSDash Notepad. 


# Ad Hoc ------------------------------------------------------------------



# DataExplorer Import to get Results and R from BAR & HFM 

datae <- openxlsx::read.xlsx("dataexplorer.xlsx") %>% 
    mutate(Region = ifelse(is.na(Region),"NA",Region)) %>% 
  as_tibble() %>% 
  left_join(calendar, by = c("Custom#2" = "hfm_day")) %>% 
  mutate(quarter = quarter(report_date)) %>% 
  mutate(quarter = paste0("Q",quarter))


# Example
sah <- openxlsx::read.xlsx("sales_actuals_history.xlsx") %>% 
  as_tibble() %>% 
  mutate(Region = ifelse(is.na(Region),"NA",Region)) %>% 
  left_join(calendar, by = c("Custom#2" = "hfm_day")) %>% 
  mutate(quarter = quarter(report_date)) %>% 
  mutate(quarter = paste0("Q",quarter))
  
sah %>% openxlsx::write.xlsx("sales_actuals_history.xlsx", overwrite = T)

