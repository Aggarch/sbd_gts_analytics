
# DS ' Daily Sales 

library(tidyverse)
library(lubridate)

myDocs         <- "C:/Users/AEG1130/Documents"

setwd(myDocs)

tidy_day <- openxlsx::read.xlsx("DS_Simplif.xlsx", "tidy") %>%
  as_tibble() %>% 
  mutate(Region = ifelse(is.na(Region), "NA" , Region)) %>% 
  janitor::clean_names()



calendar <- openxlsx::read.xlsx("DS_Simplif.xlsx", "calendar") %>% 
  as_tibble() %>% 
  mutate(Report.Date = as.Date(Report.Date,origin = "1899-12-30")) %>% 
  mutate(Data.Date = as.Date(Data.Date,origin = "1899-12-30")) %>% 
  janitor::clean_names()




# Squeleton of query Function

squeleton <- function(hfm_day){ 
  
  tidy_day %>% mutate(custom_number_2 = hfm_day) %>% 
    mutate(result = '=@HsGetValue("Entity#"&C2&";Scenario#"&D2&";Year#"&E2&";Period#"&F2&";View#"&G2&";Value#"&H2&";ICP#"&I2&";Account#"&J2&";Custom1#"&K2&";Custom2#"&L2&";Custom3#"&M2&";Custom4#"&N2&";")/1000000') %>% 
    mutate(result = str_trim(result)) %>% 
    set_names(., str_to_title) %>% 
    rename(ICP = Icp, 
           'Custom#1' = Custom_number_1,
           'Custom#2' = Custom_number_2,
           'Custom#3' = Custom_number_3,
           'Custom#4' = Custom_number_4
           
           )
  
}


# Iteration : 

bigquery <- 

  map(calendar$hfm_day, 
       squeleton) %>% 
    map_dfr(., bind_rows) 



# Full Table: 

perspective <- bigquery %>% left_join(calendar, by = c("Custom#2" = "hfm_day"))



