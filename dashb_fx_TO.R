
# Alteryx Algorithm 

library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(zoo)


# Paths 
consolidations <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Consolidations" 
bottom_up      <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Bottoms Up Detail" 


setwd(consolidations)

consolid_act_cb <-  read.xlsx("Consolidated Actuals.xlsx",sheet = "CB")
consolid_act_ncb <- read.xlsx("Consolidated Actuals.xlsx",sheet = "NonCB")
                              




rgm_query = function(data){ 
  
rgm_data =  data %>% as_tibble %>%
    filter(grepl("RGM",Growth.Initiative)) %>% 
    select(Team, Growth.Initiative, contains(month.name)) %>%
    mutate(Team = ifelse(is.na(Team),"NA",Team)) %>% 
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>% 
    pivot_longer(!c(Team,Growth.Initiative),
                 names_to = "month",
                 values_to = "value") %>% 
  group_by(Team, Growth.Initiative, month) %>% 
  summarise(value = sum(value),.groups = "drop") %>% 
  ungroup() %>% 
  mutate(month_num = match(month, month.name)) %>% 
  mutate(quarter = quarter(month_num)) %>% 
  mutate(quarter = paste0("Q",quarter)) %>% 
  arrange(month_num) %>% select(-month, -quarter) %>% 
  pivot_wider(names_from = month_num, values_from = value) 

return(rgm_data)

}


result = rgm_query(consolid_act_ncb) %>% 
  mutate(account = "NonCB") %>% 
  bind_rows(
  rgm_query(consolid_act_cb)%>%
  mutate(account = "CB")
            )





