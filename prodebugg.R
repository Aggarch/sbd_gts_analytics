
library(tidyverse)
library(lubridate)
library(zoo)


# This Script allow the debbugging of heterogenous data structures across the same
# observational unit, the most common errors in homogenization are related to hidden
# values at the top of each document, most of the times thos values are valueless 
# the exception are the dates related to fisCal used at CB docs to calculate values 



#  Location
bottom_up      <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Bottoms Up Detail" 
setwd(bottom_up) 



# List of resources 

resources <- function(period,account){ 
  
data <- list.dirs() %>%
  as_tibble() %>% 
  filter(grepl(period,value)) %>% 
  rename(file = value) %>% 
  separate(file, c("main","second","third","fourth"),sep = "([/])") %>% 
  mutate(path = paste0(second,"/", third,"/", fourth)) %>% 
  filter(!is.na(fourth)) %>%
  select(-main) %>% 
  mutate(files = map(.$path, list.files)) %>% 
  unnest(cols = files) %>% 
  mutate(fullp = paste0(path,"/",files)) %>% 
  mutate(sheets = map(.$fullp, excel_sheets)) %>% 
  unnest(cols = sheets) %>% 
  filter(!grepl("Drop", sheets)) %>% 
  mutate(type = case_when(str_detect(files,"NonCB")~"NonCB",
                          str_detect(files,"CB")~"CB",
                          str_detect(files,"Capex")~"Capex",
                          str_detect(files,"SalesSGM")~"SalesSGM",
                          TRUE ~ as.character(files))) %>% 
filter(grepl(account, sheets))

return(data)

}



# specific list of resources 
object = resources("8", "NonCB")



# wrangler function original 
read_data <- function(file, sheet){ 
  
  data <- openxlsx::read.xlsx(file, sheet) 
  
  
  names(data) <- as.character(data[1,])
  
  data <- data %>% janitor::clean_names() %>% 
    mutate(sheet_name = sheet) %>% 
    mutate(file_name = file) %>% 
    slice(-1) %>% 
    as_tibble() %>% 
    select(!contains("q")) %>% 
    select(!contains("fy")) %>% 
    select(!contains("na")) %>% 
    select(!contains("forecast_region"))
  
  
  
  
    # select(team, growth_initiative, spend_category, spend_description, 
    #        october, november, december, q4) %>% 
    # filter(!is.na(growth_initiative)) %>% 
    # mutate_at(c("october", "november", "december","q4"), as.numeric)  
    # 
  
  
  return(data)
  
  # CB data colnames exists in second row because blank column dosnt count 
  # this additional column exist to set up the salary formula correctly. 
  # the salary formula most be re-engineered since it's unnecesary complex. 
  
  # Non-CB  Capex files colnames exists in fist row, SGMSales its a special 
  # example, where each sheet contains 3 or 4 different small tables. 
  
}






# detail seeker addition to wrangler 
colnames_reader = function (file, sheet){read_data(file, sheet) %>% 
    colnames()
  }



# evaluation 
datata = object %>%
  mutate(struct = map2(.$fullp, .$sheets, colnames_reader)) %>%
  unnest(cols = c(struct)) %>% 
  group_by(fullp,files, sheets) %>% 
  summarise(struct = str_flatten(struct, 
                                 collapse = ","), .groups = "drop") %>% 
  ungroup() %>% 
  filter(!grepl("Drop",sheets))   



# diagnostics 
dataproblem = datata %>% mutate(test = str_detect(struct, "team"))
dataproblem = datata %>% filter(!grepl("team", struct))






