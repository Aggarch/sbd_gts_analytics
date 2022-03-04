# Copy & Paste files recursively 


library(tidyverse)
library(lubridate)
library(zoo)


# Script to duplicate files located in each folder team 

#  Location
bottom_up  <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Bottoms Up Detail" 
fpa <- "C:\Users\AEG1130\Stanley Black & Decker\GTS Group FP&A - Documents" 


# Actuals Update Function
actuals_update <- function(){ 
  
  setwd(bottom_up) 
  
  data.op <- list.dirs() %>%
    as_tibble() %>% 
    filter(grepl("Forecast",value)) %>% 
    filter(grepl("F10",value)) %>% 
    separate(value, c("main","second","third","fourth"),sep = "([/])") %>% 
    mutate(path = paste0(second,"/", third,"/", fourth)) %>% 
    filter(!is.na(fourth)) %>%
    select(-main) %>%
   
    mutate(files = map(.$path, list.files)) %>% 
    unnest(cols = files) %>% 
    mutate(fullp = paste0(path,"/",files)) %>% 
    select(-second,-third ) %>%
  
    rename(monthyear = fourth) %>% 
    mutate(path2 = str_replace_all(path, "F10", "OP22")) %>% 
    mutate(fullp2 = str_replace_all(fullp, "F10", "OP22"))
    
    
  
  news = data.op %>% distinct(path2) 
  map(news$path2, dir.create)
  
  map2(data.op$fullp, data.op$fullp2, file.copy)  
  
  # delete directories 
  # map(news$path2, unlink(news$path2, recursive = T))
  
  
  return(data)
  
}
