

library(tidyverse)
library(lubridate)
library(zoo)



#  Location
bottom_up  <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Bottoms Up Detail" 



# Actuals Update Function
actuals_update <- function(){ 
  
  setwd(bottom_up) 

  data <- list.dirs() %>%
    as_tibble() %>% 
    filter(grepl("Actuals",value)) %>% 
    separate(value, c("main","second","third","fourth"),sep = "([/])") %>% 
    mutate(path = paste0(second,"/", third,"/", fourth)) %>% 
    filter(!is.na(fourth)) %>%
    select(-main) %>%
    mutate(month = substr(fourth,0,2)) %>%
    mutate(year = substr(fourth,4,7)) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(date = make_date(year = year,
                            month = month,
                            day =1L)) %>% 
    group_by(second, third) %>% 
    filter(date == max(date)) %>% 
    ungroup() %>% 
    mutate(files = map(.$path, list.files)) %>% 
    unnest(cols = files) %>% 
    mutate(fullp = paste0(path,"/",files)) %>% 
    select(-second,-third )%>%
    mutate(date2  = date %m+% months(1)) %>% 
    mutate(month2 = month(date2),
           year2  = year (date2)) %>% 
    rename(monthyear = fourth) %>% 
    mutate(monthyear2 = as.yearmon(date2)) %>% 
    mutate(monthyear2 =  format(monthyear2,"%m-%Y"))%>%
    mutate(path2  = str_replace_all(path,monthyear,monthyear2))%>% 
    mutate(sufix  = paste0(month,substr(year,3,4))) %>% 
    mutate(sufix2 = paste0(month2,substr(year2,3,4)))%>%
    mutate(files2 = str_replace_all(files, sufix, sufix2)) %>% 
    mutate(fullp2 = str_replace_all(fullp, monthyear, monthyear2)) %>% 
    mutate(fullp2 = str_replace_all(fullp2, sufix, sufix2)) %>% 
    select(everything(), contains("2")) %>% 
    ungroup() %>% 
    select(contains(c("date", "path","fullp")))
  
  news = data %>% distinct(path2) 
  map(news$path2, dir.create)
  
  map2(data$fullp, data$fullp2, file.copy)  
  
  # delete directories 
  # map(news$path2, unlink(news$path2, recursive = T))
  
  
  return(data)
  
}
