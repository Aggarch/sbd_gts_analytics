
#  head.Counter

library(tidyverse)
library(lubridate)
library()


hc_path = "S:/North_America/Towson-TOW/FINANCE/WWPTAFIN/COE/SG&A - General Information/Headcount Reports/2021/Corporate Actives List"

setwd(hc_path)


#  resources
resources <- list.files() %>% 
  as_tibble() %>%
  filter(!grepl("~",value)) %>% 
  rename(files = value) %>% 
  mutate(sheets = map(.$files, excel_sheets)) %>% 
  unnest(col = sheets) %>% 
  filter(sheets == "Actives") %>% 
  mutate(period = substr(files,14,20))
# 3""


#  reading
read_actuals_hc <- function(files, sheets, period){ 
  
  data <- openxlsx::read.xlsx(files, sheets) 
  
  
  names(data) <- as.character(data[1,])
  
  data <- data %>% janitor::clean_names() %>% 
    slice(-1) %>% 
    as_tibble() %>% 
    select(name, work_location_name, job_code_description, 
           employee_status, business_unit_desc, gl_cost_center) %>% 
    
    filter(gl_cost_center %in% c("9401500226","9401500333","9401500230")) %>% 
    
    mutate(cost_center = case_when(gl_cost_center == "9401500230"~"TO",
                                   gl_cost_center == "9401500226"~"DA",
                                   gl_cost_center == "9401500333"~"IoT")) %>% 
    mutate(period = period)
    
  
  
  
  return(data)
  
}



#  Iteration 
consolidation <-function(data){
  
start_time <- Sys.time()
  
hc_data <- data %>% 
    pmap(read_actuals_hc) %>% 
    map_dfr(., bind_rows) 
    
end_time <- Sys.time()
end_time - start_time




return(hc_data)
  
  }



consolid_hc <- consolidation(resources)

consolid_hc_grouped <- consolid_hc %>% 
  group_by(period,cost_center) %>%
  summarise(n = n(), .groups = "drop") %>% 
  mutate_if(is.character, str_trim) %>% 
  pivot_wider(names_from = period, values_from = n)






