
# Alteryx Algorithm 

library(tidyverse)
library(lubridate)
library(zoo)


# Paths 
consolidations <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Consolidations" 
bottom_up      <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Bottoms Up Detail" 





# Operating Plan  ---------------------------------------------------------
operating_plan <- function(){ 
  
  
  # setWD
  setwd(consolidations)
  
  
  # OP resources ::::
  op_resources <- list.files() %>%
    as_tibble() %>% 
    rename(files = value) %>% 
    filter(grepl('OP.xlsx',files)) %>% 
    mutate(sheets = map(.$files, excel_sheets)) %>% 
    unnest(cols = sheets) %>% 
    filter(sheets != "Dropdowns") 
  # filter(sheets == 'NonCB')
  
  
  # Data Wrangling
  read_op <- function(file, sheet){ 
    
    data <- openxlsx::read.xlsx(file, sheet) %>% 
      as_tibble() %>%
      select(!contains("Q"))%>%
      select(!contains("FY")) %>% 
      pivot_longer(!c(Team, Growth.Initiative, Account),
                   names_to = "Name", values_to = "Value") %>% 
      janitor::clean_names() %>% 
      rename(month = name) %>% 
      mutate(month_num = match(month, month.name)) %>% 
      mutate(quarter = quarter(month_num)) %>% 
      mutate(quarter = paste0("Q",quarter)) %>% 
      mutate(team = ifelse(is.na(team),"NA",team)) %>% 
      mutate(account = ifelse(account =="HC","C&B",account)) %>% 
      
      mutate(account_l2 = case_when(str_detect(account,"Non-C&B")~"Opex",
                                    str_detect(account,"C&B")~"Opex",
                                    str_detect(account,"Sales")~"Sales",
                                    str_detect(account,"SGM")~"SGM",
                                    str_detect(account,"Capex")~"Capex",
                                    TRUE ~ as.character(account))) %>% 
      
      mutate(account_l1 = case_when(str_detect(account_l2,"Opex")~"Investment",
                                    str_detect(account_l2,"Capex")~"Investment",
                                    str_detect(account_l2,"Sales")~"Sales",
                                    str_detect(account_l2,"SGM")~"SGM",
                                    TRUE ~ as.character(account_l2))) %>% 
      
      select(growth_initiative, team, account_l1, account_l2, account, month,
             month_num, quarter, OP = value)
    
    
    
    return(data)
    
  }
  
  
  #  Iteration 
  consolidation <-function(data){
    
    map2(data$files, 
         data$sheets,
         read_op) %>% 
      map_dfr(., bind_rows)
    
  }
  
  
  # Raw Data 
  raw_data <- consolidation(op_resources)
  
  raw_data_resumen <- raw_data %>% 
    group_by(account,quarter) %>%
    summarise(volume = n(),
              account_l2 = first(account_l2),
              account_l1 = first(account_l1),
              OP = sum(OP), .groups = "drop") %>%
    pivot_wider(names_from = quarter, values_from = OP)
  
  
  print(raw_data_resumen)
  
  return(raw_data)
  
  
}





# Actuals Data   ----------------------------------------------------------
actuals_data   <- function(){ 
  
  
  # setWD
  setwd(bottom_up)  
  
  # OP resources ::::
  actuals_resources <- function(period){  
    
    
    data <- list.dirs() %>%
      as_tibble() %>% 
      filter(grepl("Actuals",value)) %>% 
      rename(file = value) %>% 
      separate(file, c("main","second","third","fourth"),sep = "([/])") %>% 
      mutate(path = paste0(second,"/", third,"/", fourth)) %>% 
      filter(!is.na(fourth)) %>%
      select(-main) %>% 
      mutate(month = substr(fourth,0,2),
             year  = substr(fourth,4,7)) %>% 
      select(-fourth) %>% 
      mutate(date = make_date(year = year,
                              month = month,
                              day = 1L)) %>% 
      filter(third == "Actuals") %>% 
      rename(team = second) %>% 
      mutate(month = as.numeric(month)) %>% 
      filter(month == max(period)) %>% 
      mutate(files = map(.$path, list.files)) %>% 
      unnest(cols = files) %>% 
      mutate(fullp = paste0(path,"/",files)) %>% 
      mutate(sheets = map(.$fullp, excel_sheets)) %>% 
      unnest(cols = sheets) %>% 
      filter(sheets != "Dropdowns") %>% 
      mutate(type = case_when(str_detect(files,"NonCB")~"NonCB",
                              str_detect(files,"CB")~"CB",
                              str_detect(files,"Capex")~"Capex",
                              str_detect(files,"SalesSGM")~"SalesSGM",
                              TRUE ~ as.character(files))) 
    
    
    data %>% group_by(type) %>% summarise(nvol = n()) %>% print()
    
    
    
    return(data)
    
  }
  
  
  # Data Wrangling
  read_actuals_ncb <- function(file, sheet){ 
    
    data <- openxlsx::read.xlsx(file, sheet)
    
    
    names(data) <- as.character(data[1,])
    
    data <- data %>% janitor::clean_names() %>% 
      mutate(sheet_name = sheet) %>% 
      mutate(file_name = file) %>% 
      slice(-1) %>% 
      as_tibble() %>% 
      select(!contains("actuals_")) %>% 
      select(!contains("na_")) %>% 
      select(!contains("inputs_r")) 
    
    
    return(data)
    
  }
  
  
  # read_actuals_cb <- function(file, sheet){
  # 
  #   data <- openxlsx::read.xlsx(file, sheet) %>%
  #     slice(-1)
  # 
  # 
  #   names(data) <- as.character(data[1,])
  # 
  #   data <- data %>% janitor::clean_names() %>%
  #     mutate(sheet_name = sheet) %>%
  #     mutate(file_name = file) %>%
  #     slice(-1) %>%
  #     as_tibble() %>%
  #     select(!contains("actuals_")) %>%
  #     select(!contains("na_")) %>%
  #     select(!contains("inputs_r"))
  # 
  # 
  #   return(data)
  # 
  # }
  
  
  
  
  
  #  Iteration 
  consolidation <-function(data){
    
    map2(data$fullp, 
         data$sheets,
         read_actuals) %>% 
      map_dfr(., bind_rows) %>% 
      
      relocate(.before = team, "sheet_name") %>% 
      relocate(.before = team, "file_name") 
    
    
    
  }
  
  
  # Raw Data 
  raw_data <- consolidation(actuals_resources(7) %>% filter(type == "NonCB"))
  
  # raw_data_resumen <- raw_data %>% 
  #   group_by(account,quarter) %>%
  #   summarise(volume = n(),
  #             account_l2 = first(account_l2),
  #             account_l1 = first(account_l1),
  #             OP = sum(OP), .groups = "drop") %>%
  #   pivot_wider(names_from = quarter, values_from = OP)
  
  
  print(raw_data_resumen)
  
  return(raw_data)
  
  
}
