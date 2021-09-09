
# Alteryx Algorithm 

library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)


# Paths 
consolidations <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Consolidations" 
bottom_up      <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Bottoms Up Detail" 


# opendir <- function(directory = getwd()){
#   system(sprintf('open %s', shQuote(directory)))
# }


# OPERATING PLAN  ---------------------------------------------------------

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
      
      mutate(ftes_started = ifelse(account == "HC",value,0)) %>% 
      mutate(value = ifelse(account == "HC",0,value)) %>%
      
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
      rename(OP = value) %>% 
      
      select(growth_initiative, team, account_l1, account_l2, account, month,
             month_num, quarter, OP, ftes_started)
    
    
    
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
  raw_data <- consolidation(op_resources) %>% 
    arrange(growth_initiative, team,
            account_l1,account_l2)
  
  
  raw_data_resumen <- raw_data %>% 
    group_by(account,quarter) %>%
    summarise(volume = n(),
              account_l2 = first(account_l2),
              account_l1 = first(account_l1),
              OP = sum(OP), .groups = "drop") %>%
    pivot_wider(names_from = quarter, values_from = OP) 
  
  
  print(raw_data_resumen)
  
  return(raw_data)
  
  
}                                      # ✔✔✔
# OP, no_input; consolidated, filterable by account, team, initiative, etc




# ACTUALS         ---------------------------------------------------------

# Actuals resources of specific period ::::
  actuals_resources         <- function(period, target){  
  
  # setWD
  setwd(bottom_up) 
  
  
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
  
  data_target <- data %>% 
    filter(type == target) 
    # mutate_if(is.character, str_trim)
  
  
  data %>% group_by(type) %>% summarise(nvol = n()) %>% print()
  
  
  print(target)
  
  return(data_target)
  
}             # ✔✔✔
# Actuals Files, inputs;    (p = month.#), target; {CB, NonCB, Capex, Sales/SGM}


# Non C&B Query R
  noncb_actuals_data        <- function(period){ 
  
  
  # Data Wrangling for NonCB
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
  

  
  #  Iteration 
  consolidation <-function(data){
    
    map2(data$fullp, 
         data$sheets,
         read_actuals_ncb) %>% 
      map_dfr(., bind_rows) 
      
   
    }
  
  
  ncb_act_raw_data <- consolidation(actuals_resources(period, target = "NonCB")) %>% 
    
    relocate(.before = team, "sheet_name") %>% 
    relocate(.before = team, "file_name") %>% 
    mutate(team = ifelse(is.na(team),"NA",team)) %>% 
    

    filter(!is.na(spend_category)|
           !is.na(spend_description)|
           !is.na(cost_center_local)|
           !is.na(vendor)|
           !is.na(purchase_order_number)|
           !is.na(erp_system)|
           !is.na(cost_center_global)|
           !is.na(contact)) %>% 
    
    separate(file_name, c("file_name1","file_name2",
                          "file_name3","file_name4"),sep = "([/])") %>% 
    select(-erp_system,-file_name1, -sheet_name,
           -contact, -file_name4, ) %>% 
    rename(scenario = file_name2,
           date = file_name3) %>% 
    select(-date) %>% 
    mutate(account = "NonCB") %>% 
    pivot_longer(!c(team, growth_initiative, spend_category,
                    spend_description,cost_center_local,vendor,
                    cost_center_global,forecast_region,scenario,
                    account,purchase_order_number),
                    names_to = "month", values_to = "value") %>% 
    filter(!grepl("q|fy",month)) %>% 
    mutate(month = str_to_title(month)) %>% 
    mutate(month_num = match(month, month.name)) %>% 
    mutate(quarter = quarter(month_num)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>%
    mutate(value = as.numeric(value)) %>% 
    pivot_wider(names_from = "scenario", 
                values_from = value,
                values_fn = sum) %>% 
    mutate(account_l1 = "investment",
           account_l2 = "opex") %>% 
    group_by(team,growth_initiative,forecast_region,
             account_l1,account_l2,account,
             month, month_num, quarter) %>% 
    summarise(Actuals = sum(Actuals),.groups = "drop")%>% 
    arrange(growth_initiative, team,
            account_l1,account_l2)
  
    
  return(ncb_act_raw_data)
  
}                     # ✔✔✔
# NonCB Actuals, inputs;    (p = month.#, as.number())
  
  
# C&B Query 
  cb_actuals_data           <- function(period){ 
  
  
# Data Wrangling for CB
  read_actuals_cb <- function(file, sheet){
    
    data <- openxlsx::read.xlsx(file, sheet) %>%
      slice(-1)
    
    
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
  
  
  
#  Iteration 
  consolidation <-function(data){
    
    map2(data$fullp, 
         data$sheets,
         read_actuals_cb) %>% 
      map_dfr(., bind_rows) %>% 
      
      relocate(.before = team, "sheet_name") %>% 
      relocate(.before = team, "file_name") 
    
    
    
  }
  
  
  cb_act_raw_data <- consolidation(actuals_resources(period, target = "CB")) %>% 
    filter(!is.na(brassring_job_title)|
           !is.na(brassring_req_number)|
           !is.na(employee_name)|
           !is.na(employee_id)|
           !is.na(country)|
           !is.na(budgeted_salary_usd)|
           !is.na(actual_salary_usd)|
           !is.na(budgeted_start_month)|
           !is.na(actual_start_date)|
           !is.na(hiring_status)|
           !is.na(technical_skillset_gained_with_role)|
           !is.na(employee_level)|
           !is.na(backfill_name_if_applicable)|
           !is.na(cost_center_global)|
           !is.na(hiring_manager)|
           !is.na(variable_comp_if_applicable)) %>% 
    mutate(fte_estimated = 1) %>% 
    separate(file_name, c("file_name1","file_name2",
                          "file_name3","file_name4"),sep = "([/])") %>% 
    select(!starts_with("x"),
           -sheet_name, -fy_2022,
           -file_name1, -file_name3,
           -file_name4,-sub_team) %>% 
    rename(scenario = file_name2) %>% 
    mutate(account = "CB") %>% 
    pivot_longer(!c(team, growth_initiative, brassring_job_title,
                    brassring_req_number,employee_name,employee_id,
                    country,budgeted_salary_usd,actual_salary_usd,
                    bene_taxes_percent_formula,budgeted_start_month,
                    actual_start_date,hiring_status,technical_skillset_gained_with_role,
                    employee_level,backfill_name_if_applicable,cost_center_global,
                    hiring_manager, variable_comp_if_applicable,forecast_region,scenario,
                    fte_estimated),
                 names_to = "month", values_to = "value") %>%
    filter(!grepl("q|fy",month)) %>%
    mutate(month = str_to_title(month)) %>%
    mutate(month_num = match(month, month.name)) %>%
    mutate(quarter = quarter(month_num)) %>%
    mutate(quarter = paste0("Q",quarter)) %>% 
    filter(!grepl("QNA",quarter)) %>% 
    rename(actuals = value) %>% 
    mutate(ftes_hired = ifelse( hiring_status %in% c("Started - Internal Transfer", 
                                  "Started - External", 
                                  "Offer Accepted - Internal",
                                  "Offer Accepted - External"),1,0)) %>% 
    
  mutate(ftes_started = ifelse(actuals > 0,1,0)) %>% 
  mutate(account_l2 = "opex",
         account_l1 = "investment",
         account = "C&B") %>% 
  group_by(growth_initiative, team, forecast_region,
           country, hiring_status, account_l1, account_l2,
           account, month, month_num, quarter) %>% 
  mutate(actuals = as.double(actuals)) %>% 
  summarise(actuals = sum(actuals),
            ftes_hired = sum(ftes_hired),
            ftes_started = sum(ftes_started),
            fte_estimated = sum(fte_estimated),.groups = "drop") %>% 
    arrange(growth_initiative, team,
            account_l1,account_l2)

  
  return(cb_act_raw_data)
  
}                     # ✔✔✔
# CB Actuals, inputs;       (p = month.#, as.number())
  
  
# Capex Query 
  capex_actuals_data        <- function(period){ 
    
    
    # Data Wrangling for CB
    read_actuals_capex <- function(file, sheet){
      
      data <- openxlsx::read.xlsx(file, sheet) 

      
      names(data) <- as.character(data[1,])
      
      data <- data %>% janitor::clean_names() %>%
        mutate(sheet_name = sheet) %>%
        mutate(file_name = file) %>%
        slice(-1) %>%
        as_tibble() 
      
      return(data)
      
    }
    
    
    
    #  Iteration 
    consolidation <-function(data){
      
      map2(data$fullp, 
           data$sheets,
           read_actuals_capex) %>% 
        map_dfr(., bind_rows) %>% 
        
        relocate(.before = team, "sheet_name") %>% 
        relocate(.before = team, "file_name") 
      
      
      
    }
    
    
    capex_act_raw_data <- consolidation(actuals_resources(period, target = "Capex")) %>% 
      filter(!is.na(far_number)|
             !is.na(far_description)|
             !is.na(internal_order_number)|
             !is.na(internal_order_description)|
             !is.na(erp_system)) %>% 
      separate(file_name, c("file_name1","file_name2",
                            "file_name3","file_name4"),sep = "([/])") %>% 
      mutate(team = ifelse(is.na(team),"NA",team)) %>% 
      
      select(!starts_with("actuals_")) %>% 
      select(!starts_with("q")) %>% 
      select(!starts_with("fy")) %>% 
      
      select(-file_name1,-file_name3,-file_name4,
             -sheet_name,-erp_system) %>% 
      
      mutate(account = "Capex") %>% 
      rename(scenario = file_name2) %>% 
      pivot_longer(!c(team, growth_initiative,
                      far_number, far_description,
                      internal_order_number, internal_order_description,
                      scenario,account),
                   names_to = "month", values_to = "value") %>% 
      mutate(month = str_to_title(month)) %>% 
      mutate(month_num = match(month, month.name)) %>%
      mutate(quarter = quarter(month_num)) %>%
      mutate(quarter = paste0("Q",quarter)) %>% 
      filter(!grepl("QNA",quarter)) %>% 
      mutate(account_l1 = "Investment", 
             account_l2 = "Capex") %>% 
      group_by(team, growth_initiative,
               # far_number, far_description,
               # internal_order_number, internal_order_description,
               account,account_l1,account_l2,
               month, month_num, quarter) %>% 
      mutate(value = as.double(value)) %>% 
      summarise(Actuals = sum(value),.groups = "drop") %>% 
      ungroup() %>% 
      arrange(growth_initiative, team,
              account_l1,account_l2,month_num)
     
      
    
    return(capex_act_raw_data)
    
  }                     # ✔✔✔
# Capex Actuals, inputs;    (p = month.#, as.number())
  
  
# Sales/SGM Query 
  sales_sgm_actuals_data    <- function(period){ 
    
    
    # Data Wrangling for CB
    read_actuals_salessgm <- function(file, sheet){
      
      data <- openxlsx::read.xlsx(file, sheet) 
      
      
      names(data) <- as.character(data[1,])
      
      data <- data %>% janitor::clean_names() %>%
        mutate(sheet_name = sheet) %>%
        mutate(file_name = file) %>%
        slice(-1) %>%
        as_tibble()  %>% 
        
        filter(account != "SGM %",
               !is.na(account)) %>% 
        mutate(region = ifelse(is.na(region),"NA",region)) %>% 
        group_by(growth_initiative) %>% 
        slice(1:2) %>% 
        ungroup()
      
      return(data)
      
    }
    
    
    
    #  Iteration 
    consolidation <-function(data){
      
      map2(data$fullp, 
           data$sheets,
           read_actuals_capex) %>% 
        map_dfr(., bind_rows)
        
       
    }
    
    
    salessgm_act_raw_data <- consolidation(actuals_resources(period, target = "SalesSGM")) %>%
      filter(account != "SGM %",
            !is.na(account),
            !is.na(growth_initiative),
             account %in% c("SGM","Sales"),
            !grepl("Init",growth_initiative)) %>%
     
      mutate(region = ifelse(is.na(region),"NA",region)) %>% 
      group_by(region, growth_initiative) %>% 
      slice(1:2) %>% 
      ungroup() %>% 
      select(!starts_with("x")) %>%
      select(!starts_with("q")) %>%
      select(!starts_with("fy"))%>% 
      select(-sheet_name, -file_name) %>%
      rename(team = region) %>% 
      pivot_longer(!c(team, growth_initiative,account),
                   names_to = "month", values_to = "value") %>% 
      mutate(month = str_to_title(month)) %>% 
      mutate(month_num = match(month, month.name)) %>%
      mutate(quarter = quarter(month_num)) %>%
      mutate(quarter = paste0("Q",quarter)) %>% 
      relocate(.before = month, value) %>% 
      rename(Actuals = value) %>% 
      mutate(account_l1 = account,
             account_l2 = account) %>% 
      arrange(growth_initiative, team,
              account_l1,account_l2,account,
              month_num,Actuals) %>% 
      mutate(Actuals = as.double(Actuals))
    
 
    
    
    return(salessgm_act_raw_data)
    
  }                     # ✔✔✔
# Sales/SGM Actuals, inputs;(p = month.#, as.number())
  
  

  

# FCAST -------------------------------------------------------------------

# Forecast resources of specific period ::::
  forecast_resources         <- function(period, target){  
    
    # setWD
    setwd(bottom_up) 
    
    
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
      filter(!grepl("Summary|Dropdowns|Sheet", sheets)) %>% 
      mutate(type = case_when(str_detect(sheets,"NonCB")~"NonCB",
                              str_detect(sheets,"CB")~"CB",
                              str_detect(sheets,"Capex")~"Capex",
                              str_detect(sheets,"SalesSGM")~"SalesSGM",
                              TRUE ~ as.character(sheets)))
    
  
    
    data %>% group_by(type) %>% summarise(nvol = n()) %>% print()
    
    data_target <- data %>% 
      filter(type == target) 
    
    print(target)
    
    return(data_target)
    
  }           # ✔✔✔
# Actuals Files, inputs;     (p = month.#), target; {CB, NonCB, Capex, Sales/SGM}

  
# Non C&B Query R
  noncb_forecast_data        <- function(period){ 
    
    
    # Data Wrangling for NonCB
    read_forecast_ncb <- function(file, sheet){ 
      
      data <- openxlsx::read.xlsx(file, sheet) 
      
      
      names(data) <- as.character(data[1,])
      
      data <- data %>% janitor::clean_names() %>% 
        mutate(sheet_name = sheet) %>% 
        mutate(file_name = file) %>% 
        slice(-1) %>% 
        as_tibble() 
        
       
      
      
      return(data)
      
    }
    
    
    
    #  Iteration 
    consolidation <-function(data){
      
      map2(data$fullp, 
           data$sheets,
           read_forecast_ncb) %>% 
        map_dfr(., bind_rows) 
      
      
    }
    
    
    ncb_fct_raw_data <- consolidation(forecast_resources(period, target = "NonCB")) %>% 
      
      filter(!is.na(spend_category)|
             !is.na(spend_description)|
             !is.na(cost_center_local)|
             !is.na(vendor)|
             !is.na(purchase_order_number)|
             !is.na(erp_system)|
             !is.na(cost_center_global)|
             !is.na(contact)) %>% 
       separate(file_name, c("file_name1","file_name2",
                             "file_name3","file_name4"),sep = "([/])") %>% 
       select(-erp_system,-file_name1, -sheet_name,
              -contact, -file_name4, ) %>% 
       rename(scenario = file_name2,
              date = file_name3) %>% 
       select(-date) %>% 
      pivot_longer(!c(team, growth_initiative, spend_category,
                      spend_description,cost_center_local,vendor,
                      cost_center_global,scenario,
                      purchase_order_number),
                   names_to = "month", values_to = "value") %>%
      filter(!grepl("q|fy",month)) %>% 
      mutate(month = str_to_title(month)) %>%
      mutate(month_num = match(month, month.name)) %>%
      mutate(quarter = quarter(month_num)) %>%
      mutate(quarter = paste0("Q",quarter)) %>%
      mutate(account_l1 = "investment",
             account_l2 = "opex",
             account = "NonCB") %>% 
      mutate(value = as.double(value)) %>%
      replace_na(list(value = 0)) %>% 
      group_by(team,growth_initiative,
               account_l1,account_l2,account,
               month, month_num, quarter) %>%
      summarise(F03 = sum(value),.groups = "drop") %>%
      replace_na(list(F03 = 0)) %>%
      filter(!is.na(growth_initiative)) %>%
      mutate(team = ifelse(is.na(team),"NA",team))

  
    return(ncb_fct_raw_data)
    
  }                   # ✔✔✔
# NonCB Forecast, inputs;    (p = month.#, as.number())
  
  
# Non C&B Query R
  cb_forecast_data           <- function(period){ 
    
    
    # Data Wrangling for NonCB
    read_forecast_cb <- function(file, sheet){ 
      
      data <- openxlsx::read.xlsx(file, sheet) 
      
      
      names(data) <- as.character(data[2,])
      
      data <- data %>% janitor::clean_names() %>% 
        mutate(sheet_name = sheet) %>% 
        mutate(file_name = file) %>% 
        slice(-2) %>% 
        as_tibble() %>% 
        filter(!grepl("Team|Inputs", team))
      
      
      
      
      return(data)
      
    }
    
    
    
    #  Iteration 
    consolidation <-function(data){
      
      map2(data$fullp, 
           data$sheets,
           read_forecast_ncb) %>% 
        map_dfr(., bind_rows) 
      
      
    }
    
    
    cb_fct_raw_data <- consolidation(forecast_resources(period, target = "CB")) %>% 
      filter(!is.na(brassring_job_title)|
             !is.na(brassring_req_number)|
             !is.na(employee_name)|
             !is.na(employee_id)|
             !is.na(country)|
             !is.na(budgeted_salary_usd)|
             !is.na(forecasted_salary_usd)|
             !is.na(budgeted_start_month)|
             !is.na(forecasted_start_date)|
             !is.na(hiring_status)|
             !is.na(technical_skillset_gained_with_role)|
             !is.na(employee_level)|
             !is.na(backfill_name_if_applicable)|
             !is.na(cost_center_global)|
             !is.na(hiring_manager)|
             !is.na(variable_comp_if_applicable)) %>% 
      separate(file_name, c("file_name1","file_name2",
                            "file_name3","file_name4"),sep = "([/])") %>% 
      select(-file_name1, -sheet_name,
             -file_name4, ) %>% 
      rename(scenario = file_name2,
             date = file_name3) %>% 
      select(-date)
      # pivot_longer(!c(team, growth_initiative,
      #                 cost_center_local,vendor,
      #                 cost_center_global,scenario,
      #                 purchase_order_number),
      #              names_to = "month", values_to = "value") %>%
      # filter(!grepl("q|fy",month)) %>% 
      # mutate(month = str_to_title(month)) %>%
      # mutate(month_num = match(month, month.name)) %>%
      # mutate(quarter = quarter(month_num)) %>%
      # mutate(quarter = paste0("Q",quarter)) %>%
      # mutate(account_l1 = "investment",
      #        account_l2 = "opex",
      #        account = "NonCB") %>% 
      # mutate(value = as.double(value)) %>%
      # replace_na(list(value = 0)) %>% 
      # group_by(team,growth_initiative,
      #          account_l1,account_l2,account,
      #          month, month_num, quarter) %>%
      # summarise(F03 = sum(value),.groups = "drop") %>%
      # replace_na(list(F03 = 0)) %>%
      # filter(!is.na(growth_initiative)) %>%
      # mutate(team = ifelse(is.na(team),"NA",team))
    
    
    return(cb_fct_raw_data)
    
  }                   # ✔✔✔
# CB Forecast, inputs;       (p = month.#, as.number())
  

  
