
#Hoppe Functions 


# DA ----------------------------------------------------------------------



# ACTUALS 
dp_close_actuals <- function(tw){
  
  # cost_centers_data.  
  ccdata <- openxlsx::read.xlsx("datacc_da.xlsx", "hist_raw") %>% 
    as_tibble() %>%  janitor::clean_names()
  
  samort <- openxlsx::read.xlsx("datacc_da.xlsx", "samort") %>% 
    as_tibble() %>%  janitor::clean_names() %>% 
    mutate(period = as.Date(period, origin = "1899-12-30")) %>% 
    mutate(value = abs(value)) %>%
    mutate(period = as.yearmon(period)) %>% 
    select(-cost_center) %>% 
    mutate(s_description = case_when(str_detect(description,"Tool Connect")~"Tool Connect",
                                     str_detect(description,"Connected Product Integration")~"Connected Product Integration",
                                     str_detect(description,"Dewalt Connector")~"Dewalt Connector",
                                     str_detect(description,"HTAS Enhancements")~"HTAS Enhancements",
                                     str_detect(description,"LDMs")~"Laser Distance Measures",
                                     str_detect(description,"PTE Enhancements")~"PTE Enhancements",
                                     str_detect(description,"Rotary")~"Rotary Laser",
                                     str_detect(description,"All Purpose Light")~"All Purpose Light",
                                     str_detect(description,"Universal Serial")~"Universal Serial Number",
                                     TRUE ~ as.character("Other projects"))) %>% 
    mutate(category = "Soft.Amort") %>% 
    unite("category", c(category, s_description), sep = "/") %>% 
    relocate(.before = period, value) %>% 
    group_by(category, period) %>%
    summarise(value = sum(value),.groups = "drop") %>% 
    filter(period <= tw) 
  
  
  # mutate(period = lubridate::month(period))%>%
  # pivot_wider(names_from = period, values_from = value, values_fn = sum) 
  
  
  # fix_assets
  clearing <- ccdata %>% 
    filter(grepl("C.I.P.", name_of_offsetting_account)) %>%
    select(cost_element, cost_element_name, period,val_in_rep_cur) %>%
    rename(clearing_account = val_in_rep_cur)
  
  
  # tidy_actuals.
  actuals <- ccdata %>%
    filter(!cost_element %in% c("1817400","1919350","5672215")) %>%
    mutate(cost_element_name = ifelse(cost_element_name == "UTILITY TELEPHONE" & val_in_rep_cur > 5000,
                                      name_of_offsetting_account, cost_element_name)) %>% 
    group_by(cost_element, cost_element_name, period) %>%
    summarise(val_in_rep_cur = sum(val_in_rep_cur), .groups = "drop") %>%
    left_join(clearing, by = c("cost_element", "cost_element_name", "period")) %>%
    ungroup() %>%
    replace_na(list(val_in_rep_cur = 0, clearing_account = 0)) %>%
    mutate(period = as.numeric(period)) %>%
    mutate(date = make_date(year = year(today()),
                            month = period, day = 1L)) %>%
    mutate(period = as.yearmon(date)) %>%
    select(-date) %>%
    rename(actual = val_in_rep_cur) %>%
    mutate(gross = actual - clearing_account) %>%
    mutate(category =case_when(str_detect(cost_element_name,"EMP BEN")~"C&B",
                               str_detect(cost_element_name,"PR TAXES")~"C&B",
                               str_detect(cost_element_name,"WAGE")~"C&B",
                               # str_detect(cost_element_name,"DEMO")~"Demo Tools - FG Stock",
                               str_detect(cost_element_name,"DEMO")~"Demo Tools",
                               str_detect(cost_element_name,"OS FEE LABOR")~"Professional Fees - Globant LLC",
                               str_detect(cost_element_name,"OS FEE GENERAL")~"Services Fees - Cambridge Sharpe",
                               # str_detect(cost_element_name,"PROMO SPECIAL P")~"Promo Services",
                               str_detect(cost_element_name,"OS FEE RECRUIT")~"Recruiting",
                               str_detect(cost_element_name,"RENT BUILD")~"Rent",
                               str_detect(cost_element_name,"AMORTIZ SOFTW")~"Software Amortization",
                               str_detect(cost_element_name,"MATL PROTO")~"Supplies",
                               str_detect(cost_element_name,"OS FEE LEGAL GEN")~"Supplies",
                               str_detect(cost_element_name,"OTH EXP MISC")~"Supplies",
                               str_detect(cost_element_name,"SUPPLIES")~"Supplies",
                               str_detect(cost_element_name,"T&E")~"T&E",
                               str_detect(cost_element_name,"UTILITY TELEP")~"Telephone",
                               str_detect(cost_element_name,"EMP DEV SHOW EXHIBIT")~"Supplies",
                               str_detect(cost_element_name,"HAMILTON MANU")~"Gyro Development - Didio Design",
                               TRUE ~ as.character("Others"))) %>%
    relocate(.before = cost_element, category )
  
  
  
  # summarized actuals by category.
  resumen_actuals <-  actuals %>%
    group_by(category, period) %>%
    summarise(gross = sum(gross),.groups="drop") %>% 
    rename(value = gross) %>% 
    filter(!grepl("Software Amort",category)) %>% 
    bind_rows(samort)
  
  
  
  # summarized capitalization.
  resumen_capitalized <-  actuals %>%
    group_by(category, period) %>%
    mutate(category = "Capitalized") %>% 
    summarise(clearing_account = sum(clearing_account),.groups="drop") %>% 
    rename(value = clearing_account)
  
  
  detailed <- resumen_actuals %>% 
    bind_rows(resumen_capitalized) %>% 
    mutate(period = as.Date(period, 
                            origin = "1899-12-30")) %>% 
    mutate(period = as.yearmon(period)) %>% 
    mutate(quarter = quarter(period)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>% 
    mutate(type = "actuals") %>% 
    filter(period <= all_of(tw)) 
  
  
  monthly <- detailed %>%
    select(-quarter) %>%
    pivot_wider(names_from = period,
                values_from = value, 
                values_fn = sum) %>% 
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.))
  
  
  quarterly <- detailed %>%
    select(-period, -type) %>%
    pivot_wider(names_from = quarter,
                values_from = value, 
                values_fn = sum) %>% 
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.))
  
  overview = monthly %>% left_join(quarterly, by = "category") %>% 
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.))
  
  
  
  
  return(list(raw_cc = ccdata,
              fixed_assets = clearing,
              actuals = actuals,
              detailed = detailed,
              # monthly = monthly, 
              # quarterly = quarterly, 
              overview = overview))
  
  
}

# OPlan
dp_op_plan       <- function(tw){ 
  
  # detailed
  
  detailed <- openxlsx::read.xlsx("datacc_da.xlsx", "OP") %>% 
    as_tibble() %>%  janitor::clean_names() %>% 
    mutate(period = as.Date(period,
                            origin = "1899-12-30")) %>%
    mutate(period = as.yearmon(period)) %>% 
    mutate(quarter = quarter(period)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>% 
    mutate(type = "op") %>% 
    filter(period <= all_of(tw))
  
  
  
  
  # monthly
  monthly <- detailed %>% 
    select(-quarter) %>% 
    pivot_wider(names_from = period, values_from = value)
  
  
  # quarterly 
  quarterly <- detailed %>% 
    select(-period, -type) %>% 
    pivot_wider(names_from = quarter,
                values_from = value,
                values_fn = sum) 
  # mutate(full_year = rowSums(select(., -category)))
  
  
  overview = monthly %>% left_join(quarterly, by = "category")
  
  
  return(list(
    detailed = detailed,
    overview = overview))
  
  
}

# Forecast
dp_fcast         <- function(tw){ 
  
  # detailed
  detailed <- openxlsx::read.xlsx("datacc_da.xlsx", "F7") %>% 
    as_tibble() %>%  janitor::clean_names() %>% 
    mutate(period = as.Date(period,
                            origin = "1899-12-30")) %>%
    mutate(period = as.yearmon(period)) %>% 
    mutate(quarter = quarter(period)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>% 
    mutate(type = "fcast") %>%
    filter(period <= all_of(tw))
  
  
  
  # monthly
  monthly <- detailed %>% 
    select(-quarter) %>% 
    pivot_wider(names_from = period, values_from = value)
  
  
  # quarterly 
  quarterly <- detailed %>% 
    select(-period, -type) %>% 
    pivot_wider(names_from = quarter,
                values_from = value,
                values_fn = sum) 
  # mutate(full_year = rowSums(select(., -category)))
  
  
  
  overview = monthly %>% left_join(quarterly, by = "category") 
  
  
  return(list(
    detailed = detailed,
    overview = overview))
  
  
}




# IoT ---------------------------------------------------------------------

# ACTUALS 
iot_close_actuals     <- function(tw){
  
  # cost_centers_data.  
  ccdata <- openxlsx::read.xlsx("datacc_iot.xlsx", "hist_raw") %>% 
    as_tibble() %>%  janitor::clean_names() %>% 
    filter(cost_element != 5363840)
  
  
  
  # tidy_actuals.
  actuals <- ccdata %>%
    mutate(cost_element_name = ifelse(cost_element_name == "UTILITY TELEPHONE" & val_in_rep_cur > 5000,
                                      name_of_offsetting_account, cost_element_name)) %>%
    
    mutate(cost_element_name = ifelse(cost_element_name == "UTILITY TELEPHONE" & val_in_rep_cur < -5000,
                                      name_of_offsetting_account, cost_element_name)) %>%
    
    group_by(cost_element, cost_element_name, period) %>%
    summarise(val_in_rep_cur = sum(val_in_rep_cur), .groups = "drop") %>%
    ungroup() %>%
    replace_na(list(val_in_rep_cur = 0)) %>%
    mutate(period = as.numeric(period)) %>%
    mutate(date = make_date(year = year(today()),
                            month = period, day = 1L)) %>%
    mutate(period = as.yearmon(date)) %>%
    select(-date) %>%
    rename(actual = val_in_rep_cur) %>%
    mutate(category =case_when(str_detect(cost_element_name,"EMP BEN")~"C&B",
                               str_detect(cost_element_name,"PR TAXE")~"C&B",
                               str_detect(cost_element_name,"WAGE")~"C&B",
                               # str_detect(cost_element_name,"DEMO")~"Demo Tools - FG Stock",
                               str_detect(cost_element_name,"DEMO")~"Demo Tools",
                               str_detect(cost_element_name,"OS FEE RECRUIT")~"Recruiting",
                               str_detect(cost_element_name,"RENT BUILD")~"Rent",
                               str_detect(cost_element_name,"AMORTIZ SOFTW")~"Software Amortization",
                               str_detect(cost_element_name,"MATL PROTO")~"Cloud Usage and Support - Amazon Web Services",
                               str_detect(cost_element_name,"OS FEE LEGAL GEN")~"Supplies",
                               str_detect(cost_element_name,"SUPPLIES")~"Supplies",
                               str_detect(cost_element_name,"T&E")~"T&E",
                               str_detect(cost_element_name,"UTILITY TELEP")~"Telephone",
                               str_detect(cost_element_name,"OS FEE GENERAL")~"IoT Cloud Service - Software AG",
                               str_detect(cost_element_name,"ZIGATTA")~"ConsumerApp - Zigatta",
                               
                               
                               # ON RAW - KSB1 inputted --- check uot reference file + Accruels Sharepoint :  
                               str_detect(cost_element_name,"Software Engineering - Infotech Prism")~"Software Engineering - Infotech Prism",
                               str_detect(cost_element_name,"IoT Cloud Service - Software AG")~"IoT Cloud Service - Software AG",
                               str_detect(cost_element_name,"Cloud Usage and Support - AWS")~"Cloud Usage and Support - Amazon Web Services",
                               str_detect(cost_element_name,"ConsumerApp - Zigatta")~"ConsumerApp - Zigatta",
                               
                               
                               
                               TRUE ~ as.character("Others"))) %>%
    relocate(.before = cost_element, category )
  
  
  
  # summarized actuals by category.
  resumen_actuals <-  actuals %>%
    group_by(category, period) %>%
    summarise(actual = sum(actual),.groups="drop") %>% 
    rename(value = actual)
  
  
  detailed <- resumen_actuals %>% 
    mutate(period = as.Date(period, 
                            origin = "1899-12-30")) %>% 
    mutate(period = as.yearmon(period)) %>% 
    mutate(quarter = quarter(period)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>% 
    mutate(type = "actuals") %>% 
    filter(period <= all_of(tw)) 
  
  
  monthly <- detailed %>%
    select(-quarter) %>%
    pivot_wider(names_from = period,
                values_from = value, 
                values_fn = sum) %>% 
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.))
  
  
  quarterly <- detailed %>%
    select(-period, -type) %>%
    pivot_wider(names_from = quarter,
                values_from = value, 
                values_fn = sum) %>% 
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.))
  
  overview = monthly %>% left_join(quarterly, by = "category") %>% 
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.))
  
  
  
  
  return(list(raw_cc = ccdata,
              actuals = actuals,
              detailed = detailed,
              overview = overview))
  
  
}

# OPlan
iot_op_plan           <- function(tw){ 
  
  # detailed
  
  detailed <- openxlsx::read.xlsx("datacc_iot.xlsx", "OP") %>% 
    as_tibble() %>%  janitor::clean_names() %>% 
    mutate(period = as.Date(period, 
                            origin = "1899-12-30")) %>% 
    mutate(period = as.yearmon(period)) %>% 
    mutate(quarter = quarter(period)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>% 
    mutate(type = "op") %>% 
    filter(period <= all_of(tw))
  
  
  # monthly
  monthly <- detailed %>% 
    select(-quarter) %>% 
    pivot_wider(names_from = period, values_from = value)
  
  
  # quarterly 
  quarterly <- detailed %>% 
    select(-period, -type) %>% 
    pivot_wider(names_from = quarter,
                values_from = value,
                values_fn = sum) 
  # mutate(full_year = rowSums(select(., -category)))
  
  
  overview = monthly %>% left_join(quarterly, by = "category")
  
  
  return(list(
    detailed = detailed,
    # monthly = monthly,
    # quarterly = quarterly,
    overview = overview))
  
  
}

# Forecast
iot_fcast             <- function(tw){ 
  
  # detailed
  detailed <- openxlsx::read.xlsx("datacc_iot.xlsx", "F7") %>% 
    as_tibble() %>%  janitor::clean_names() %>% 
    mutate(period = as.Date(period, 
                            origin = "1899-12-30")) %>% 
    mutate(category = ifelse(str_detect(category,"PSD"),
                             "Product Service Technology Upgrade", category)) %>%
    group_by(category, period) %>% 
    summarise(value = sum(value), .groups = "drop") %>% 
    mutate(period = as.yearmon(period)) %>% 
    mutate(quarter = quarter(period)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>% 
    mutate(type = "fcast") %>%
    filter(period <= all_of(tw))
  
  
  
  # monthly
  monthly <- detailed %>% 
    select(-quarter) %>% 
    pivot_wider(names_from = period, values_from = value)
  
  
  # quarterly 
  quarterly <- detailed %>% 
    select(-period, -type) %>% 
    pivot_wider(names_from = quarter,
                values_from = value,
                values_fn = sum) 
  # mutate(full_year = rowSums(select(., -category)))
  
  
  
  overview = monthly %>% left_join(quarterly, by = "category") 
  
  
  return(list(
    detailed = detailed,
    # monthly = monthly,
    # quarterly = quarterly,
    overview = overview))
  
  
}



# Time Windows ------------------------------------------------------------

# MTD
mtd_output_da <- function(tw){ 
  
  MTD= dp_fcast(tw)$detailed %>% 
    bind_rows(dp_close_actuals(tw)$detailed) %>%
    bind_rows(dp_op_plan(tw)$detailed) %>%
    filter(period == tw) %>% 
    select(category,type,value) %>% 
    pivot_wider(names_from = type, 
                values_from = value,
                values_fn = sum) %>% 
    rename(MTD_forecast = fcast,
           MTD_actuals = actuals,
           MTD_OP =op) %>% 
    select(category, MTD_actuals,MTD_forecast,MTD_OP) %>%
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>%  
    mutate(MTD_actuals  = MTD_actuals/1,
           MTD_forecast = MTD_forecast/1,
           MTD_OP       = MTD_OP/1) %>% 
    mutate(MTD_VF  = MTD_actuals-MTD_forecast,
           MTD_VOP = MTD_actuals-MTD_OP) 
  
  return(MTD)
  
}

# QTD
qtd_output_da <- function(tw, q){ 
  
  QTD = dp_fcast(tw)$detailed %>% 
    bind_rows(dp_close_actuals(tw)$detailed) %>%
    bind_rows(dp_op_plan(tw)$detailed) %>%
    filter(period <= tw) %>% 
    filter(quarter == q) %>% 
    select(category,type,value) %>% 
    pivot_wider(names_from = type, 
                values_from = value,
                values_fn = sum) %>% 
    rename(QTD_forecast = fcast,
           QTD_actuals = actuals,
           QTD_OP =op) %>% 
    select(category, QTD_actuals,QTD_forecast,QTD_OP) %>%
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>%  
    mutate(QTD_actuals  = QTD_actuals/1,
           QTD_forecast = QTD_forecast/1,
           QTD_OP       = QTD_OP/1) %>% 
    mutate(QTD_VF  = QTD_actuals-QTD_forecast,
           QTD_VOP = QTD_actuals-QTD_OP) 
  
  
  
  return(QTD)
  
}

# YTD 
ytd_output_da <- function(tw){ 
  
  YTD = dp_fcast(tw)$detailed %>% 
    bind_rows(dp_close_actuals(tw)$detailed) %>%
    bind_rows(dp_op_plan(tw)$detailed) %>%
    filter(period <= tw) %>% 
    select(category,type,value) %>% 
    pivot_wider(names_from = type, 
                values_from = value,
                values_fn = sum) %>% 
    rename(YTD_forecast = fcast,
           YTD_actuals = actuals,
           YTD_OP =op) %>% 
    select(category, YTD_actuals,YTD_forecast,YTD_OP) %>%
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>%  
    mutate(YTD_actuals  = YTD_actuals/1,
           YTD_forecast = YTD_forecast/1,
           YTD_OP       = YTD_OP/1) %>% 
    mutate(YTD_VF  = YTD_actuals-YTD_forecast,
           YTD_VOP = YTD_actuals-YTD_OP) 
  
  return(YTD)
  
}



# MTD
mtd_output_iot            <- function(tw){ 
  
  MTD= iot_fcast(tw)$detailed %>% 
    bind_rows(iot_close_actuals(tw)$detailed) %>%
    bind_rows(iot_op_plan(tw)$detailed) %>%
    filter(period == tw) %>% 
    select(category,type,value) %>% 
    pivot_wider(names_from = type, 
                values_from = value,
                values_fn = sum) %>% 
    rename(MTD_forecast = fcast,
           MTD_actuals = actuals,
           MTD_OP =op) %>% 
    select(category, MTD_actuals,MTD_forecast,MTD_OP) %>%
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>%  
    mutate(MTD_actuals  = MTD_actuals/1,
           MTD_forecast = MTD_forecast/1,
           MTD_OP       = MTD_OP/1) %>% 
    mutate(MTD_VF  = MTD_actuals-MTD_forecast,
           MTD_VOP = MTD_actuals-MTD_OP) 
  
  
  return(MTD)
  
}

# QTD
qtd_output_iot            <- function(tw, q){ 
  
  QTD = iot_fcast(tw)$detailed %>% 
    bind_rows(iot_close_actuals(tw)$detailed) %>%
    bind_rows(iot_op_plan(tw)$detailed) %>%
    filter(period <= tw) %>% 
    filter(quarter == q) %>% 
    select(category,type,value) %>% 
    pivot_wider(names_from = type, 
                values_from = value,
                values_fn = sum) %>% 
    rename(QTD_forecast = fcast,
           QTD_actuals = actuals,
           QTD_OP =op) %>% 
    select(category, QTD_actuals,QTD_forecast,QTD_OP) %>%
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>%  
    mutate(QTD_actuals  = QTD_actuals/1,
           QTD_forecast = QTD_forecast/1,
           QTD_OP       = QTD_OP/1) %>% 
    mutate(QTD_VF  = QTD_actuals-QTD_forecast,
           QTD_VOP = QTD_actuals-QTD_OP) 
  
  
  return(QTD)
  
}

# YTD 
ytd_output_iot            <- function(tw){ 
  
  YTD = iot_fcast(tw)$detailed %>% 
    bind_rows(iot_close_actuals(tw)$detailed) %>%
    bind_rows(iot_op_plan(tw)$detailed) %>%
    filter(period <= tw) %>% 
    select(category,type,value) %>% 
    pivot_wider(names_from = type, 
                values_from = value,
                values_fn = sum) %>% 
    rename(YTD_forecast = fcast,
           YTD_actuals = actuals,
           YTD_OP =op) %>% 
    select(category, YTD_actuals,YTD_forecast,YTD_OP) %>%
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>%  
    mutate(YTD_actuals  = YTD_actuals/1,
           YTD_forecast = YTD_forecast/1,
           YTD_OP       = YTD_OP/1) %>% 
    mutate(YTD_VF  = YTD_actuals-YTD_forecast,
           YTD_VOP = YTD_actuals-YTD_OP) 
  
  
  return(YTD)
  
}


