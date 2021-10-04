

# Generalities ------------------------------------------------------------


# SBD GTS FP&A TO Analytics, Kurt Hoppe Innovation. 
# Slanlytics 


# DA XOR Digital Products Actuals automation;
# source ::: SAP KSB1 available at instance c11

library(tidyverse)
library(lubridate)
library(openxlsx)
library(zoo)


# Periodicity / A step forward in abstraction 4 Automation. 
  
  setwd("~/projects/sbd_gts_analytics")
  
  fisCal <-   readRDS("fisCal.Rds")
  tw     <-   as.yearmon(today()%m-% months(1) %m-% days(3))
  q      <-   paste0("Q", quarter(tw))
 

# datacc_da.xlsx_contrast.xlsx contains information updated for DA cost center, historical data YTD,
# raw data without any aditional analysis, new DA Data can be appendiced manually,
# downloading the KSB1 report from SAP source for the corresponding cost center IDs
# CCs for DA {221,225,226,227}


# Data Integration ::: Directly Sourced. 
getwd()
setwd("C:/Users/AEG1130/Documents/data/hoppe_innovation")



# DA Analysis  ------------------------------------------------------------


# Returns the overview for actuals, OP and forecast.
# detailed process into each function. 
# where tw = time window as specific period month year  e.g: "Jan 2021"
# Final output contains 3 tables detailed, consolidated, overview. 
# detailed specific observational units types are filterable. 
# DA = cc(221, 225, 226, 227)< IoT = (233)

digital_products <- function(tw, q){ 
  
  
# ACTUALS 
dp_close_actuals <- function(tw){
  
  # cost_centers_data.  
  ccdata <- openxlsx::read.xlsx("datacc_da.xlsx", "hist_raw") %>% 
    as_tibble() %>%  janitor::clean_names()
  
  
  # fix_assets
  clearing <- ccdata %>% 
    filter(grepl("C.I.P.", name_of_offsetting_account)) %>%
    select(cost_element, cost_element_name, period,val_in_rep_cur) %>%
    rename(clearing_account = val_in_rep_cur) %>% 
    distinct()
  
  
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
                               str_detect(cost_element_name,"PR TAXE")~"C&B",
                               str_detect(cost_element_name,"WAGE")~"C&B",
                               str_detect(cost_element_name,"DEMO")~"Demo Tools - FG Stock",
                               str_detect(cost_element_name,"OS FEE LABOR")~"Professional Fees - Globant",
                               str_detect(cost_element_name,"PROMO SPECIAL P")~"Promo Services",
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
                               str_detect(cost_element_name,"HAMILTON MANU")~"Gyro Development - Didio",
                               TRUE ~ as.character("Others"))) %>%
    relocate(.before = cost_element, category )

  

  # summarized actuals by category.
  resumen_actuals <-  actuals %>%
    group_by(category, period) %>%
    summarise(gross = sum(gross),.groups="drop") %>% 
    rename(value = gross)
    
  
  
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


# detailed 
detailed_data_dp     <- dp_close_actuals(tw)$detailed %>% 
                    bind_rows(dp_op_plan(tw)$detailed) %>% 
                    bind_rows(dp_fcast(tw)$detailed) %>% 
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) 
  


# consolidation
consolidated_data_dp <- dp_close_actuals(tw)$overview %>%
                    bind_rows(dp_op_plan(tw)$overview) %>%
                    bind_rows(dp_fcast(tw)$overview) %>% 
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>% 
  select(category, type, contains(month.abb),contains("Q"))


# Returns the final table with the overview by time window
# details about algorithm insight each function embeed.
# the inputs are also time windows, but the underlying 
# data for the outputs creation is the consolidated_data()
# data comming from SAP and Sharepoint. 



# Rolling Windows ////////////////////////////////////////////////////////////




# MTD
mtd_output <- function(tw){ 
  
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
        mutate(MTD_actuals  = MTD_actuals/1000,
               MTD_forecast = MTD_forecast/1000,
               MTD_OP       = MTD_OP/1000) %>% 
        mutate(MTD_VF  = MTD_actuals-MTD_forecast,
               MTD_VOP = MTD_actuals-MTD_OP) 

return(MTD)

}

# QTD
qtd_output <- function(tw, q){ 
  
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
    mutate(QTD_actuals  = QTD_actuals/1000,
           QTD_forecast = QTD_forecast/1000,
           QTD_OP       = QTD_OP/1000) %>% 
    mutate(QTD_VF  = QTD_actuals-QTD_forecast,
           QTD_VOP = QTD_actuals-QTD_OP) 

  
  
  return(QTD)
  
}

# YTD 
ytd_output <- function(tw){ 
  
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
    mutate(YTD_actuals  = YTD_actuals/1000,
           YTD_forecast = YTD_forecast/1000,
           YTD_OP       = YTD_OP/1000) %>% 
    mutate(YTD_VF  = YTD_actuals-YTD_forecast,
           YTD_VOP = YTD_actuals-YTD_OP) 
  
  return(YTD)
  
}


# consolidation
mtd = mtd_output(tw)
qtd = qtd_output(tw, q)
ytd = ytd_output(tw)


overview_dp = mtd %>%
  left_join(qtd, by = "category") %>% 
  left_join(ytd, by = "category") %>% 
  mutate_if(is.numeric, round) %>% 
  relocate(.before = MTD_OP, MTD_VF) %>% 
  relocate(.before = QTD_OP, QTD_VF) %>% 
  relocate(.before = YTD_OP, YTD_VF) %>% 
  separate(category, c("category", "vendor"),"-") %>% 
  mutate_if(is.character, str_trim)

  

return(list(consolidated_data_dp = consolidated_data_dp,
            detailed_data_dp = detailed_data_dp,
            overview_dp = overview_dp))


}


  

# Function exec delivers the 3 outputs that can be combined and or visualize. 
# DA_tables$overview %>% flextable::flextable()



# IoT Analysis -----------------------------------------------------------

# [[ datacc_iot.xlsx ]]

# As well as dp, KSB1 c11 SAP it's the source. 
# cost of element name, equals the pivot table at IoT sheet, its just re-arranged
# and primitively categorized as C&B or NonCB class. historically natural grouping 
# consist in 16 elements, UI is structure in categories, incluiding 
# supplies & others, Capitalized always Zero. 


iot_products <- function(tw, q){
  
  
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
                                 str_detect(cost_element_name,"DEMO")~"Demo Tools - FG Stock",
                                 str_detect(cost_element_name,"OS FEE RECRUIT")~"Recruiting",
                                 str_detect(cost_element_name,"RENT BUILD")~"Rent",
                                 str_detect(cost_element_name,"AMORTIZ SOFTW")~"Software Amortization",
                                 str_detect(cost_element_name,"MATL PROTO")~"Cloud Usage and Support - AWS",
                                 str_detect(cost_element_name,"OS FEE LEGAL GEN")~"Supplies",
                                 str_detect(cost_element_name,"SUPPLIES")~"Supplies",
                                 str_detect(cost_element_name,"T&E")~"T&E",
                                 str_detect(cost_element_name,"UTILITY TELEP")~"Telephone",
                                 str_detect(cost_element_name,"OS FEE GENERAL")~"IoT Cloud Service - AG Software",
                                 str_detect(cost_element_name,"MISC AC")~"ConsumerApp - Zigatta",
                                 str_detect(cost_element_name,"ZIGATTA")~"ConsumerApp - Zigatta",
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
      
      mutate(category = ifelse(category == "ConsumerApp - ZIGATTA" & period == "Aug 2021",
                               "IoT Cloud Service - AG Software", category)
      ) %>%
      
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
  
  
  
  # detailed 
  detailed_data_iot     <- iot_close_actuals(tw)$detailed %>% 
    bind_rows(iot_op_plan(tw)$detailed) %>% 
    bind_rows(iot_fcast(tw)$detailed) %>% 
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.))
  
  
  # consolidation
  consolidated_data_iot <- iot_close_actuals(tw)$overview %>%
    bind_rows(iot_op_plan(tw)$overview) %>%
    bind_rows(iot_fcast  (tw)$overview) %>% 
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>% 
    select(category, type, contains(month.abb),contains("Q"))
  
  
  
  # Rolling Windows ////////////////////////////////////////////////////////////
  
  
  
  # MTD
  mtd_output            <- function(tw){ 
    
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
      mutate(MTD_actuals  = MTD_actuals/1000,
             MTD_forecast = MTD_forecast/1000,
             MTD_OP       = MTD_OP/1000) %>% 
      mutate(MTD_VF  = MTD_actuals-MTD_forecast,
             MTD_VOP = MTD_actuals-MTD_OP) 

    
    return(MTD)
    
  }
  
  # QTD
  qtd_output            <- function(tw, q){ 
    
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
      mutate(QTD_actuals  = QTD_actuals/1000,
             QTD_forecast = QTD_forecast/1000,
             QTD_OP       = QTD_OP/1000) %>% 
      mutate(QTD_VF  = QTD_actuals-QTD_forecast,
             QTD_VOP = QTD_actuals-QTD_OP) 
    
    
    return(QTD)
    
  }
  
  # YTD 
  ytd_output            <- function(tw){ 
    
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
      mutate(YTD_actuals  = YTD_actuals/1000,
             YTD_forecast = YTD_forecast/1000,
             YTD_OP       = YTD_OP/1000) %>% 
      mutate(YTD_VF  = YTD_actuals-YTD_forecast,
             YTD_VOP = YTD_actuals-YTD_OP) 

    
    return(YTD)
    
  }
  
  
  
  # consolidation
  mtd = mtd_output(tw)
  qtd = qtd_output(tw, q)
  ytd = ytd_output(tw)
  
  
  overview_iot = mtd %>%
    left_join(qtd, by = "category") %>% 
    left_join(ytd, by = "category") %>% 
    mutate_if(is.numeric, round) %>% 
    relocate(.before = MTD_OP, MTD_VF) %>% 
    relocate(.before = QTD_OP, QTD_VF) %>% 
    relocate(.before = YTD_OP, YTD_VF) %>% 
    separate(category, c("category", "vendor"),"-") %>% 
    mutate_if(is.character, str_trim)
  
  
  
  return(list(consolidated_data_iot = consolidated_data_iot,
              detailed_data_iot = detailed_data_iot,
              overview_iot = overview_iot))
  
  
}




# Hoppe Consolidation ----------------------------------------------------


hoppe_innovation_summary <- function(){ 

  
  hoppe_consol <-
    DA_tables$overview_dp %>% 
    bind_rows(IoT_tables$overview_iot) %>% 
    group_by(category, vendor) %>% 
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    ungroup()
  
  
  hoppe_consol_resumen <- 
    hoppe_consol %>% 
    mutate(category = ifelse(str_detect(category,"PSD"),
                             "Product Service Investment", category)) %>% 
    group_by(category, vendor) %>% 
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    ungroup()
  
  
  return(hoppe_consol_resumen)
  
  }


# Outputs Storage ---------------------------------------------------------



DA_tables <- digital_products(tw, q) 
IoT_tables <- iot_products(tw,q)
hoppe_consolidation <- hoppe_innovation()





# Data Fcast Management ---------------------------------------------------

# re-populate Forecast with delivered close report actuals 

refresh.fcast.data_da <- function(tw){ 
  
  
  # Close actuals
  closed.data  <-  DA_table %>%
    select(category, MTD_actuals) %>%
    rename(value  = MTD_actuals) %>% 
    mutate(period = tw) %>% 
    relocate(.before = value, period)
  
  
  # Refreshin forecast 
  current.fcast <- openxlsx::read.xlsx("datacc_da.xlsx", "F7") %>% 
    as_tibble() %>%  janitor::clean_names() %>% 
    mutate(period = as.Date(period, origin = "1899-12-30")) %>% 
    mutate(period = as.yearmon(period)) %>% 
    mutate(period = as.character(period)) %>% 
    filter(period != tw)
  
  
  
  # Update forecast 
  updated.fcast <- current.fcast %>%
    bind_rows(closed.data) %>% 
    mutate(period = as.yearmon(period))
  
  
  
  # File Management :::
  wb <- openxlsx::loadWorkbook("datacc_da.xlsx")
  
  addWorksheet(wb,"Fcast")
    writeDataTable(wb, sheet = "Fcast", updated.fcast)
      removeWorksheet(wb, "F7")
        renameWorksheet(wb, "Fcast", "F7")
          saveWorkbook(wb,"datacc_da.xlsx",overwrite = T)
  
  
  print("updated 100%")
  
}




