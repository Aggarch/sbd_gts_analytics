
# Generalities ------------------------------------------------------------


# SBD GTS FP&A TO Analytics, Transformation Office. 
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



# Data Integration ::: Directly Sourced. 
getwd()
setwd("C:/Users/AEG1130/Documents/data/transformation_office/KSB1_RAW")


# TO Analysis  ------------------------------------------------------------

transform_off <- function(tw, q){ 
  
  
  # ACTUALS 
  to_close_actuals <- function(tw){
    
    # cost_centers_data.  
    ccdata <- openxlsx::read.xlsx("datacc_to.xlsx", "hist_raw") %>% 
      as_tibble() %>%  janitor::clean_names()
   
    
    # tidy_actuals.
    actuals <- ccdata %>%
      filter(!cost_element %in% c("9585000","5363840")) %>%
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
                                 str_detect(cost_element_name,"DEMO")~"Demo Tools - FG Stock",
                                 str_detect(cost_element_name,"OS FEE LABOR")~"Professional Fees - Globant",
                                 str_detect(cost_element_name,"OS FEE GENERAL")~"Services Fees - Cambridge",
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
      mutate(MTD_actuals  = MTD_actuals/1,
             MTD_forecast = MTD_forecast/1,
             MTD_OP       = MTD_OP/1) %>% 
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
      mutate(QTD_actuals  = QTD_actuals/1,
             QTD_forecast = QTD_forecast/1,
             QTD_OP       = QTD_OP/1) %>% 
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
      mutate(YTD_actuals  = YTD_actuals/1,
             YTD_forecast = YTD_forecast/1,
             YTD_OP       = YTD_OP/1) %>% 
      mutate(YTD_VF  = YTD_actuals-YTD_forecast,
             YTD_VOP = YTD_actuals-YTD_OP) 
    
    return(YTD)
    
  }
  
  
  # consolidation
  mtd = mtd_output(tw)
  qtd = qtd_output(tw, q)
  ytd = ytd_output(tw)
  
  
  overview_samort = ytd %>%
    left_join(qtd, by = "category") %>% 
    left_join(mtd, by = "category") %>% 
    relocate(.before = MTD_OP, MTD_VF) %>% 
    relocate(.before = QTD_OP, QTD_VF) %>% 
    relocate(.before = YTD_OP, YTD_VF) %>% 
    filter(grepl("Software Amort",category)) %>% 
    separate(category, c("category", "vendor"),"/") %>% 
    mutate_if(is.character, str_trim)
  
  
  
  overview_all = ytd %>%
    left_join(qtd, by = "category") %>% 
    left_join(mtd, by = "category") %>% 
    relocate(.before = MTD_OP, MTD_VF) %>% 
    relocate(.before = QTD_OP, QTD_VF) %>% 
    relocate(.before = YTD_OP, YTD_VF) %>% 
    filter(!grepl("Software Amort",category)) %>% 
    separate(category, c("category", "vendor"),"-") %>% 
    mutate_if(is.character, str_trim)
  
  
  overview_dp = overview_all %>% 
    bind_rows(overview_samort) %>% 
    select(category, vendor, contains("MTD"),
           contains("QTD"),
           contains("YTD"))
  
  
  
  return(list(consolidated_data_dp = consolidated_data_dp,
              detailed_data_dp = detailed_data_dp,
              overview_dp = overview_dp))
  
  
}
