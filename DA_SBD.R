
# DA Actuals automation;
# source ::: SAP KSB1 report from instance c11
# Change data path 


library(tidyverse)
library(lubridate)
library(zoo)


# datacc.xlsx contains information updated for DA cost center, historical data YTD,
# raw data without any aditional analysis, new DA Data can be appendiced manually,
# downloading the KSB1 report from SAP source for the corresponding cost center ID

# C:/Users/AEG1130/Documents/data/datacc.xlsx contains the historical data from 
# (DA CC), Appendized data coming from last fiscal close SAPc11-KSB1, report. 




# Data Integration ::: 
getwd()
setwd("C:/Users/AEG1130/Documents/data")


# ACTUALS 
da_close_actuals <- function(){
  
  # cost_centers_data.  
  ccdata <- openxlsx::read.xlsx("datacc.xlsx", "hist_raw") %>% 
    as_tibble() %>%  janitor::clean_names()
  
  
  # fix_assets
  clearing <- ccdata %>% 
    filter(grepl("FIXED ASSETS - C.I.P.", name_of_offsetting_account)) %>%
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
                               str_detect(cost_element_name,"DEMO")~"Demo Tools",
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
    relocate(.before = cost_element, category )%>%
    replace(.,is.na(.),0)
  
  

  # summarized actuals by category.
  resumen_actuals <-  actuals %>%
    group_by(category, period) %>%
    summarise(gross = sum(gross),.groups="drop") %>%
    pivot_wider(names_from = period, values_from = gross) %>%
    replace(.,is.na(.),0) %>%
    # janitor::adorn_totals() %>%
    mutate(type = "gross") %>%
    relocate(.before = category, type)
  
  
  # summarized capitalization.
  resumen_capitalized <-  actuals %>%
    group_by(category, period) %>%
    mutate(category = "Capitalized") %>% 
    summarise(clearing_account = sum(clearing_account),.groups="drop") %>%
    pivot_wider(names_from = period, values_from = clearing_account) %>%
    replace(.,is.na(.),0) %>%
    # janitor::adorn_totals() %>%
    mutate(type = "adjustment") %>%
    relocate(.before = category, type)
  
  
  resumen <- resumen_actuals %>% bind_rows(resumen_capitalized) 
  
  overview <- resumen %>% mutate(category = ifelse(type == "adjustment",
                                                   "Capitalized",category)) %>% 
    select(-type)
    
  
  
  return(list(raw_cc = ccdata, fixed_assets = clearing,
              tidy_actuals = actuals,
              summary = overview))
  
  
}

# OPlan
da_op_plan  <- function(){ 
  
# detailed
  
op_plan_21 <- openxlsx::read.xlsx("datacc.xlsx", "OP") %>% 
  as_tibble() %>%  janitor::clean_names() %>% 
  mutate(period = as.Date(period, 
                          origin = "1899-12-30")) %>% 
  mutate(period = as.yearmon(period)) %>% 
  mutate(quarter = quarter(period)) %>% 
  mutate(quarter = paste0("Q",quarter))
  

# monthly
monthly <- op_plan_21 %>% 
  select(-quarter) %>% 
  pivot_wider(names_from = period, values_from = value)


# quarterly 
quarterly <- op_plan_21 %>% 
  select(-period) %>% 
  pivot_wider(names_from = quarter,
              values_from = value,
              values_fn = sum) %>% 
  mutate(full_year = rowSums(select(., -category)))


overview = monthly %>% left_join(quarterly, by = "category")


return(list(
       detailed = op_plan_21,
       monthly = monthly,
       quarterly = quarterly,
       overview = overview))


}

# Forecast
da_forecast <- function(){ 
  
  # detailed
  
  forecast_21 <- openxlsx::read.xlsx("datacc.xlsx", "F7") %>% 
    as_tibble() %>%  janitor::clean_names() %>% 
    mutate(period = as.Date(period, 
                            origin = "1899-12-30")) %>% 
    mutate(period = as.yearmon(period)) %>% 
    mutate(quarter = quarter(period)) %>% 
    mutate(quarter = paste0("Q",quarter))
  
  
  # monthly
  monthly <- forecast_21 %>% 
    select(-quarter) %>% 
    pivot_wider(names_from = period, values_from = value)
  
  
  # quarterly 
  quarterly <- forecast_21 %>% 
    select(-period) %>% 
    pivot_wider(names_from = quarter,
                values_from = value,
                values_fn = sum) %>% 
    mutate(full_year = rowSums(select(., -category)))
  
  
  
  overview = monthly %>% left_join(quarterly, by = "category") 

  
  return(list(
    detailed = op_plan_21,
    monthly = monthly,
    quarterly = quarterly,
    overview = overview))
  
  
}







act = da_close_actuals()$summary

op = da_op_plan()$overview

fcast = da_forecast()$overview














# DA Wrangling  -----------------------------------------------------------

DA  = "S:/North_America/Baltimore-BLT/Transformation Office/Admn/Digital Accelerator Reporting"
setwd(DA)

jul.da <- openxlsx::read.xlsx("07 Jul_DA_Close.xlsx") %>% as_tibble() %>%
  janitor::clean_names()

jul.da %>% 
  # mutate(period = month.abb[as.numeric(period)]) %>% 
  group_by(cost_center, period) %>% 
  summarise(value = sum(val_in_rep_cur),.groups = "drop") %>%
  pivot_wider(names_from = period, values_from = value) %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) 



jul.da %>% 
  # mutate(period = month.abb[as.numeric(period)]) %>% 
  group_by(cost_center, period) %>% 
  summarise(n = n(),.groups = "drop") %>%
  pivot_wider(names_from = period, values_from = n) %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.))



hoppe_consolidation <- function(){ 

setwd("C:/Users/AEG1130/Documents/data")

hoppe = openxlsx::read.xlsx("hoppe.xlsx") %>% as_tibble 

consol_da = hoppe %>% 
  group_by(Spend.Categories,Vendors, time_frame) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  mutate(Spend.Categories = ifelse(str_detect(Spend.Categories,"PSD"),
                                   "Product Service Investment",Spend.Categories)) %>% 
  mutate(Spend.Categories = str_trim(Spend.Categories)) %>% 
  pivot_wider(names_from = time_frame,
              values_from = c(Actual, F07,VF07, OP, VOP),
              values_fn = sum) %>% 
  select(Spend.Categories, Vendors, contains("QTD"), contains("YTD")) %>% 
  ungroup() %>%
  arrange(desc(Actual_QTD))

grouped_leader <- hoppe %>% 
  group_by(Account, time_frame) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  pivot_wider(names_from = time_frame,
              values_from = c("Actual", "F07", "VF07", "OP", "VOP")) %>% 
  select(Account,contains("QTD"), everything()) %>% 
  janitor::adorn_totals()


return(list(details = consol_da ,
            by_leader = grouped_leader
       ))


}


hoppe_consolidation %>%
  openxlsx::write.xlsx(.,"../hoppe_consol.xlsx", overwrite = T)



# https://gt.rstudio.com/articles/case-study-gtcars.html
