
# DA Actuals automation;
# source ::: SAP KSB1 report from instance c11
# Change data path 


library(tidyverse)
library(lubridate)
library(zoo)


# datacc_da.xlsx contains information updated for DA cost center, historical data YTD,
# raw data without any aditional analysis, new DA Data can be appendiced manually,
# downloading the KSB1 report from SAP source for the corresponding cost center ID

# C:/Users/AEG1130/Documents/data/datacc_da.xlsx contains the historical data from 
# (DA CC), Appendized data coming from last fiscal close SAPc11-KSB1, report. 




# Data Integration ::: Directly Sourced. 
getwd()
setwd("C:/Users/AEG1130/Documents/data")




# DA Analysis  ------------------------------------------------------------



# Returns the overview for actuals, OP and forecast.
# detailed process embeed into each function. 
# where tw = time window as specific period monthyear  e.g: "Jan 2021"
# Final output is a single table containing all observation in a pivot 
# wider structure, cutting at the inputed monthyear, to visualize an 
# specific observational unit, use the type filter. 
consolidated_data <- function(tw){ 

# ACTUALS 
da_close_actuals <- function(tw){
  
  # cost_centers_data.  
  ccdata <- openxlsx::read.xlsx("datacc_da.xlsx", "hist_raw") %>% 
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
    rename(value = gross)
    
  
  
  # summarized capitalization.
  resumen_capitalized <-  actuals %>%
    group_by(category, period) %>%
    mutate(category = "Capitalized") %>% 
    summarise(clearing_account = sum(clearing_account),.groups="drop") %>% 
    rename(value = clearing_account)
  
    # pivot_wider(names_from = period, values_from = clearing_account) %>%
    # replace(.,is.na(.),0) %>%
    # # janitor::adorn_totals() %>%
    # mutate(type = "adjustment") %>%
    # relocate(.before = category, type)
  
  
  detailed <- resumen_actuals %>% 
    bind_rows(resumen_capitalized) %>% 
    mutate(period = as.Date(period, 
                            origin = "1899-12-30")) %>% 
    mutate(period = as.yearmon(period)) %>% 
    mutate(quarter = quarter(period)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>% 
    mutate(type = "actuals") %>% 
    filter(period <= tw ) 

  
  
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
              monthly = monthly, 
              quarterly = quarterly, 
              overview = overview))
  
  
}

# OPlan
da_op_plan  <- function(tw){ 
  
# detailed
  
  detailed <- openxlsx::read.xlsx("datacc_da.xlsx", "OP") %>% 
  as_tibble() %>%  janitor::clean_names() %>% 
  mutate(period = as.Date(period, 
                          origin = "1899-12-30")) %>% 
  mutate(period = as.yearmon(period)) %>% 
  mutate(quarter = quarter(period)) %>% 
  mutate(quarter = paste0("Q",quarter)) %>% 
  mutate(type = "op") %>% 
  filter(period <= tw )
  

  

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
       monthly = monthly,
       quarterly = quarterly,
       overview = overview))


}

# Forecast
da_forecast <- function(tw){ 
  
  # detailed
  detailed <- openxlsx::read.xlsx("datacc_da.xlsx", "F7") %>% 
    as_tibble() %>%  janitor::clean_names() %>% 
    mutate(period = as.Date(period, 
                            origin = "1899-12-30")) %>% 
    mutate(period = as.yearmon(period)) %>% 
    mutate(quarter = quarter(period)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>% 
    mutate(type = "fcast") %>%
    filter(period <= tw )
  
  
  
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
    monthly = monthly,
    quarterly = quarterly,
    overview = overview))
  
  
}

# consolidation
da_actuals  = da_close_actuals(tw)$overview
da_forecast = da_forecast(tw)$overview
da_op       = da_op_plan(tw)$overview


consolidated_data = da_actuals %>%
                    bind_rows(da_op) %>%
                    bind_rows(da_forecast) %>% 
  relocate(.before = Q1, tw) %>% 
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.))



return(consolidated_data)

}


# Returns the final table with the overview by time window
# details about algorithm insight each function embeed.
# the inputs are also time windows, but the underlying 
# data for the outputs creation is the consolidated_data()
digital_products <- function(period, quarter){ 

data <- consolidated_data(period)
    
# MTD
mtd_output <- function(period){ 
  
act = data %>% 
  filter(type == "actuals") %>% 
  select(category,
         contains(all_of(period))) %>% 
  rename(MTD_actuals = period)


op = data %>% 
  filter(type == "op") %>% 
  select(category,
         contains(all_of(period))) %>%
  rename(MTD_OP = period)


fcast = data %>% 
  filter(type == "fcast") %>% 
  select(category,
         contains(all_of(period))) %>%
  rename(MTD_forecast = period)


MTD = op %>%
  left_join(fcast, by = "category") %>%
  left_join(act, by = "category") %>%
  select(category, MTD_actuals,MTD_forecast,MTD_OP) %>%
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>%  
  mutate(MTD_actuals  = MTD_actuals/1000,
         MTD_forecast = MTD_forecast/1000,
         MTD_OP       = MTD_OP/1000) %>% 
  mutate(MTD_VF  = MTD_actuals-MTD_forecast,
         MTD_VOP = MTD_actuals-MTD_OP) %>% 
  mutate_if(is.numeric, round)

return(MTD)

}

# QTD
qtd_output <- function(period, quarter){ 
  
  act = data %>% 
    filter(type == "actuals") %>% 
    select(category,
           contains(all_of(quarter))) %>% 
    rename(QTD_actuals = quarter)
  
  op = data %>% 
    filter(type == "op") %>% 
    select(category,
           contains(all_of(quarter))) %>%
    rename(QTD_OP = quarter)
  
  fcast = data %>%
    filter(type == "fcast") %>% 
    select(category,
           contains(all_of(quarter))) %>%
    rename(QTD_forecast = quarter)
  
  
  QTD = op %>%
    left_join(fcast, by = "category") %>%
    left_join(act, by = "category") %>%
    select(category, QTD_actuals,QTD_forecast,QTD_OP) %>%
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>% 
    mutate(QTD_actuals  = QTD_actuals/1000,
           QTD_forecast = QTD_forecast/1000,
           QTD_OP       = QTD_OP/1000) %>% 
    mutate(QTD_VF  = QTD_actuals-QTD_forecast,
           QTD_VOP = QTD_actuals-QTD_OP) %>% 
    mutate_if(is.numeric, round)
  
  
  
  return(QTD)
  
}

# YTD 
ytd_output <- function(period){ 
  
  act = data %>% 
    filter(type == "actuals") %>% 
    select(!contains("Q"),-type) %>% 
    mutate(ytd = rowSums(select(., -category))) %>% 
    select(category, YTD_actuals = ytd)
    
  
  op = data %>%
    filter(type == "op") %>% 
    select(!contains("Q"),-type) %>% 
    mutate(ytd = rowSums(select(., -category))) %>% 
    select(category, YTD_OP = ytd)
  
  
  
  fcast = data %>% 
    filter(type == "fcast") %>% 
    select(!contains("Q"),-type) %>% 
    mutate(ytd = rowSums(select(., -category))) %>% 
    select(category, YTD_forecast = ytd)
  
  
  
  YTD = op %>%
    left_join(fcast, by = "category") %>%
    left_join(act, by = "category") %>%
    select(category, YTD_actuals,YTD_forecast,YTD_OP) %>%
    mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>% 
    mutate(YTD_actuals  = YTD_actuals/1000,
           YTD_forecast = YTD_forecast/1000,
           YTD_OP       = YTD_OP/1000) %>% 
    mutate(YTD_VF  = YTD_actuals-YTD_forecast,
           YTD_VOP = YTD_actuals-YTD_OP) %>% 
    mutate_if(is.numeric, round)
  
  
  
  return(YTD)
  
}

# consolidation
mtd = mtd_output(period)
qtd = qtd_output(period, quarter)
ytd = ytd_output(period)


overview = mtd %>%
  left_join(qtd, by = "category") %>% 
  left_join(ytd, by = "category")


return(overview)

}


# Produce the output and generate a visual of it 
table = digital_products("Aug 2021", "Q3")
table %>% flextable::flextable()


# IoT Analysis -----------------------------------------------------------






# Hoppe Consolidation ----------------------------------------------------

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
