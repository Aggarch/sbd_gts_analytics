
# <> Net Sales Structure <>  --------------------------------------------------------


# From Basic structure, {COLLECTION/IMPORTATION}
# Iterative functionality to offer all Ad hocs organics across Regions, . 
# BA&R recursive refreshal as a result of HsGet formula by row. 

# Reference: 
# CFP&A Organic % Analysis (Corporate Ad Hoc). 


library(tidyverse)
library(lubridate)

net_sales_data  <- "C:/Users/AEG1130/Documents/Net Sales/"

setwd(net_sales_data)



# Chasis Struct -----------------------------------------------------------

# Build all the queries needed to extract the last 5 years P&L Data. 
query_meta <- function(observ_year, observ_month, product){ 
  
  file <- openxlsx::read.xlsx("ref/tidy_ns_query.xlsx") %>% as_tibble()
  
  
  chasis<- file %>% 
    janitor::clean_names() %>% 
    select(-years, -period) %>% 
    mutate(observation = observ_year,
           period = observ_month) 

  return(chasis)
  
}



# Times & Iterations ------------------------------------------------------

timing  <- function(period){ 
  
  times <- tibble(observ_month  = month.name) %>% 
    mutate(observ_year = period) 

  return(times)
  
}

years   <- tibble(period = 2018  : year(today()))

periods <-  map(years$period,
                timing) %>% 
  map_dfr(., bind_rows) 



# Products Iteration ------------------------------------------------------

product  <- tibble(prd = c("Total Product", "PTG", "HTAS", "Other"))

fill.prd <- function(prd){
  ns_struct %>% 
  mutate(product = prd)
}



# Recursive Scale RAW ---------------------------------------------------------
# Output to be BAR Refreshed 

nsales <- function(){ 
  
  ns_struct <- map2(periods$observ_year, 
                    periods$observ_month,
                    query_meta) %>% 
               map_dfr(., bind_rows) 
    
  
  structure<-  map(product$prd,fill.prd) %>% 
               map_dfr(., bind_rows) %>% 
    
    arrange(desc(observation)) %>% 
    mutate(year_num = substr(observation, 3,4)) %>% 
    mutate(year_num = as.double(year_num)) %>% 
    mutate(year_num = ifelse(type == "actual_sales_PY", year_num-1, year_num)) %>% 
    mutate(year_num = ifelse(type == "actual_sales_2PY", year_num-2, year_num)) %>% 
    
    mutate(years = paste0("FY",year_num)) %>% 
    mutate(month = match(period, month.name)) %>%  
    mutate(ref_date = make_date(year = observation, month = month, day = 1L)) %>% 
    mutate(quarter = quarter(ref_date)) %>% 
    mutate(quarter = paste0("Q",quarter)) %>%  
    # filter(ref_date <= today()%m-%months(1) %>% rollback(roll_to_first = T)) %>% 
    mutate(index = row_number()+1) %>% 
    mutate(index = as.character(index)) %>% 
    mutate(result = '=HsGetValue("PRD_OAC_RPTSBD01","Entity#"&E2,"Scenario#"&F2,"Years#"&N2,"Period#"&O2,"Account#"&G2,"Currency#"&H2,"Total Product#"&I2,"Total Customer#"&J2,"Function#"&K2,"DTS#"&P2,"Total Ship-to Geography#"&L2,"Total Brand#"&M2)/1000000') %>% 
    mutate(result = str_replace_all(result,'[[:digit:]]+',index)) %>% 
    mutate(result = str_replace_all(result,'/[[:digit:]]+',"/1000000")) %>% 
    mutate(result = str_replace_all(result,'PRD_OAC_RPTSBD[[:digit:]]+',"PRD_OAC_RPTSBD01")) %>%  
    relocate(.after = dts, result) %>%  
    relocate(.after = brand, years) %>% 
    relocate(.after = years, period) %>% 
    mutate(index = as.numeric(index))
  
  
  the_others <- structure %>% select(region_channel, type, customer, observation, period, product, index) %>% 
         pivot_wider(names_from = product, values_from = index) %>% 
         mutate(Other_form = paste0("=Q",`Total Product`,"-Q",PTG,"-Q",HTAS)) %>% 
         rename(index = Other) %>% 
    select(-`Total Product`,-PTG,-HTAS)
  
  
  ns_structure <- structure %>%
    left_join(the_others %>%
                select(index, Other_form), by = "index") %>% 
    mutate(result = ifelse(product == "Other",Other_form, result)) %>% 
    select(-Other_form) %>% 
    mutate(result = ifelse(region_channel == "Retail Other", paste0("=Q",index-7,"-SUM(Q",index-6,":Q",index-1,")"),result)) %>% 
    mutate(result = ifelse(region_channel == "NA Other", paste0("=Q",index-11,"-Q",index-10,"-Q",index-2,"-Q",index-1),result)) %>% 
    mutate(result = ifelse(region_channel == "EMEA ANZ Other", paste0("=Q",index-7,"-SUM(Q",index-6,":Q",index-1,")"),result)) %>% 
    mutate(result = ifelse(region_channel == "LAG Other", paste0("=Q",index-4,"-SUM(Q",index-3,":Q",index-1,")"),result)) %>% 
    mutate(result = ifelse(region_channel == "Asia Other", paste0("=Q",index-5,"-SUM(Q",index-4,":Q",index-1,")"),result))
  
  
    ns_structure %>% openxlsx::write.xlsx("ns_struct_hist.xlsx", overwrite = T)
    
    # 595*5*12*4
    
  return(ns_structure)
  
}



# BAR Pull Results Wider --------------------------------------------------

netsales_data  <- "C:/Users/AEG1130/Documents/Net Sales/PBI"
setwd(netsales_data)

# Enable the prioritization with doubles


 prior <- openxlsx::read.xlsx("raw_ns.xlsx", sheet = "priority") %>% as_tibble()
 p.order <- openxlsx::read.xlsx("raw_ns.xlsx", sheet = "p.order") %>% as_tibble()
 r.order <- openxlsx::read.xlsx("raw_ns.xlsx", sheet = "r.order") %>% as_tibble()


netsales <- openxlsx::read.xlsx("raw_ns.xlsx") %>% 
  as_tibble() %>% 
  mutate(result = ifelse(is.na(result),0,result)) %>% 
  mutate(region = ifelse(region_channel == "GTS HQ", "GTS HQ", region))%>%
  mutate(region = ifelse(region_channel == "GTS HQ Hedge", "GTS HQ Hedge",region)) %>% 
 
  # left_join(prior, by = "region_channel") %>% 
  # relocate(.before = region_channel, priority) %>% 
  
  left_join(p.order, by = "product") %>%
  relocate(.before = product, p.order) %>%

  left_join(r.order, by = "region") %>%
  relocate(.before = region, r.order)


netsales %>% openxlsx::write.xlsx(.,"rw.p.xlsx", overwrite = T)


# Report Structured Embeed ------------------------------------------------
# grouped_ns resides here Wider Pivot mimics report originals : 

net_sales_ingest <- function(){ 
  
grouped_ns <- netsales %>% 
  
  mutate(region_channel = ifelse(product == "HTAS" & region_channel == "Tools", "HTAS", region_channel)) %>% 
  mutate(region_channel = ifelse(product == "PTG" & region_channel == "Tools", "PTG", region_channel)) %>% 
  mutate(region_channel = ifelse(product == "Other" & region_channel == "Tools", "Outdoor", region_channel)) %>% 
  
  mutate(region = ifelse(region_channel == "GTS HQ", "GTS HQ", region))%>%
  mutate(region = ifelse(region_channel == "GTS HQ Hedge", "GTS HQ Hedge",region)) %>% 

  
 group_by(observation, ref_date,quarter,
           period, product, region,
           region_channel, type, priority, r.order,p.order) %>% 
  summarise(result = sum(result),.groups = "drop") %>% 
  pivot_wider(names_from = type, values_from = result) %>% 
  
  # subzero items 
  mutate(fcst_salesacqdiv = actual_salesacqdiv - fcst_salesacqdiv) %>% 
  mutate(op_salesacqdiv = actual_salesacqdiv - op_salesacqdiv) %>% 
  # 10 fields formulation
  
  # VPY
  mutate(sales_vpy = actual_sales - actual_sales_PY,
           org_vpy = actual_sales - actual_sales_PY - actual_salesfx - actual_salesacqdiv) %>% 
  
  # VFcst
  mutate(sales_vfcst = actual_sales - fcst_sales_qr,
         price_vfcst = actual_price - fcst_price,
         vol_vfcst   = sales_vfcst  - actual_salesfxvqr - price_vfcst - fcst_salesacqdiv,
         org_vfcst   = vol_vfcst + price_vfcst) %>% 

  # VOP
  mutate(sales_vop  = actual_sales - op_sales,
         price_vop  = actual_price - op_price,
         vol_vop    = sales_vop    - actual_salesfxvop - price_vop - op_salesacqdiv, #AO86-AQ86-AY86-AS86
         org_vop    = vol_vop + price_vop) %>% 

  # ///////////////////////////
  
  # organization
  select(priority,p.order,r.order,
         ref_date,quarter,observation,period,
         product,region,region_channel,
         
         actual_sales,fcst_sales_qr,op_sales,actual_sales_PY,
         actual_sales_2PY,fcst_price,op_price,fcst_salesvol,op_salesvol,
         
         #VPY
         sales_vpy, salesfx_vpy = actual_salesfx, salesacqdiv_vpy = actual_salesacqdiv,
         org_vpy, salesvol_vpy = actual_salesvol, price_vpy = actual_price,
         
         #VQR
         sales_vfcst, salesfx_vfcst = actual_salesfxvqr, salesacqdiv_vfcst = fcst_salesacqdiv,
         org_vfcst, salesvol_vfcst = vol_vfcst, price_vfcst,
         
         #VOP
         sales_vop, salesfx_vop = actual_salesfxvop, salesacqdiv_vop = op_salesacqdiv,
         org_vop, salesvol_vop = vol_vop, price_vop)
  
  return(grouped_ns)

}


  


# Ratios Tooltip ----------------------------------------------------------

# Must Tie Out, Should Organics, Volume and Price be formulated the same way 
# in HTAS as it is Total Product and PTG. 
# If we understood PTG as the Total Basket why should the elements of the basket 
# follow different rules than the benchmark? The sub of the BUs organics should 
# be equivalent to the Total Org. 

# This formulated function do work for specifc months, however not for YTD 
# Perspective, thats b/c being linear and not summing al vector before calcuating


# This Rates XOR Kpis table do works at least for salesvpyratio, do works for 
# VFcst and VOP, If that-s the case an additional date-wise function needed.

# Organics Measure example 
# organics_dynamic = ((SUM(summary[sales_actual_mtd])-SUM(summary[sales_PY_mtd])-
#                      SUM(summary[sales_fx_mtd])-SUM(summary[sales_acqdiv_mtd])) /
#                      SUM(summary[sales_PY_mtd] ))


# Mirror the PBI grouped_ns table and create 



kpis_table <- function(){ 

ratios <- grouped_ns %>%

  # VPY Ratios
  mutate(sales_vpy_ratio        = sales_vpy/actual_sales_PY) %>% 
  mutate(fx_vpy_ratio           = salesfx_vpy/actual_sales_PY) %>% 
  mutate(salesacqdiv_vpy_ratio  = salesacqdiv_vpy/actual_sales_PY) %>% 
  mutate(org_vpy_ratio          = org_vpy/actual_sales_PY) %>% 
  mutate(vol_vpy_ratio          = salesvol_vpy/actual_sales_PY) %>% 
  mutate(price_vpy_ratio        = price_vpy/actual_sales_PY) %>% 

  # VFcst Ratios
  mutate(sales_vfcst_ratio      = round(sales_vpy_ratio * 10000, -1) - round(((sales_vpy - sales_vfcst)/actual_sales_PY)*10000,-1)) %>% 
  mutate(fx_vfcst_ratio         = round(fx_vpy_ratio  * 10000, -1)   - round(((salesfx_vpy - salesfx_vfcst)/actual_sales_PY)*10000,-1)) %>% 
  mutate(salesacqdiv_vfcst_ratio= round(salesacqdiv_vpy_ratio  * 10000, -1)   - round(((salesacqdiv_vpy - salesacqdiv_vfcst)/fcst_sales_qr)*10000,-1)) %>% 
  mutate(org_vfcst_ratio        = round(org_vpy_ratio  * 10000, -1)   - round(((org_vpy - org_vfcst)/actual_sales_PY)*10000,-1)) %>% 
  mutate(vol_vfcst_ratio        = round(vol_vpy_ratio  * 10000, -1)   - round(((salesvol_vpy - salesvol_vfcst)/actual_sales_PY)*10000,-1)) %>% 
  mutate(price_vfcst_ratio      = round(price_vpy_ratio  * 10000, -1)   - round((fcst_price/actual_sales_PY)*10000,-1)) %>% 
  
  # VOP Ratios
  mutate(sales_vop_ratio        = round(sales_vpy_ratio * 10000, -1) - round(((sales_vpy - sales_vop)/actual_sales_PY)*10000,-1)) %>% 
  mutate(fx_vop_ratio           = round(fx_vpy_ratio  * 10000, -1)   - round(((salesfx_vpy - salesfx_vop)/actual_sales_PY)*10000,-1)) %>% 
  mutate(salesacqdiv_vop_ratio  = round(salesacqdiv_vpy_ratio  * 10000, -1)   - round(((salesacqdiv_vpy - salesacqdiv_vop)/actual_sales_PY)*10000,-1)) %>% 
  mutate(org_vop_ratio          = round(org_vpy_ratio  * 10000, -1)   - round(((org_vpy - org_vop)/actual_sales_PY)*10000,-1)) %>% 
  mutate(vol_vop_ratio          = round(vol_vpy_ratio  * 10000, -1)   - round(((salesvol_vpy - salesvol_vop)/actual_sales_PY)*10000,-1)) %>% 
  mutate(price_vop_ratio        = round((price_vpy_ratio)  * 10000, -1)   - round(((price_vpy-price_vop)/actual_sales_PY)*10000,-1)) 

ratios[is.na(ratios)] <- 0  

ratios_table <- ratios %>% 
arrange(priority) %>% 
  select(priority,region_channel,
         observation, period, ref_date, quarter,
         product, contains("ratio")
         )

return(ratios_table)

}




# Summarized KPis ---------------------------------------------------------



summarized_kpis <- function(){ 
  
  table <-  grouped_ns %>% 
    filter(product == "Total Product", observation == 2022) %>% 
    filter(period %in% c("January","February", "March", "April", "May","June","July")) %>% 
    group_by(priority, region_channel) %>% 
    summarise(sales_vpy = sum(sales_vpy),
              sales_vfcst = sum(sales_vfcst),
              actual_sales_PY = sum(actual_sales_PY)) %>% 
    mutate(sales_vpy_ratio        = sales_vpy/actual_sales_PY) %>% 
    mutate(sales_vfcst_ratio      = round(sales_vpy_ratio * 10000, -1) - round(((sales_vpy - sales_vfcst)/actual_sales_PY)*10000,-1))
    
  
  return(table)
  
}


summarized_kpis <- function(){ 
  
  table <-  grouped_ns %>% 
    filter(product == "Total Product", observation == 2022) %>% 
    filter(period %in% c("January","February", "March", "April", "May","June","July")) %>% 
    group_by(priority, region_channel) %>% 
    summarise(sales_vpy = sum(sales_vpy),
              actual_sales_PY = sum(actual_sales_PY),
              
              .groups = "drop") %>% 
    mutate(sales_vpy_ratio        = sales_vpy/actual_sales_PY) 
  
  return(table)
  
}


# -161.528217131219


# Create table to replicate the ratios at percentage and Bps on the Original V.
# Table should have same structure and contain 18 ratios for VPY, VFcast & VOP.
# Design view for main table and tooltip to hover. 


