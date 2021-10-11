
#  head.Counter

library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)


# Parths ------------------------------------------------------------------

hc_path        <- "S:/North_America/Towson-TOW/FINANCE/WWPTAFIN/COE/SG&A - General Information/Headcount Reports/2021/Corporate Actives List"
consolidations <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Consolidations" 
DA             <- "S:/North_America/Baltimore-BLT/Transformation Office/Admn/Digital Accelerator Reporting"
TO             <- "S:/North_America/Baltimore-BLT/Transformation Office/Admn/TO Reporting"
myDocs         <- "C:/Users/AEG1130/Documents"

# Count heads for DA, IoT and TO 
headCounter    <- function(){

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
           employee_status,ee_number = hr_empl_id, 
           business_unit_desc, gl_cost_center) %>% 
    
    filter(gl_cost_center %in% c("9401500226","9401500225","9401500227","9401500221",
                                 "9401500233",
                                 "9401500230")) %>% 
    
    mutate(cost_center = case_when(
                                   gl_cost_center %in% c("9401500226","9401500225","9401500227","9401500221") ~"DA",
                                   gl_cost_center == "9401500230"~"TO",
                                   gl_cost_center == "9401500233"~"IoT")) %>% 
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



consolid_hc <- consolidation(resources) %>% 
  mutate(date = make_date( year = substr(period,4,7),
                          month = substr(period,0,2))) %>%  
  mutate(year_month = as.yearmon(date, "%Y-%m")) %>% 
  select(-period) %>% 
  mutate(quarter = quarter(date)) %>%
  mutate(quarter = paste0("Q",quarter))


consolid_hc_grouped <- consolid_hc %>% 
  group_by(year_month,cost_center) %>%
  summarise(n = n(), .groups = "drop") %>% 
  mutate_if(is.character, str_trim) %>% 
  pivot_wider(names_from = year_month, values_from = n) %>% 
  replace(is.na(.), 0) 


return(list(consolid_hc = consolid_hc,
            consolid_hc_grouped = consolid_hc_grouped))

}


heads = headCounter()


setwd(myDocs) 



# heads_actuals -----------------------------------------------------------


actuals_cb = heads$consolid_hc %>% 
  filter(year_month == "Sep 2021") %>% 
  select(name, work_location_name , year_month,
         job_code_description, ee_number, cost_center)

adp_report = openxlsx::read.xlsx("adp_report.xlsx") %>% 
  as_tibble %>% 
  janitor::clean_names() %>% 
  filter(earning_code == "Regular") %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>%
  group_by(ee_number) %>% 
  summarise(sept_regular = sum(earnings))


actuals_cb_estim = actuals_cb %>%
  left_join(adp_report, by = "ee_number") %>% 
  mutate(year_estimate = (sept_regular*12))




actuals_cb_estim %>% openxlsx::write.xlsx(.,"heads_cc.xlsx",overwrite = T)




# queries -----------------------------------------------------------------




setwd(DA)



heads %>% 
openxlsx::write.xlsx(.,"headcounter.xlsx", overwrite = T)



# New Heads >>>

all = heads$consolid_hc %>% filter(date != "2021-08-01")
aug = heads$consolid_hc %>% filter(date == "2021-08-01")

aug %>% anti_join(all, by="name")




# # HeadCount Report  -----------------------------------------------------


setwd(consolidations)



actuals_hc = openxlsx::read.xlsx("Consolidated Actuals.xlsx", "CB") %>% as_tibble %>% 
  select(!starts_with("F")) %>% 
  select(!contains("Q")) %>% 
  mutate(Hiring.Status = str_trim(Hiring.Status)) %>% 
  mutate(Hiring.Status = str_to_lower(Hiring.Status)) %>% 
  filter(Hiring.Status %in% c("started - internal transfer", "started - external")) %>% 
  mutate(CC = NA, LOCAL_CC = NA, Reg_Region = NA, Platform = NA, Location_Country = NA,
         Labor_Type = 0, HFM_CO = NA, 'Month of Month Year'= NA,
         Division = "Global Tools & Storage", Employee_Overlap = NA ) %>% 
  select(Employee.ID, Employee.Name, Growth.Initiative,CC, LOCAL_CC, 
         Reg_Region, Division, Platform, Location_Country, Labor_Type,
         Team, HFM_CO, 'Month of Month Year', Employee_Overlap) 



setwd(myDocs)

openxlsx::write.xlsx(actuals_hc,"Headcount Analysis Growth Confirmation.xlsx", asTable = F, overwrite = T)







# ConstrasteR HeadCounting ------------------------------------------------

setwd(TO)


# Actuals -----------------------------------------------------------------

actuals_aug = openxlsx::read.xlsx("Trans Off_August.xlsx") 

# Transformation
actuals_august = actuals_aug %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  filter(cost_element != "9585000") %>% 
  mutate(category =case_when(str_detect(cost_element_name,"EMP BEN")~"C&B",
                             str_detect(cost_element_name,"PR TAXE")~"C&B",
                             str_detect(cost_element_name,"WAGE")~"C&B",
                             str_detect(cost_element_name,"DEMO")~"Demo Tools",
                             str_detect(cost_element_name,"OS FEE LABOR")~"Globant Profesional Fees",
                             str_detect(cost_element_name,"PROMO SPECIAL P")~"Promo Services",
                             str_detect(cost_element_name,"OS FEE RECRUIT")~"Recruiting",
                             str_detect(cost_element_name,"RENT BUILD")~"Rent",
                             str_detect(cost_element_name,"AMORTIZ SOFTW")~"Software Amortization",
                             str_detect(cost_element_name,"MATL PROTO")~"Supplies & Other",
                             str_detect(cost_element_name,"OS FEE LEGAL GEN")~"Supplies & Other",
                             str_detect(cost_element_name,"OTH EXP MISC")~"Supplies & Other",
                             str_detect(cost_element_name,"SUPPLIES")~"Supplies & Other",
                             str_detect(cost_element_name,"T&E")~"T&E",
                             str_detect(cost_element_name,"UTILITY TELEP")~"Telephone",
                             str_detect(cost_element_name,"EMP DEV SHOW EXHIBIT")~"Supplies & Other",
                             str_detect(document_header_text,"BU funded")~"Corporate IT",
                             TRUE ~ as.character(cost_element_name)))

# Pivot 
actuals_august %>%
  group_by(category,cost_element_name) %>%
  summarise(amount = sum(val_in_rep_cur),
            .groups = "drop") %>%
  janitor::adorn_totals()
  


# Comp & Benefits 
aug_cb = actuals_august %>% filter(category == "C&B") 

sum(aug_cb$val_in_rep_cur)


# August Diff Actuals vs 
diff_aug = sum(aug_cb$val_in_rep_cur)-sum(forecasts_august$august)

diff_aug/174729.46 



# Forecasted  -------------------------------------------------------------

setwd(TO)

forecasts_aug = openxlsx::read.xlsx("TO_2021_F07.xlsx") 


forecasts_august = forecasts_aug %>% 
  janitor::clean_names() %>% 
  as_tibble()



forecast_resumen = forecasts_august %>% 
  select(employee_name, employee_id)%>%
  separate(employee_name, c("first", "last"), sep = " ")

# US Headcount  -----------------------------------------------------------

setwd(hc_path)

hc_aug = openxlsx::read.xlsx("US Headcount 08-2021.xlsx", "Actives")

names(hc_aug) <- as.character(hc_aug[1,])

hc_august <- hc_aug %>% 
  janitor::clean_names() %>% 
  slice(-1) %>% 
  as_tibble() %>% 
  filter(gl_cost_center == "9401500230")
  

hc_resumen = hc_august %>% select(name, hr_empl_id)
  
hc_split = hc_resumen %>%
  separate(name, c("last", "first"),sep = ",") %>%
  separate(first, c("first", "second"), sep = " ") %>% 
  select(-second)
  # unite(name, last, first, sep = " ") %>% 
  # mutate()
  


# Intersect ---------------------------------------------------------------


hc_split
forecast_resumen


# In Forecast not in US HeadCount 
missing_names_hc = forecast_resumen %>% 
  select(last)   %>%
  anti_join(hc_split %>% select(last), by = "last") %>% 
  na.omit()


# In US HeadCount not in Forecast 
missing_names_fcst = hc_split %>% 
  # select(last)   %>%
  anti_join(forecast_resumen %>% select(last), by = "last") %>% 
  na.omit()



# Details of Missings -----------------------------------------------------

forecasts_august %>%
  separate(employee_name, c("first","last"),sep = " ") %>%
  filter(last %in% missing_names_hc$last)->a


setwd(myDocs)

payrolls = openxlsx::read.xlsx("ADP_report_TO_aug.xlsx") %>% 
  as_tibble() %>%
  janitor::clean_names() %>% 
  mutate(ee_number = as.character(ee_number))
  


payments = missing_names_fcst %>% 
  left_join(payrolls, by=c("hr_empl_id" = "ee_number")) %>% 
  arrange(desc(earnings))

sum(payments$earnings)


