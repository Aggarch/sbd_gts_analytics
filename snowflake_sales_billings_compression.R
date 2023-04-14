
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readr)

# Data sample for Sales and Billings correspond to FEB 2023 

organics_data         <- "C:/Users/AEG1130/Documents/Reconcilation"

setwd(organics_data)



# Formation ---------------------------------------------------------------


# SALES 


data_global = read_csv("SBU_GLOBAL.csv")


data = data_global %>% 
  as_tibble %>% 
  janitor::clean_names()


# saveRDS(data, "data_global")


global_sales = read_rds("data_global")


global_sales

cols = global_sales %>% colnames %>%  as_tibble()



# BILLING

data_global_billing = read_csv("SBU_GLOBAL_BILLING.csv")


data_billing = data_global_billing %>% 
  as_tibble %>% 
  janitor::clean_names()


#saveRDS(data_billing, "data_global_billing")

global_billing = read_rds("data_global_billing")





# Execution ---------------------------------------------------------------


global_sales = read_rds("data_global")
global_billing = read_rds("data_global_billing")


global_sales
global_billing



# ColumNames

global_billing %>% colnames
global_sales %>% colnames



# VisoR

global_billing %>% select(contains("prod")) %>% View
global_sales %>% select(contains("prod")) %>% View


# Wrangling ---------------------------------------------------------------

# Replace all Zeros, select grouping variables and summmarize all numeric values,
# Export Results to different sheets to compare. 
# Sales table do has 1,565 more Id-s than the Billing table. 



billing <- global_billing %>% 
  mutate_if(is.numeric,~replace(.,is.na(.),0)) %>% 
  mutate(sls_ord_ln_nbr = as.character(sls_ord_ln_nbr),
         fmth_id = as.character(fmth_id),
         fqtr_id = as.character(fqtr_id),
         fwk_id  = as.character(fwk_id),
         fmth_nbr  = as.character(fmth_nbr),
         fyr_id  = as.character(fyr_id),
         clndr_mth_nbr  = as.character(clndr_mth_nbr),
         clndr_yr_id  = as.character(clndr_yr_id)) %>% 
  group_by(gpp_sbu_desc, brand_desc, 
           gpp_division_desc, prod_hier_lvl_5_id) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)


sales <- global_sales %>% 
  mutate_if(is.numeric,~replace(.,is.na(.),0)) %>% 
  mutate(fiscal_year = as.character(fiscal_year),
         fiscal_qtr = as.character(fiscal_month),
         fiscal_month = as.character(fiscal_month_id),
         npd_eanz_project_number = as.character(npd_eanz_project_number),
         fiscal_month_id  = as.character(fiscal_month_id)) %>%
  group_by(gpp_sbu_descr, product_brand,
           gpp_division_descr, product_sap_sub_group_code) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)

# Export
billing %>% openxlsx::write.xlsx(.,"billing_data.xlsx", overwrite = T)
sales   %>% openxlsx::write.xlsx(.,"sales_data.xlsx", overwrite = T)


# Read Summary ------------------------------------------------------------

sales = openxlsx::read.xlsx("sales_data.xlsx") %>% as_tibble() %>% 
  mutate_if(is.character, stringr::str_to_upper) %>% 
  

billing = openxlsx::read.xlsx("billing_data.xlsx") %>% as_tibble() %>% 
  mutate_if(is.character, stringr::str_to_upper)



# COMMON PROD CODES -------------------------------------------------------


# 4788 Unique Records. 
sids = sales   %>% 
  distinct(product_sap_sub_group_code) %>% 
  rename(prod_code = product_sap_sub_group_code)

# 3223 Unique Records 
bids = billing %>% 
  distinct(prod_hier_lvl_5_id) %>% 
  rename(prod_code = prod_hier_lvl_5_id)


# COMMON PROD CODES ::: 
common_prod_codes = sids %>%
  inner_join(bids, by = "prod_code") %>% 
  filter(!grepl("9999", prod_code))



# CHECKS ------------------------------------------------------------------


code_check <- function(){ 
  
  
bills = billing %>% 
  group_by(prod_hier_lvl_5_id, brand_desc) %>% 
  filter(prod_hier_lvl_5_id %in% c(common_prod_codes$prod_code)) %>% 
  select(brand_desc,prod_hier_lvl_5_id,dispatch_dcrncy_amt_usd,cost_dcrncy_amt_usd)%>%
  summarise(dispatch_dcrncy_amt_usd = sum(dispatch_dcrncy_amt_usd),
            cost_dcrncy_amt_usd = sum(cost_dcrncy_amt_usd),
            .groups = "drop") %>% 
  distinct() %>% 
  mutate(type = "bills") %>% 
  rename(prod_code = prod_hier_lvl_5_id,
         net_ship_dispatch = dispatch_dcrncy_amt_usd,
         cos_total = cost_dcrncy_amt_usd) 


saless = sales %>% 
  group_by(product_sap_sub_group_code, product_brand) %>% 
  filter(product_sap_sub_group_code %in% c(common_prod_codes$prod_code)) %>% 
  # filter(product_sap_sub_group_code == code) %>% 
  select(product_brand,product_sap_sub_group_code,net_ship_gsv_w_o_rsa, cos_total) %>% 
  summarise(net_ship_gsv_w_o_rsa = sum(net_ship_gsv_w_o_rsa),
            cos_total = sum(cos_total),
            .groups = "drop") %>% 
  distinct() %>% 
  mutate(type = "sales") %>% 
  rename(prod_code = product_sap_sub_group_code,
         brand_desc = product_brand,
         net_ship_dispatch = net_ship_gsv_w_o_rsa)



reconc <- bills %>%
  rbind(saless) %>%
  pivot_wider(names_from = type,
              values_from = c("net_ship_dispatch", "cos_total")) %>%
  mutate(delta_net_ship_dispatch = (net_ship_dispatch_bills - net_ship_dispatch_sales)) %>%
  mutate(delta_cos_total         = (cos_total_bills -cos_total_sales)) %>% 
  mutate_if(is.numeric,~replace(.,is.na(.),0))%>%
  arrange(desc(delta_cos_total)) %>%
  mutate(delta_total = delta_cos_total + delta_net_ship_dispatch) %>% 
  mutate(PMt = ifelse(delta_total == 0, 1,0))



return(reconc)


}




# CODE CHECK ITERATION ----------------------------------------------------

# Iterate the code check across all common Prod Codes. 

result = map(common_prod_codes$prod_code, code_check)





# The Mayority of the IDs for Sub-Group Code on Billin table do exist on the Sales table
# Approx 95% of this Ids in the billing table are on the other, 

sids$product_sap_sub_group_code %in% c(bids$prod_hier_lvl_5_id) %>% 
  as_tibble() %>% count(value) %>% janitor::adorn_totals()



bids$prod_hier_lvl_5_id %in% c(sids$product_sap_sub_group_code) %>% 
  as_tibble() %>% count(value) %>% janitor::adorn_totals()



sids


