
# https://support.microsoft.com/en-us/office/refresh-an-external-data-connection-in-excel-1524175f-777a-48fc-8fc7-c8514b984440


library(tidyverse)
library(lubridate)

#setwd("C:/Users/AEG1130/Stanley Black & Decker/Commercial Sales Operations - Reporting - Global Ometer Output")

setwd("C:/Users/AEG1130/Stanley Black & Decker/T&O Analytics - orderometer")


# NA LAG ------------------------------------------------------------------

data_na <- read.delim("GLOBAL_OMETER_OUTPUT.csv", header = T, sep = "|") %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  select(
    country,super_sbu,gpp_sbu, 
    super_demand_group, shipments, shipments_py,    
    pd_shipments, target) %>% 
  mutate(region = ifelse(country == "US", "NA", country)) %>% 
  mutate(region = ifelse(region == "CAN", "NA", region)) %>% 
  mutate(region = ifelse(country == "LATAM", "LAG", region))




# EMEA ANZ ----------------------------------------------------------------


data_eanz <- openxlsx::read.xlsx("Hybrid Report v2.0.xlsx") %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  rename(country = entity_level_8_renamed) %>% 
  rename(super_demand_group = cust_hier_d_l1_super_holding) %>% 
  rename(super_sbu  = gpp_l1_hp_sbu_renamed_groups) %>% 
  rename(gpp_sbu  = gpp_l2_hp_sub_sbu_renamed) %>% 
  rename(shipments  = shipped_mtd_today) %>% 
  rename(region  = entity_level_6_renamed ) %>% 
  rename(target  = forecast ) %>% 
  select( -entity_level_9_renamed, -gpp_l3_divison_renamed) %>% 
  
# SIMULATED //// 
  
  
  mutate(shipments = as.numeric(shipments)) %>% 
  mutate(target = as.numeric(target)) %>% 
  mutate(shipments_py = shipments*0.7) %>% 
  mutate(pd_shipments = shipments- (shipments*0.03)) %>% 
  replace_na(list(shipments = 0, shipments_py = 0 , pd_shipments = 0, target = 0)) %>% 
  mutate(region = ifelse(region == "AMZ", "ANZ",region)) %>% 
  mutate(region = ifelse(region != "ANZ", "EMEA",region))  
  


# ------------ UNIFY -------------------------------------------------------

global_data <- data_na %>% rbind(data_eanz) %>% 
  mutate(run_date = today()) %>% distinct()


# Write Global Data to combine later day after day. Write on SPoint. 

# Locally locate ::: #setwd("C:/Users/AEG1130/Documents/Global Orderometer")  

setwd("C:/Users/AEG1130/Stanley Black & Decker/T&O Analytics - orderometer")

openxlsx::write.xlsx(global_data, "Global_orderometer.xlsx", overwrite = T)


# NOTE ////

# From EMEA Section we are missing the PD_Shipments and PY_Shipments, 
# This data is being SIMULATED ////

# Will be desirable to get writing access to the Comercial Sharepoint 
# this way we can refresh EMEA Data but also write the consolidated one 
# After this access, Dashboard should be connected directly to the SPoint 
# So we can trigger the unification process and dinamically refresh PBI. 

# Hybrid Data shoul be refreshing Automatically every X # of minutes.
# Veridy the bottom line is moving, today 03/10/2023, Shipments MTD at 65,3M




