
# <> Organics Query <>  --------------------------------------------------------


# From Basic Organics structure, {COLLECTION/IMPORTATION}
# Iterative functionality to offer all Ad hocs organics across Regions, . 
# BA&R recursive refreshal as a result of HsGet formula by row. 

# Reference: 
# CFP&A Organic % Analysis (Corporate Ad Hoc). 


library(tidyverse)
library(lubridate)

organics_data         <- "C:/Users/AEG1130/Documents/organics/"

setwd(organics_data)




# Close Update ---------------------------------------------------------------

# ETL

# To execute this step both sources should be BAR refreshed, an additional filter
# was set at the summary lyer to double check the correct twindow. 



# EXECUTE AFTER BAR REFRESHAL 
organics_data_update <- function(){ 

# Structure comming from this file, logic available at organics_struct.R  
# org_hist_upt <- openxlsx::read.xlsx("ref/organics_history.xlsx") %>% 
  
  org_hist_upt <- openxlsx::read.xlsx("ETL/organics_updated.xlsx") %>% 
        as_tibble() %>% 
        mutate(ref_date  = as.Date(ref_date, origin = "1899-12-30")) %>% 
        mutate(exec_day  = as.Date(exec_day, origin = "1899-12-30")) %>% 
        mutate(region = ifelse(is.na(region),"NAm",region)) %>% 
        mutate(timestamp = as.character(timestamp)) %>% 
    
    filter(observation >= 2016) 
    
         # select(-index, -ship_to, -currency, -customer,
         #        -'function', -product, -brand)


  org_curr <- openxlsx::read.xlsx("ETL/organics_current.xlsx") %>%
            as_tibble() %>% 
        mutate(ref_date = as.Date(ref_date, origin = "1899-12-30")) %>% 
        mutate(region = ifelse(is.na(region),"NAm",region)) %>% 
        select(-index, -ship_to, -currency, -customer,
               -'function', -product, -brand) %>% 
       
        relocate(.before = region, geo) %>% 
        mutate(exec_day = today()) %>% 
        mutate(timestamp = timestamp())


  org_updated <- org_hist_upt  %>%
     bind_rows(org_curr) %>% 
    relocate(.before = region, geo)

  
# RESET 
# org_updated <- org_updated %>%
#   anti_join(org_curr,
#             by = c("geo","region","channel",
#                    "type", "observation", "period"))



org_updated %>% openxlsx::write.xlsx(.,"ETL/organics_updated.xlsx", overwrite = T)


return(org_updated)
}



# Transformation & Summarization ---------------------------------------------

# PBI

organics_summary <- function(){ 

# Recursive Table
organics_tidy <- openxlsx::read.xlsx("ETL/organics_updated.xlsx") %>% as_tibble() %>% 
  mutate(ref_date = as.Date(ref_date, origin = "1899-12-30")) %>% 
  filter(ref_date <= rollback(today()%m-%months(1),roll_to_first = T)) %>% 
  mutate(region = ifelse(is.na(region),"NAm",region)) 


# Summary
org_summ <- organics_tidy %>%
  group_by(geo, region, channel,
           type,
           ref_date,observation,month,period,quarter) %>%
  summarise(result = sum(result),.groups = "drop") %>% 
  pivot_wider(names_from = type, values_from = result) %>% 
    rename(forecast_mtd = forecast_10_mtd,
           forecast_qtd = forecast_10_qtd) %>% 
  select(ref_date, observation, month, period, quarter,
         geo, region, channel,
         organic_mtd, sales_actual_mtd, forecast_mtd, OP_mtd, sales_PY_mtd,
         organic_qtd, sales_actual_qtd, forecast_qtd, OP_qtd, sales_PY_qtd,
         ) %>%   

  # org_summ %>% select(ref_date, period, quarter, geo, region, channel,
  #                     sales_PY_mtd, sales_PY_qtd)
  
  mutate(mtd_sales_vfcast = sales_actual_mtd - forecast_mtd,
         mtd_sales_vop    = sales_actual_mtd - OP_mtd,
         mtd_sales_vpy    = sales_actual_mtd - sales_PY_mtd) %>% 
  relocate(.after = forecast_mtd,mtd_sales_vfcast) %>% 
  relocate(.after = OP_mtd, mtd_sales_vop) %>% 
  relocate(.after = sales_PY_mtd, mtd_sales_vpy) %>% 
  
  
  mutate(qtd_sales_vfcast = sales_actual_qtd - forecast_qtd,
         qtd_sales_vop    = sales_actual_qtd - OP_qtd,
         qtd_sales_vpy    = sales_actual_qtd - sales_PY_qtd) %>% 
  relocate(.after = forecast_qtd, qtd_sales_vfcast) %>% 
  relocate(.after = OP_qtd, qtd_sales_vop) %>% 
  relocate(.after = sales_PY_qtd, qtd_sales_vpy)
  


# after wider pivot, total rows= 1,440, there are 10 diff types of observations.
# organize with ui-sec as original. 

ui_sec <- openxlsx::read.xlsx("ref/ui_sec.xlsx") %>% 
  as_tibble() %>% mutate(region = ifelse(is.na(region),"NAm",region))


organics_summ <- left_join(ui_sec, org_summ,
                      by = c("region", "channel")) %>% 
  relocate(.before = region, geo)


organics_summ %>% openxlsx::write.xlsx(.,
                                       "PBI/organics_summ.xlsx",
                                       sheetName = "summary",  overwrite = T)


organics_tidy %>% openxlsx::write.xlsx(.,
                                       "PBI/organics_tidy.xlsx",
                                       sheetName = "detailed", overwrite = T)


return(list(organics_summ = organics_summ,
            organics_tidy = organics_tidy))


}


