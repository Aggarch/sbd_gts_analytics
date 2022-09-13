
# FX rates Sales Impact #

fx      <- "//americas.swk.pri/Apps/Enterprise/Reval/IntRptg/FXRates/Reval/Loaded" 
fx_last <- "//americas.swk.pri/Apps/Enterprise/Reval/IntRptg/FXRates/Reval" 
reconc     <- "C:/Users/AEG1130/Documents/Reconcilation" 

model   <- "C:/Users/AEG1130/Documents/fx"   
docs    <- "C:/Users/AEG1130/Documents" 

library(tidyverse)
library(openxlsx)



# SBD Treasury Rates Wrangling --------------------------------------------



# Rates & Keep Bzns Days

SBD_FX         <- function(){ 

# Extraction funciton ::::::::::::::::::::::

rates_table <- function(path){ 
  

rates<-function(path){

setwd(path)

rate_files <- list.files() %>% as_tibble() %>% 
  filter(grepl("ReVal_DailyRates_", value)) %>% 
  rename(rates = value) %>% 
  janitor::clean_names() %>% 
  mutate(dates = substr(rates, 18,27))

return(rate_files)

}


rate_files <- rates(path) 




# Bzns Calendar 

setwd(docs)

fcalend <- openxlsx::read.xlsx("fx/fiscal_calendar.xlsx") %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  select(!contains("left"),-report_date,-week_2) %>% 
  mutate(data_date = as.Date(data_date, origin = "1899-12-30")) %>% 
  mutate(dates = as.character(data_date)) %>% 
  select(-data_date)

# Fiscal Monthly Close Ranges: 
close_ranges<-fcalend %>%
      group_by(month) %>%
      summarise(idate = min(dates),
                fdate = max(dates)) %>% 
  mutate(idate = as.Date(idate, origin="1899-12-30")) %>% 
  arrange(idate)

# Resources

resource <- rate_files %>%
  left_join(fcalend, by = "dates") %>% 
  filter(!is.na(hfm_day))


# Extractor

setwd(path)

f <- function(rates){ 

files <- read.csv(rates) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(from_ccy,date,inverse_spot_rate) %>%  
  left_join(resource %>% select(dates,day,week,month),
            by = c("date"="dates"))

  
  
  return(files)
  
}


# Ideal structure calculating 

pull_long <- map(resource$rates,f) %>% 
  map_dfr(., bind_rows) %>% 
  mutate(year = substr(date,0,4))

last_info_date<- pull_long %>%
  mutate(date = as.Date(date,
                        origin = "1899-12-30")) %>%
  filter(date == max(date)) %>% distinct(date)


return(list(table = pull_long,
            last_date = last_info_date))


}


# Sources Unification ::::::::::::::::::::::
table <- rates_table(fx)
last  <- rates_table(fx_last) 


# Merged Cleaned Data (FX RATES) :::::::::::
# Equivalent to the daily_rates sheet on SBD_Rates.xlsx dwl3


 pull_long <- table$table %>%
   bind_rows(last$table) %>% 
   distinct()

#pull_long<-table


return(pull_long)

}

pull_long      <- SBD_FX()




# Original Perspective, wider pivot SBD_Rates sheet
# With Excel Output

SBD_Rates_original <- function(){ 
  
# Wider Pivot ::::::::::::::::::::::::::::::

  library(lubridate)  
  
wider_pull <-
  # pull_long$table %>%
  pull_long %>% 
 # filter(date >= today() %m-% days(60)) %>% 
  select(from_ccy, date, inverse_spot_rate) %>% 
  pivot_wider(names_from = date, values_from = inverse_spot_rate)


# Verify desire order - - - - -- - -- -- - - -
# FX Construct 

# model   <- "C:/Users/AEG1130/Documents/fx"   
# setwd(model)
# 
# order.cur <- openxlsx::read.xlsx("GTS_FX_2022_FEB28_SPOT_03022022@7PM.xlsx",
#                                  sheet = "SBD Rates 2022")
# 
# struct <- order.cur %>% as_tibble() %>% select(X2,X3) %>% filter(!is.na(X2)) %>% 
#   rename(from_ccy = X2,
#          currency = X3)


#wider_pull <- struct %>% left_join(wider_pull, by = "from_ccy")


# Save Wider Pivot - - - - - - - - - - - - - -

setwd(model)

wider_pull %>% openxlsx::write.xlsx("SBD_Rates_original.xlsx", overwrite = T)

return(wider_pull)

}


# Querying, to be able plotting :::::::::::::::::::::::::::::::::::::::::::
# Examples time windows : Excel output


SBD_Rates <- function(){ 

  wb <- createWorkbook()
  addWorksheet(wb,"daily_data")
  addWorksheet(wb,"weekly_rates")
  addWorksheet(wb,"monthly_rates")
  
 daily_data <- pull_long  
  
  weekly_rates <- daily_data %>% 
      mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
      group_by(from_ccy, week, month,year) %>%
      summarise(n_periods_per_week = n(),
                last_date = max(date),
                first_date = min(date),
                average = sum(inverse_spot_rate)/n_periods_per_week,
                .groups = "drop") %>% 
      mutate(date_range = paste(first_date ,":", last_date)) %>% 
      mutate(week = str_replace_all(week,"Week","")) %>% 
      mutate(week = str_trim(week)) %>% 
      mutate(week = as.numeric(week)) %>% 
      arrange(week) %>% 
      select(-last_date, -first_date)


 monthly_rates <- daily_data %>% 
      mutate(date = as.Date(date, origin = "1899-12-30")) %>%
        group_by(from_ccy,month) %>% summarise(n_periods = n(),
                                               idate = min(date),
                                               fdate = max(date),
                                               iweek = first(week),
                                               fweek = last(week),
                                               avg = sum(inverse_spot_rate)/n_periods,
                                               .groups = "drop") %>% 
      mutate(date_range = paste(idate,":",fdate)) %>% 
      mutate(week_range = paste(iweek,":",fweek)) %>% 
      mutate(month.n = lubridate::month(idate)) %>% 
        select(-idate, -fdate,
               -iweek, -fweek) %>% 
        arrange(month.n)
    


  writeData(wb, "daily_data", daily_data)
  writeData(wb, "weekly_rates", weekly_rates)
  writeData(wb, "monthly_rates", monthly_rates)

  
  openxlsx::saveWorkbook(wb, file = "SBD_Rates.xlsx", overwrite = TRUE)
  

return(list(daily_rates   =  daily_data,
            weekly_rates  =  weekly_rates,
            monthly_rates =  monthly_rates))

}



# BAR Scenarios For FX Impact Measurement ---------------------------------


getwd()

setwd("C:/Users/AEG1130/Documents")

transact <- openxlsx::read.xlsx("fx_trans.xlsx")



# Pivot Summaries Actuals NoFin Extraction ----------------------------------------

setwd(model)
getwd()

library(tidyverse)
library(openxlsx)

# pivot_summ_last <- openxlsx::read.xlsx("TOOLS_FX_2022_JUL_CLOSE.xlsx", sheet = "Pivot Summaries") %>% 
#   as_tibble()


remove_lag = c("CCA incl PR","Colombia","Argentina Commercial",
               "Chile","Peru","LAG SOUTH HQ")

psum <- read.xlsx("pivot_summ_june_close.xlsx") %>% as_tibble() %>% 
        janitor::clean_names() %>% 
        select(month,scenario,entity,business,
               region,market_1,lc,pnl_rates,transactional_fx) %>% 
        filter(grepl("ACTUAL",scenario)) %>% 
  filter(!grepl("OPG",region)) %>% 
  filter(!grepl("DORMANT",region)) %>% 
  filter(entity != "[none]") %>% 
  mutate_all(funs(replace(., is.na(.), 0)))


pnl_rates <- psum %>% select(month,lc,pnl_rates) %>% 
  distinct() %>% 
  pivot_wider(names_from = month, values_from = "pnl_rates") %>% 
  rename_at(vars(-lc), ~ paste0(., '_Pnl_Rates'))


psum %>% group_by(month) %>% 
  summarise(transfx = sum(transactional_fx)) %>%
  janitor::adorn_totals()

psum_wider <- psum %>%
  select(-pnl_rates) %>% 
  pivot_wider(names_from = month, 
              values_from = transactional_fx) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  # mutate(pnl_rates = as.character(pnl_rates)) %>% 
  mutate(all_period_sum = rowSums(across(where(is.numeric)))) %>% 
  filter(all_period_sum != 0) %>% 
  select(-all_period_sum)
  

# Remove LAG entities that run AVG cost,
# TOOLS FX FACTOR LAG , OCOS == 0


psum_no_lag <- psum_wider %>% 
  #filter(!market_1 %in% remove_lag)
  mutate(LAG_ex = ifelse(market_1 %in% remove_lag, "yes", "no")) %>% 
  left_join(pnl_rates, by = "lc") %>% 
  relocate(.after = Jan, Jan_Pnl_Rates) %>% 
  relocate(.after = Feb, Feb_Pnl_Rates) %>% 
  relocate(.after = Mar, Mar_Pnl_Rates) %>% 
  relocate(.after = Apr, Apr_Pnl_Rates) %>% 
  relocate(.after = May, May_Pnl_Rates) %>% 
  relocate(.after = Jun, Jun_Pnl_Rates) %>% 
  relocate(.after = Jul, Jul_Pnl_Rates) %>% 
  relocate(.after = Aug, Aug_Pnl_Rates)


# psum_no_lag %>% openxlsx::write.xlsx(.,"pivot_summ_transf.xlsx")

# OCOS Transactional Carve Out OPG 

vect_math <- read.xlsx("pivot_summ_transf.xlsx", sheet = "Dist") %>% as_tibble()


# FACTORS ------------------------------------------------------------------

fx_factor_ocos <- read.xlsx("fx_factor_sgm.xlsx") %>% as_tibble() %>% 
  filter(region_market %in% vect_math$Reg) %>% 
  rename(business_market = region_market) %>% 
  rename_at(vars(-business_market), ~ paste0(., '_ocos_dist'))

transFX_ocos <- psum_no_lag %>%
  left_join(fx_factor_ocos, by = c("business" = "business_market")) %>% 
  left_join(fx_factor_ocos, by = c("market_1" = "business_market")) 

transFX <- function(){ 

transFX_ocos_dist <- transFX_ocos %>% select(entity, contains("ocos_dist")) %>% 
  unite("Jan_ocos_dist",Jan_ocos_dist.x,Jan_ocos_dist.y) %>% 
  unite("Feb_ocos_dist",Feb_ocos_dist.x,Feb_ocos_dist.y) %>%  
  unite("Mar_ocos_dist",Mar_ocos_dist.x,Mar_ocos_dist.y) %>% 
  unite("Apr_ocos_dist",Apr_ocos_dist.x,Apr_ocos_dist.y) %>% 
  unite("May_ocos_dist",May_ocos_dist.x,May_ocos_dist.y) %>% 
  unite("Jun_ocos_dist",Jun_ocos_dist.x,Jun_ocos_dist.y) %>% 
  unite("Jul_ocos_dist",Jul_ocos_dist.x,Jul_ocos_dist.y) %>% 
  unite("Aug_ocos_dist",Aug_ocos_dist.x,Aug_ocos_dist.y) %>% 
  
  

  mutate(Jan_ocos_dist = str_replace_all(Jan_ocos_dist,"NA_","")) %>% 
  mutate(Jan_ocos_dist = str_replace_all(Jan_ocos_dist,"_NA","")) %>% 
  
  mutate(Feb_ocos_dist = str_replace_all(Feb_ocos_dist,"NA_","")) %>% 
  mutate(Feb_ocos_dist = str_replace_all(Feb_ocos_dist,"_NA","")) %>% 
  
  mutate(Mar_ocos_dist = str_replace_all(Mar_ocos_dist,"NA_","")) %>% 
  mutate(Mar_ocos_dist = str_replace_all(Mar_ocos_dist,"_NA","")) %>% 
  
  mutate(Apr_ocos_dist = str_replace_all(Apr_ocos_dist,"NA_","")) %>% 
  mutate(Apr_ocos_dist = str_replace_all(Apr_ocos_dist,"_NA","")) %>% 
  
  mutate(May_ocos_dist = str_replace_all(May_ocos_dist,"NA_","")) %>% 
  mutate(May_ocos_dist = str_replace_all(May_ocos_dist,"_NA","")) %>% 
  
  mutate(Jun_ocos_dist = str_replace_all(Jun_ocos_dist,"NA_","")) %>% 
  mutate(Jun_ocos_dist = str_replace_all(Jun_ocos_dist,"_NA","")) %>% 
    
  mutate(Jul_ocos_dist = str_replace_all(Jul_ocos_dist,"NA_","")) %>% 
  mutate(Jul_ocos_dist = str_replace_all(Jul_ocos_dist,"_NA","")) %>% 
  
  mutate(Aug_ocos_dist = str_replace_all(Aug_ocos_dist,"NA_","")) %>% 
  mutate(Aug_ocos_dist = str_replace_all(Aug_ocos_dist,"_NA","")) %>% 
  
    separate(Jan_ocos_dist, c("Jan_ocos_dist", "b"), "_") %>%
    separate(Feb_ocos_dist, c("Feb_ocos_dist", "b"), "_") %>%
    separate(Mar_ocos_dist, c("Mar_ocos_dist", "b"), "_") %>%
    separate(Apr_ocos_dist, c("Apr_ocos_dist", "b"), "_") %>%
    separate(May_ocos_dist, c("May_ocos_dist", "b"), "_") %>%
    separate(Jun_ocos_dist, c("Jun_ocos_dist", "b"), "_") %>% 
    separate(Jul_ocos_dist, c("Jul_ocos_dist", "b"), "_") %>% 
    separate(Aug_ocos_dist, c("Aug_ocos_dist", "b"), "_")  

    
   
TransFX_OCOS_YTD <- psum_no_lag %>% left_join(transFX_ocos_dist, by = "entity") %>% 
  relocate(.after = Jan, Jan_ocos_dist) %>% 
  relocate(.after = Feb, Feb_ocos_dist) %>% 
  relocate(.after = Mar, Mar_ocos_dist) %>% 
  relocate(.after = Apr, Apr_ocos_dist) %>% 
  relocate(.after = May, May_ocos_dist) %>% 
  relocate(.after = Jun, Jun_ocos_dist) %>% 
  relocate(.after = Jul, Jul_ocos_dist) %>% 
  relocate(.after = Aug, Aug_ocos_dist)  




return(TransFX_OCOS_YTD)

}

TransFX_OCOS_YTD <- transFX()



# SGM Transactional -------------------------------------------------------

# Change the Factor table to be SGM distribution.

TransFX_OCOS_YTD %>% select(!contains(".x")) %>% select(!contains(".y")) %>% 
  select(entity, business, region, market_1, lc, contains("Aug")) %>% 
  mutate(Aug_ocos_dist = as.numeric(Aug_ocos_dist)) %>% 
  mutate(a = Aug * Aug_ocos_dist) %>% 
  filter(a != 0) %>% 
  mutate(b = Aug_Pnl_Rates * a) -> ttt

sgm_transact <- ttt %>% select(entity,a)




setwd(model)
TransFX_OCOS_YTD %>%  openxlsx::write.xlsx(.,"TransFX_OCOS_YTD_AUG.xlsx") 
  


# LH INP01 Template BAR Upload  -------------------------------------------

# Calculate measures.

TransFX_OCOS_YTD <- openxlsx::read.xlsx("Transactional_OCOS_FX/TransFX_OCOS_YTD_AUG.xlsx",sheet = "data") %>% 
  select(entity, OCOS_TRANSFX_LC_AUG)


OCOS_Transactional_FX_INP01 <- openxlsx::read.xlsx("OCOS_Transactional_FX_INP01.xlsx") %>% 
  select(LocalCur) %>% mutate(entity = str_replace_all(LocalCur,"<<",""))

# For Laura Hamblin template /// OCOS Transactional
LH_del <- OCOS_Transactional_FX_INP01 %>% 
  left_join(TransFX_OCOS_YTD, by = "entity") %>% 
  mutate_all(funs(replace(., is.na(.), 0)))


# For Laura Hamblin template /// SGM Transactional
LH_del <- OCOS_Transactional_FX_INP01 %>% 
  left_join(sgm_transact, by = "entity") %>% 
  mutate_all(funs(replace(., is.na(.), 0)))

LH_del %>% openxlsx::write.xlsx(.,"Transactional_OCOS_FX/LH_del.xlsx", overwrite = T)

# On template not on data pull 
OCOS_Transactional_FX_INP01 %>% anti_join(TransFX_OCOS_YTD, by = "entity")

# On data pull not on template 
TransFX_OCOS_YTD %>% anti_join(OCOS_Transactional_FX_INP01, by = "entity")


# Looking for FX accounts -------------------------------------------------


acct <- function(account){
  
  tables %>%
    filter(dimension == "Account") %>%
    filter(grepl("FX",structure)) %>%
    filter(grepl(account,structure)) %>% select(dimension, sub_dimension, name, description)
}

# Ents Update -------------------------------------------------------------


setwd(model)
fx_ent <- openxlsx::read.xlsx("fx_ents.xlsx") %>% as_tibble() %>% 
  janitor::clean_names()


setwd(reconc)
ent_all <- openxlsx::read.xlsx("ent_all_aug22.xlsx") %>%
  as_tibble() %>% 
select(entity,Value)


ttt <- ent_all %>% mutate(ttt = entity %in% fx_ent$entity)


ent_all %>% anti_join(fx_ent, by = "entity")
fx_ent %>% anti_join(ent_all, by = "entity")


check_ent <- fx_ent %>% 
  left_join(ent_all, by = "entity") %>% 
  mutate(diff_check = sales_usd_impact - Value) %>% 
  mutate_all(funs(replace(., is.na(.), 0)))


# Pivot Summaries F07 TransFX Extraction ----------------------------------------

setwd(model)
getwd()


remove_lag = c("CCA incl PR","Colombia","Argentina Commercial",
               "Chile","Peru","LAG SOUTH HQ")

psum <- read.xlsx("pivot_summ_f07_close.xlsx") %>% as_tibble() %>% 
  janitor::clean_names() %>% 
  select(month,scenario,entity,business,
         region,market_1,lc,pnl_rates,transactional_fx) %>% 
  filter(grepl("CFCST",scenario)) %>% 
  filter(!grepl("OPG",region)) %>% 
  filter(!grepl("DORMANT",region)) %>% 
  filter(entity != "[none]") %>% 
  mutate_all(funs(replace(., is.na(.), 0)))


pnl_rates <- psum %>% select(month,lc,pnl_rates) %>% 
  distinct() %>% 
  pivot_wider(names_from = month, values_from = "pnl_rates") %>% 
  rename_at(vars(-lc), ~ paste0(., '_Pnl_Rates'))


psum %>% group_by(month) %>% 
  summarise(transfx = sum(transactional_fx)) %>%
  janitor::adorn_totals()

psum_wider <- psum %>%
  select(-pnl_rates) %>% 
  pivot_wider(names_from = month, 
              values_from = transactional_fx) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  # mutate(pnl_rates = as.character(pnl_rates)) %>% 
  mutate(all_period_sum = rowSums(across(where(is.numeric)))) %>% 
  filter(all_period_sum != 0) %>% 
  select(-all_period_sum)


# Remove LAG entities that run AVG cost,
# TOOLS FX FACTOR LAG , OCOS == 0


psum_no_lag <- psum_wider %>% 
  #filter(!market_1 %in% remove_lag)
  mutate(LAG_ex = ifelse(market_1 %in% remove_lag, "yes", "no")) %>% 
  left_join(pnl_rates, by = "lc") %>% 
  relocate(.after = Jul, Jul_Pnl_Rates) %>% 
  relocate(.after = Aug, Aug_Pnl_Rates) %>% 
  relocate(.after = Sep, Sep_Pnl_Rates) %>% 
  relocate(.after = Oct, Oct_Pnl_Rates) %>% 
  relocate(.after = Nov, Nov_Pnl_Rates) %>% 
  relocate(.after = Dec, Dec_Pnl_Rates)


# psum_no_lag %>% openxlsx::write.xlsx(.,"pivot_summ_transf.xlsx")

# OCOS Transactional Carve Out OPG 

vect_math <- read.xlsx("pivot_summ_transf.xlsx", sheet = "Dist") %>% as_tibble()


fx_factor_ocos <- read.xlsx("fx_factor_ocos.xlsx") %>% as_tibble() %>% 
  filter(region_market %in% vect_math$Reg) %>% 
  rename(business_market = region_market) %>% 
  rename_at(vars(-business_market), ~ paste0(., '_ocos_dist')) %>% 
  select(business_market,contains(c("Jul","Aug","Sep","Oct","Nov","Dec")))

transFX_ocos <- psum_no_lag %>%
  left_join(fx_factor_ocos, by = c("business" = "business_market")) %>% 
  left_join(fx_factor_ocos, by = c("market_1" = "business_market"))

transFX <- function(){ 
  
  transFX_ocos_dist <- transFX_ocos %>% select(entity, contains("ocos_dist")) %>% 
    unite("Jul_ocos_dist",Jul_ocos_dist.x,Jul_ocos_dist.y) %>% 
    unite("Aug_ocos_dist",Aug_ocos_dist.x,Aug_ocos_dist.y) %>%  
    unite("Sep_ocos_dist",Sep_ocos_dist.x,Sep_ocos_dist.y) %>% 
    unite("Oct_ocos_dist",Oct_ocos_dist.x,Oct_ocos_dist.y) %>% 
    unite("Nov_ocos_dist",Nov_ocos_dist.x,Nov_ocos_dist.y) %>% 
    unite("Dec_ocos_dist",Dec_ocos_dist.x,Dec_ocos_dist.y) %>% 
    
    mutate(Jul_ocos_dist = str_replace_all(Jul_ocos_dist,"NA_","")) %>% 
    mutate(Jul_ocos_dist = str_replace_all(Jul_ocos_dist,"_NA","")) %>% 
    
    mutate(Aug_ocos_dist = str_replace_all(Aug_ocos_dist,"NA_","")) %>% 
    mutate(Aug_ocos_dist = str_replace_all(Aug_ocos_dist,"_NA","")) %>% 
    
    mutate(Sep_ocos_dist = str_replace_all(Sep_ocos_dist,"NA_","")) %>% 
    mutate(Sep_ocos_dist = str_replace_all(Sep_ocos_dist,"_NA","")) %>% 
    
    mutate(Oct_ocos_dist = str_replace_all(Oct_ocos_dist,"NA_","")) %>% 
    mutate(Oct_ocos_dist = str_replace_all(Oct_ocos_dist,"_NA","")) %>% 
    
    mutate(Nov_ocos_dist = str_replace_all(Nov_ocos_dist,"NA_","")) %>% 
    mutate(Nov_ocos_dist = str_replace_all(Nov_ocos_dist,"_NA","")) %>% 
    
    mutate(Dec_ocos_dist = str_replace_all(Dec_ocos_dist,"NA_","")) %>% 
    mutate(Dec_ocos_dist = str_replace_all(Dec_ocos_dist,"_NA","")) %>% 
    
    
    separate(Jul_ocos_dist, c("Jul_ocos_dist", "b"), "_") %>%
    separate(Aug_ocos_dist, c("Aug_ocos_dist", "b"), "_") %>%
    separate(Sep_ocos_dist, c("Sep_ocos_dist", "b"), "_") %>%
    separate(Oct_ocos_dist, c("Oct_ocos_dist", "b"), "_") %>%
    separate(Nov_ocos_dist, c("Nov_ocos_dist", "b"), "_") %>%
    separate(Dec_ocos_dist, c("Dec_ocos_dist", "b"), "_") %>% 
    select(-b)
  
  
  
  TransFX_OCOS_YTD_F07 <- psum_no_lag %>% left_join(transFX_ocos_dist, by = "entity") %>% 
    relocate(.after = Jul, Jul_ocos_dist) %>% 
    relocate(.after = Aug, Aug_ocos_dist) %>% 
    relocate(.after = Sep, Sep_ocos_dist) %>% 
    relocate(.after = Oct, Oct_ocos_dist) %>% 
    relocate(.after = Nov, Nov_ocos_dist) %>% 
    relocate(.after = Dec, Dec_ocos_dist)  
  
  
  retun(TransFX_OCOS_YTD_F07)
  
}

TransFX_OCOS_YTD_F07 <- transFX()


TransFX_OCOS_YTD_F07 %>% openxlsx::write.xlsx(.,"TransFX_OCOS_YTD_F07.xlsx")
