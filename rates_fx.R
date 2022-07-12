
# FX rates Sales Impact #

fx      <- "//americas.swk.pri/Apps/Enterprise/Reval/IntRptg/FXRates/Reval/Loaded" 
fx_last <- "//americas.swk.pri/Apps/Enterprise/Reval/IntRptg/FXRates/Reval" 
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
  filter(date >= today() %m-% days(30)) %>% 
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



# Pivot Summaries NoFin Extraction ----------------------------------------

setwd(model)
getwd()

remove_lag = c("CCA incl PR","Colombia","Argentina Commercial",
               "Chile","Peru","LAG SOUTH HQ")

psum <- read.xlsx("pivot_summ_june_close.xlsx") %>% as_tibble() %>% 
        janitor::clean_names() %>% 
        select(month,scenario,entity,business,
               region,market_1,lc,pnl_rates,transactional_fx) %>% 
        filter(grepl("ACTUAL_POST",scenario)) %>% 
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
  relocate(.after = Jun, Jun_Pnl_Rates)




# psum_no_lag %>% openxlsx::write.xlsx(.,"pivot_summ_transf.xlsx")



