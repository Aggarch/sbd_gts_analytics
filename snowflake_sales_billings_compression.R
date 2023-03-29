

library(tidyverse)
library(lubridate)
library(readr)

# Data sample for Sales and Billings correspond to FEB 2023 



# Formation ---------------------------------------------------------------


# SALES 

organics_data         <- "C:/Users/AEG1130/Documents/Reconcilation"

setwd(organics_data)

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



# Wrangling ---------------------------------------------------------------


