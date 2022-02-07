# kickouts 

library(tidyverse)
library(lubridate)


kout_fc11    <-	"//americas.swk.pri/Apps/Enterprise/FT/Prod/P-BODSETL/PRD_Kout/C11"
hfm_dmt_maps <- "//americas.swk.pri/apps/Enterprise/FT/Prod/P-BODSETL/PRD_Maps"



setwd(kout_fc11)


timespam <- today()%>%
  as_tibble() %>% 
    mutate(value = value -1) %>% 
  mutate(value = as.character(value)) %>% 
  mutate(value = str_remove_all(value,"-")) %>% 
  pull()



file <- list.files() %>% as_tibble %>% 
  filter(grepl("NA",value)) %>% 
  filter(grepl("C11",value)) %>% 
  filter(grepl(timespam, value)) %>% 
  pull()

target <- read.csv(file) %>% as_tibble


target %>% select(DI_ERRORCOLUMNS, BAR_ENTITY, ACCT)


# Connect to the FDM File find ACCT in Src Acct to pick HFM Acct
# Open HFM DMT Files and search for the HFM account. 



# Open DMT 

setwd("//americas.swk.pri/apps/Enterprise/FT/Prod/P-BODSETL/PRD_DMT/2013")

example <- read_excel("ACN Data Construction Tool.xlsm", sheet = "test")

