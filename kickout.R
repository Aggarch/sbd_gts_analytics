
# KICKOUT

library(tidyverse)
library(lubridate)
library(readxl)


kout_fc11    <-	"//americas.swk.pri/Apps/Enterprise/FT/Prod/P-BODSETL/PRD_Kout/C11"
hfm_dmt_maps <- "//americas.swk.pri/apps/Enterprise/FT/Prod/P-BODSETL/PRD_Maps"
mapping_fold <- "C:/Users/AEG1130/Documents/mapping"


# Kickout Files 

superKick <- function(){ 


setwd(kout_fc11)

timespam <- today()%>%
  as_tibble() %>% 
    mutate(value = value -1) %>% 
  mutate(value = as.character(value)) %>% 
  mutate(value = str_remove_all(value,"-")) %>% 
  pull()

current <- today()%>%
  as_tibble() %>% 
  mutate(value = as.character(value)) %>% 
  mutate(value = str_remove_all(value,"-")) %>% 
  pull()

tw <- c(timespam, current)

file <- list.files() %>% as_tibble %>% 
  filter(grepl("NA",value)) %>% 
  filter(grepl("C11",value)) %>% 
  filter(str_detect(value, tw)) %>% 
  mutate(date = substr(value,27,34)) %>% 
  mutate(date = as.numeric(date)) %>% 
  filter(date == max(date)) %>% 
  pull(value)

# Add recursive filter to make this sensitive/Recursive to book? 
# PCA = P&L /// FISL = BSheet 

target <- read.csv(file) %>% as_tibble

setwd(mapping_fold)

target %>% openxlsx::write.xlsx("last_kick.xlsx", overwrite = T)

KICK <- target %>% select(BAR_FUNCTION, DI_ERRORCOLUMNS, BAR_ENTITY, ACCT,AMT, BAR_ACCT) %>% 
  mutate(ACCT = as.character(ACCT),
         AMT  = as.numeric(AMT)) %>% 
  mutate(BAR_ENT = str_replace_all(BAR_ENTITY, "E",""))

total_kick <- sum(KICK$AMT)


# Connect to the FDM File find ACCT in Src Acct to pick HFM Acct
# Open HFM DMT Files and search for the HFM account. 


############ FDM File 


# Make sure the FDM Files are readable;  

  
setwd(mapping_fold)

fdm_dos <- openxlsx::read.xlsx("WWPT_SAP_NA_US_2_Actual_C11.xlsx") %>%
  as_tibble() %>% mutate(version = "fdm_dos")

fdm_one <- openxlsx::read.xlsx("WWPT_SAP_NA_US_Actual_C11.xlsx") %>% 
  as_tibble() %>% mutate(version = "fdm_one")


fdm <- fdm_one %>% bind_rows(fdm_dos) %>% janitor::clean_names()

# Entities of Interest: 

fdm_focus <- fdm %>%
  filter(entity %in% KICK$BAR_ENT) %>% 
  filter(source_account %in% KICK$ACCT) 


############# HFM DMT ::: 

HFM_rec <- list.files() %>%
  as_tibble() %>% 
  filter(value =="HFM_DMT.xlsx") %>% 
  mutate(sheet = map(.$value, excel_sheets)) %>% 
  unnest(cols = c(sheet)) %>% 
  mutate(dimension = str_sub(sheet,start = 9L, end = 30))


reader <- function(value, sheet,dimension){ 
  
data <- openxlsx::read.xlsx(value,sheet) %>% 
  as_tibble() %>% mutate(dimension = dimension)

return(data)

}

hfm_tables <- HFM_rec %>% pmap(reader)
  
hfm_account <- hfm_tables[[3]]

hfm_function <- hfm_tables[[5]]


function_hfm <- hfm_function %>% filter(HFM_C1 %in% fdm_focus$custom1)


# Finding Match in HFM_DMT Maps::: 
fdm_source_account <- fdm_focus %>%
  filter(account %in% hfm_account$HFM_ACCOUNT) %>% 
  select(entity, account, source_account, custom1) %>%
  distinct() %>% 
  left_join(function_hfm %>% select(HFM_C1, BAR_FUNCTION),
            by = c("custom1" = "HFM_C1")) %>% 
  rename(HFM_C1 = custom1) %>% 
  mutate(source_account = paste0("000",source_account))


HFM <- hfm_account %>% filter(HFM_ACCOUNT %in% fdm_focus$account) %>% 
  select(-RUNID,-LOADDATETIME, -JOBNAME, -BAR_FUNCTION)



#ACN/DMT input


DMT_DETAILED <- HFM %>% left_join(fdm_source_account,
                  by =c("HFM_ACCOUNT" = "account"))


DMT_INSERT   <- DMT_DETAILED %>%
  select(ACCT = source_account,
         HFM_ENTITY = entity,
         HFM_ACCOUNT, 
         BAR_ACCOUNT,
         HFM_C1, 
         BAR_FUNCTION) %>% 
  left_join(KICK %>%
              select(BAR_ENT, BAR_ENTITY),
            by = c("HFM_ENTITY" = "BAR_ENT")) %>% 
  distinct() %>% 
  relocate(.before = HFM_ACCOUNT, BAR_ENTITY)


print(paste("DESTINY TABLE: ", "EPM_C11_ACCOUNT_FUNC_LKP_NA+"))


return(list(TO_BE_KICKOUT = KICK,
            TOTAL_TO_KICK = total_kick,
            DMT_ACN_INPUT = DMT_INSERT))



}


kickOut <- superKick()

# Open DMT 
# Citrix Script to run given a specific target. 

