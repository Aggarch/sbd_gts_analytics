
# KICKOUT

library(tidyverse)
library(lubridate)
library(readxl)


kout_fc11    <-	"//americas.swk.pri/Apps/Enterprise/FT/Prod/P-BODSETL/PRD_Kout/C11"
hfm_dmt_maps <- "//americas.swk.pri/apps/Enterprise/FT/Prod/P-BODSETL/PRD_Maps"
mapping_fold <- "C:/Users/AEG1130/Documents/mapping"


# Kickout Files 

kickout <- function(){ 

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

target <- read.csv(file) %>% as_tibble

setwd(mapping_fold)

target %>% openxlsx::write.xlsx("last_kick.xlsx", overwrite = T)

KICK <- target %>% select(BAR_FUNCTION, DI_ERRORCOLUMNS, BAR_ENTITY, ACCT,AMT) %>% 
  mutate(ACCT = as.character(ACCT),
         AMT  = as.numeric(AMT)) %>% 
  mutate(BAR_ENT = str_replace_all(BAR_ENTITY, "E",""))

total_kick <- sum(KICK$AMT)
entities   <- KICK$BAR_ENTITY

print(paste("Total to be Kicked out :", total_kick))

print(entities)

return(KICK)


}

# Connect to the FDM File find ACCT in Src Acct to pick HFM Acct
# Open HFM DMT Files and search for the HFM account. 

kick <- kickout()

############ FDM File 


# Make sure the FDM Files are readable;  
fdm_hfm_dmt <- function(){ 
  
setwd(mapping_fold)

fdm_one <- openxlsx::read.xlsx("WWPT_SAP_NA_US_2_Actual_C11.xlsx") %>%
  as_tibble() %>% mutate(version = "fdm_one")

fdm_dos <- openxlsx::read.xlsx("WWPT_SAP_NA_US_Actual_C11.xlsx") %>% 
  as_tibble() %>% mutate(version = "fdm_dos")


fdm <- fdm_one %>% bind_rows(fdm_one) %>% janitor::clean_names()

# Entities of Interest: 

fdm_focus <- fdm %>%
  filter(entity %in% kick$BAR_ENT) %>% 
  filter(source_account %in% kick$ACCT) 


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



fdm_source_account <- fdm_focus %>% filter(account %in% hfm$HFM_ACCOUNT) %>% 
  select(entity, account, source_account, custom1) %>%
  distinct() %>% 
  left_join(function_hfm %>% select(HFM_C1, BAR_FUNCTION),
            by = c("custom1" = "HFM_C1")) %>% 
  rename(HFM_C1 = custom1) %>% 
  mutate(source_account = paste0("000",source_account)) 



#ACN/DMT input


dmt<- hfm %>% left_join(fdm_source_account,
                  by =c("HFM_ACCOUNT" = "account"))


DMT_INSERT <- dmt %>%
  select(ACCT = entity, account, source_account,
         BAR_ACCOUNT,HFM_C1, BAR_FUNCTION)


return(list(dmt, DMT_INSERT))

}

fdm_hfm_dmt()


# Open DMT 
setwd("//americas.swk.pri/apps/Enterprise/FT/Prod/P-BODSETL/PRD_DMT/2013")

example <- read_excel("ACN Data Construction Tool.xlsm", sheet = "test")

