
# KICKOUT

library(tidyverse)
library(lubridate)
library(readxl)


kout_fc11    <-	"//americas.swk.pri/Apps/Enterprise/FT/Prod/P-BODSETL/PRD_Kout/C11"
hfm_dmt_maps <- "//americas.swk.pri/apps/Enterprise/FT/Prod/P-BODSETL/PRD_Maps"
KO           <- "C:/Users/AEG1130/Documents/mapping/kickouts"
mapping_fold <- "C:/Users/AEG1130/Documents/mapping"



# Kickout Files 

kickout_account <- function(){ 


setwd(kout_fc11)

file <- list.files() %>% as_tibble %>% 
  filter(grepl("NA",value)) %>% 
  filter(grepl("C11",value)) %>% 
  mutate(date = substr(value,27,34)) %>% 
  mutate(code = substr(value,36,41)) %>% 
  mutate(date = as.numeric(date)) %>% 
  mutate(code = as.numeric(code)) %>% 
  filter(date == max(date)) %>% 
  filter(code == max(code)) %>% 
  pull(value)

# Add recursive filter to make this sensitive/Recursive to book? 
# PCA = P&L /// FISL = BSheet 

target <- read.csv(file) %>% as_tibble

setwd(mapping_fold)


KICK_ACCOUNT <- target %>% select(BAR_FUNCTION, DI_ERRORCOLUMNS, BAR_ENTITY, ACCT,AMT, BAR_ACCT) %>% 
  mutate(ACCT = as.character(ACCT),
         AMT  = as.numeric(AMT)) %>% 
  mutate(BAR_ENT = str_replace_all(BAR_ENTITY, "E","")) %>% 
  filter(grepl("ACCT",DI_ERRORCOLUMNS))

total_kick_account <- sum(KICK_ACCOUNT$AMT)



# Connect to the FDM File find ACCT in Src Acct to pick HFM Acct
# Open HFM DMT Files and search for the HFM account. 


############ FDM File 


# Make sure the FDM Files are readable;  

  
setwd(mapping_fold)

fdm_dos <- openxlsx::read.xlsx("WWPT_SAP_NA_US_2_Actual.xlsx") %>%
  as_tibble() %>% mutate(version = "fdm_dos")

fdm_one <- openxlsx::read.xlsx("WWPT_SAP_NA_US_Actual.xlsx") %>% 
  as_tibble() %>% mutate(version = "fdm_one")


fdm <- fdm_one %>% bind_rows(fdm_dos) %>% janitor::clean_names()

# Entities of Interest: 

fdm_focus <- fdm %>%
  filter(entity %in% KICK_ACCOUNT$BAR_ENT) %>% 
  filter(source_account %in% KICK_ACCOUNT$ACCT) 


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
  left_join(KICK_ACCOUNT %>%
              select(BAR_ENT, BAR_ENTITY),
            by = c("HFM_ENTITY" = "BAR_ENT")) %>% 
  distinct() %>% 
  relocate(.before = HFM_ACCOUNT, BAR_ENTITY)


recom <- paste("DESTINY TABLE: ", "EPM_C11_ACCOUNT_FUNC_LKP_NA+  ,", "IF TOTAL_TO_KICK = 0, NO ACTION NEEDED")


return(list(TO_BE_KICKOUT = KICK_ACCOUNT,
            TOTAL_TO_KICK = total_kick_account,
            NOTES = recom,
            DMT_ACN_INPUT = DMT_INSERT))

}

kickout_entity <- function(){ 
  
  
  setwd(kout_fc11)
  
  file <- list.files() %>% as_tibble %>% 
    filter(grepl("NA",value)) %>% 
    filter(grepl("C11",value)) %>% 
    mutate(date = substr(value,27,34)) %>% 
    mutate(code = substr(value,36,41)) %>% 
    mutate(date = as.numeric(date)) %>% 
    mutate(code = as.numeric(code)) %>% 
    filter(date == max(date)) %>% 
    filter(code == max(code)) %>% 
    pull(value)
  
  # Add recursive filter to make this sensitive/Recursive to book? 
  # PCA = P&L /// FISL = BSheet 
  
  target <- read.csv(file) %>% as_tibble
  
  
  setwd(mapping_fold)
  

  KICK_ENT <- target %>%
    select(BAR_FUNCTION, DI_ERRORCOLUMNS,
           BAR_ENTITY, ACCT,AMT, BAR_ACCT,
           COCODE, BUSAREA, CURRKEY,COSTCTR) %>%
    mutate(ACCT = as.character(ACCT),
           AMT  = as.numeric(AMT)) %>% 
    mutate(BAR_ACT = str_replace_all(BAR_ACCT, "A","")) %>% 
    filter(grepl("ENTITY",DI_ERRORCOLUMNS)) %>% 
    mutate(ACCT = paste0("000",ACCT))
  
  total_kick_entity <- sum(KICK_ENT$AMT)
  
  
  # Connect to the FDM File find ACCT in Src Acct to pick HFM Acct
  # Open HFM DMT Files and search for the HFM account. 
  
  
  ############ FDM File 
  
  
  # Make sure the FDM Files are readable;  
  
  
  setwd(mapping_fold)
  
  fdm_dos <- openxlsx::read.xlsx("WWPT_SAP_NA_US_2_Actual.xlsx") %>%
    as_tibble() %>% mutate(version = "fdm_dos")
  
  fdm_one <- openxlsx::read.xlsx("WWPT_SAP_NA_US_Actual.xlsx") %>% 
    as_tibble() %>% mutate(version = "fdm_one")
  
  
  fdm <- fdm_one %>% bind_rows(fdm_dos) %>% janitor::clean_names()
  
  # Entities of Interest: 
  
  busarea <- KICK_ENT$BUSAREA %>% as_tibble() %>% distinct()
  
  fdm_focus <- fdm %>%
    mutate(source_account = paste0("000",source_account)) %>% 
    filter(source_account %in% KICK_ENT$ACCT) %>% 
    filter(account %in% KICK_ENT$BAR_ACT) %>% 
    mutate(busarea = str_detect(source_icp, busarea$value)) %>%
    filter(busarea == T)
    
  
  fdm_entity <- fdm_focus %>% select(source_account, entity)
  
  
  entity_map <- KICK_ENT %>% 
    left_join(fdm_entity, by = c("ACCT" = "source_account")) %>% 
    select(-BAR_FUNCTION, -DI_ERRORCOLUMNS) %>% 
    mutate(ent_lenght = str_length(entity)) %>% 
    mutate(BAR_ENTITY = ifelse(ent_lenght == 4,
                               paste0("E",entity),
                               paste0("E0",entity))) %>% 
    select(-entity, -ent_lenght) 

  
  acn_entity_input <- entity_map %>% 
    select(-AMT) %>% 
    select(COCODE, BUSAREA, COCTR = COSTCTR, CURRKEY, BAR_ENTITY) %>% 
    distinct()

  
  
  
  recom <- paste("DESTINY TABLE: ", "EPM_C11_USDSENTITY_LKP_NA+  ,", "IF TOTAL_TO_KICK = 0, NO ACTION NEEDED")
  
  
  return(list(TO_BE_KICKOUT = KICK_ENT,
              TOTAL_TO_KICK = total_kick_entity,
              NOTES = recom,
              CONFIRM_TOT = entity_map,
              DMT_ACN_INPUT = acn_entity_input))
  
}


account_KO <- kickout_account()
entity_KO <- kickout_entity()


kick_storage <- function(){ 

setwd(KO)  
  
last_kick_account <- paste0("kickout_account_",today(),".RDS") %>% as_tibble() %>% 
  mutate(value = str_replace_all(value, "-","_"))

last_kick_entity <- paste0("kickout_entity_",today(),".RDS") %>% as_tibble() %>% 
  mutate(value = str_replace_all(value, "-","_"))

  kickout_account() %>% saveRDS(last_kick_account$value)
  kickout_entity()  %>% saveRDS(last_kick_entity$value)
  
                      
list.files() %>% as_tibble() %>% filter(grepl(".RDS",value))

print(last_kick_account)
print(last_kick_entity)

  
}  

# Open DMT 
# Citrix Script to run given a specific target. 

