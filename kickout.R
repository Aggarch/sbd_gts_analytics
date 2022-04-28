
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


# Mapping CC to Function --------------------------------------------------


# Open DMT 
# Citrix Script to run given a specific target. 

# Cost_to_fucntion

getwd()
setwd("C:/Users/AEG1130/Documents/mapping")

# MK provided file:::

cc_function <- openxlsx::read.xlsx("cc_function.xlsx") %>%
  as_tibble() %>% 
  janitor::clean_names() %>%
  select(-x7) %>% 
  filter(!is.na(bar_function))

collapsed<- cc_function %>% 
  select(source_cost_cntr, bar_function) %>% 
  distinct() %>% 
  group_by(source_cost_cntr) %>% 
  summarise(bar_function = str_flatten(bar_function,collapse = ","))
  
# DMT MAP COCTR Function

dmt_function_cc <- openxlsx::read.xlsx("dmt_function_cc.xlsx") %>%
  as_tibble() %>% 
  janitor::clean_names()



dmt_function_cc2 <- openxlsx::read.xlsx("prod2_table.xlsx") %>%
  as_tibble() %>% 
  janitor::clean_names()


dmt_function_cc %>% filter(coctr == "1605641461")
dmt_function_cc %>% filter(coctr == "9401500172")
dmt_function_cc %>% filter(coctr == "6013040164")

dmt_function_cc %>% filter(coctr == "9401500170")


# CC To Function Current Mapping Table. 
current <- dmt_function_cc %>% filter(coctr %in% collapsed$source_cost_cntr)

current2 <- dmt_function_cc2 %>% filter(coctr %in% collapsed$source_cost_cntr)


# Mike Kemp Reported Not in Construction table. 
reported <- collapsed %>% filter(!source_cost_cntr %in% dmt_function_cc$coctr)


# Mike Reported Structure: 
summary_c <- collapsed %>%
  mutate(count = str_count(bar_function,",")) %>% 
  mutate(bar_function_counting = count+1) %>% 
  select(-count)

# Current State of BAR CC to Function: 
summary_r <- current %>%
  group_by(coctr) %>% 
  summarise(bar_function = str_flatten(bar_function, collapse=",")) %>% 
  mutate(count = str_count(bar_function,",")) %>% 
  mutate(bar_function_counting = count+1) %>% 
  select(-count)


# Reported Not in Current State 

diff<- summary_c %>% filter(!source_cost_cntr %in% summary_r$coctr)


# Roll files over ---------------------------------------------------------

library(tidyverse)
library(lubridate)

f4<- "C:/Users/AEG1130/Documents/Forecast_04/F04_BL"

setwd(f4)

hoy <- today() %>% 
  as_tibble() %>% 
  mutate(value = str_replace_all(value,"-","_"))

time<- now() %>% 
  as_tibble() %>%
  mutate(value = as.character(value)) %>% 
  separate(value, c("day","time"), sep = " ") %>% 
  separate(time, c("hour","mins","secs"),sep = ":") %>%
  select(-day,-secs) %>% 
  unite("time", hour:mins)


files <- list.files() %>% as_tibble() %>% 
  mutate(value = str_replace_all(value,".xlsx","")) %>% 
  mutate(append = paste("_BL_",hoy,"@",time,".xlsx")) %>% 
  mutate(append = str_replace_all(append," ","")) %>% 
  mutate(append = str_trim(append)) %>% 
  mutate(fname = paste0(value,append)) %>% 
  select(-append)


getwd()
old_files <- list.files()


new_files <- files %>% pull(fname)

file.copy(from = old_files, to = new_files)
# Clear out the old files
file.remove(old_files)

# BL Elements Created refreshes on Live Versions
# At this point should be just about Breaking Links. 

rnf<- old_files %>% as_tibble() %>% mutate(fname = str_replace_all(value,"11_14","11AM"))

new_files <- rnf %>% pull(fname)

file.copy(from = old_files, to = new_files)
file.remove(old_files)



