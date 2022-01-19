

# CITRIX ------------------------------------------------------------------

library(tidyverse)

docs <- "C:/Users/AEG1130/Documents/data"
setwd("//americas.swk.pri/apps/Enterprise/FT/Prod/P-BODSETL/PRD_DMT/2013")

list.files()

setwd(docs)


citrix <- openxlsx::read.xlsx("citrix_tables.xlsx") %>% as_tibble





# FileExplorer ------------------------------------------------------------


FPA.SP <- "C:/Users/AEG1130/Stanley Black & Decker/GTS Group FP&A - Documents/2021/Close"
closef <- "C:/Users/AEG1130/Stanley Black & Decker/GTS Group FP&A - Documents/2021/Close/12 December 2021/Closing Files/Close Checklist"



setwd(FPA.SP)

schema <- list.files() %>%
  as_tibble() %>%
  mutate(path = getwd()) %>% 
  mutate(p2 = paste0(path,"/",value)) %>% 
  mutate(directories = map(.$p2, list.dirs)) %>% 
  unnest(cols = directories) %>% 
  mutate(d2 = map(.$directories, list.dirs)) %>% 
  unnest(cols = d2) %>% 
  mutate(files = map(.$d2, list.files)) %>% 
  unnest(cols = files) %>% 
  mutate(path = paste0(d2,"/",files)) %>% 
  mutate(files = map(.$path, list.files)) %>% 
  unnest(cols = files) %>% 
  filter(!grepl(".pptx",files)) %>% 
  mutate(files = map(.$path, list.files)) %>% 
  unnest(cols = files) %>% 
  
  mutate(fullp = paste0(path,"/",files)) %>% 
  filter(grepl(".xlsx",files))
  # mutate(sheets = map(.$fullp, excel_sheets)) %>% 
  # unnest(cols = sheets)

  
  
  
  
  
  









setwd(closef)
file <- list.files()

checklist <- openxlsx::read.xlsx(file) %>% as_tibble() %>% 
  filter(grepl("Garcia",X7))


dec_rep_portfolio <- list.dirs() %>%
  as_tibble() %>% 
  rename(file = value) %>% 
  separate(file, c("main","second","third","fourth"),sep = "([/])") %>% 
  mutate(path = paste0(second,"/", third,"/", fourth)) %>% 
  # filter(!is.na(fourth)) %>%
  select(-main) %>%  
  filter(grepl("December",second)) %>% 
  mutate(files = map(.$path, list.files)) %>% 
  unnest(cols = files) %>% 
  distinct() %>% 
  mutate(fullp = paste0(path,"/",files)) %>% 
  filter(grepl(".xlsx",files)) %>% 
  mutate(sheets = map(.$fullp, excel_sheets)) %>% 
  unnest(cols = sheets)
