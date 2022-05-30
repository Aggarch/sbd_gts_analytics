# Data Dimensions : 

library(readxl)
library(tidyverse)

dimensions <- "C:/Users/AEG1130/Documents/DDIMENS" 
dimens <- "C:/Users/AEG1130/Documents/dimension" 
reconc <- "C:/Users/AEG1130/Documents/Reconcilation" 

setwd(dimensions)


 resources <- list.files() %>%
  as_tibble() %>% 
  mutate(key = str_replace_all(value, ".xlsx","")) %>% 
  mutate(key = str_replace_all(key, "_GTS_GEM","")) %>% 
  mutate(key = str_replace_all(key, "~$","")) %>% 
  mutate(value = str_replace_all(value, "~$","")) %>% 
  filter(!grepl("Product",value)) %>% 
   mutate(sheet = map(.$value, excel_sheets)) %>% 
   unnest(cols = c(sheet))
 
 
dimens <- function(value, key, sheet){ 

data <- openxlsx::read.xlsx(value,sheet) %>% 
  as_tibble() %>% 
  mutate(dimension = key) %>%
  mutate(sub.dimension = sheet) %>%
  janitor::clean_names() %>% 
   janitor::remove_empty(., which = "cols") %>% 
  unite("structure", x1:everything(),
        na.rm = TRUE, remove = FALSE, sep = "-") %>%
  # mutate(structure = str_replace_all(structure,"-Parent","")) %>% 
  # mutate(structure = str_replace_all(structure,"-Base",""))%>%
  # mutate(structure = str_replace_all(structure,"-TRUE","")) %>% 
  # mutate(structure = str_replace_all(structure,"-FALSE","")) %>% 
  # select(!contains("x")) %>% 
  relocate(dimension, sub_dimension, name,
           description, structure, everything())



return(data)

}

tables <- resources %>% pmap(dimens) %>% 
          map_dfr(.,bind_rows) %>% 
  select(-pln_member_type, -base)


setwd(dimens)

tables %>% 
  openxlsx::write.xlsx(.,"dimensions_unite.xlsx", overwrite = T)



# Entities --------------------------------------------------------------------


regtot <- tables %>% filter(dimension == "Entity") %>% filter(grepl("REG_TOT",structure))


#GEM_LEG no REG_TOT Family
tables %>% filter(dimension == "Entity") %>% filter(grepl("GEM",structure)) %>% filter(member_type == "Parent")

#GEM_EMG_LEG
tables %>% filter(dimension == "Entity") %>% filter(grepl("GEM_EMG",structure))

#EMG_META_TOT
tables %>% filter(dimension == "Entity") %>% filter(grepl("EMG_META",structure))

#EMG_MENA_TOT
tables %>% filter(dimension == "Entity") %>% filter(grepl("EMG_MENA",structure))

#GTS_GEM_ASIA
tables %>% filter(dimension == "Entity") %>% filter(grepl("GTS_GEM_ASIA",structure))

#EMG_IND_incl_Pune
tables %>% filter(dimension == "Entity") %>% filter(grepl("EMG_IND",structure))

#ASIA_CHN_TOT
tables %>% filter(dimension == "Entity") %>% filter(grepl("ASIA_CHN",structure))


# Products ----------------------------------------------------------------

# PTG
tables %>% filter(dimension == "Prod") %>% filter(grepl("PTG",structure))

#HTAS
tables %>% filter(dimension == "Prod") %>% filter(grepl("HTAS",structure))

#HTS
tables %>% filter(dimension == "Prod") %>% filter(grepl("HTS",structure))

#OPG
tables %>% filter(dimension == "Prod") %>% filter(grepl("OPG",structure))

#OutD
tables %>% filter(dimension == "Prod") %>% filter(grepl("Outdoor",structure))

#CPT 
tables %>% filter(dimension == "Prod") %>% filter(grepl("CPT",structure))



# Dimensions Search Engine ------------------------------------------------



dimens_search <- function(dimen, pattern){
  
  tables %>% filter(dimension == dimen) %>% 
             filter(grepl(pattern,structure))
  
}


explore <- tibble(dimension = c("Entity","Entity"),
                  structure = c("ALLOC", "REG_TOT"))

map2(explore$dimension, 
     explore$structure,
     dimens_search)


tables %>% filter(dimension == "Entity") %>% 
           filter(!grepl("Adjust", structure)) %>% 
           filter(!grepl("Alloc",structure)) %>%
           filter(grepl("REG_TOT",sub_dimension))-> reg_vector

# Value Match

origin <- openxlsx::read.xlsx("../origin.xlsx") %>% as_tibble()

reg_tot <- openxlsx::read.xlsx("../REG_TOT.xlsx") %>% as_tibble()





# HFM Dimensions  ---------------------------------------------------------


setwd(reconc)

  
  resources <- list.files() %>%
  as_tibble() %>% 
  mutate(key = str_replace_all(value, ".xlsx","")) %>% 
  mutate(key = str_replace_all(key, "_Dimenisons","")) %>% 
  filter(grepl("Dimensions",value)) %>% 
  mutate(sheet = map(.$value, excel_sheets)) %>% 
  unnest(cols = c(sheet))
  
  
  dimens <- function(value, key, sheet){ 
    
    data <- openxlsx::read.xlsx(value,sheet) %>% 
      as_tibble() %>% 
      mutate(dimension = key) %>%
      mutate(sub.dimension = sheet) %>%
      janitor::clean_names() %>% 
      janitor::remove_empty(., which = "cols") %>% 
      unite("structure",x2: everything(),
             na.rm = TRUE, remove = FALSE, sep = "-")
      # mutate(structure = str_replace_all(structure,"-Parent","")) %>% 
      # mutate(structure = str_replace_all(structure,"-Base",""))%>%
      # mutate(structure = str_replace_all(structure,"-TRUE","")) %>% 
      # mutate(structure = str_replace_all(structure,"-FALSE","")) %>% 
      # relocate(dimension, sub_dimension,
      #          description, structure, everything())
      # 
      # 
    
    return(data)
    
  }
  
  tables <- resources %>% pmap(dimens) %>% 
    map_dfr(.,bind_rows) %>% 
    select(!contains("x")) %>% 
    select(-dimension) %>% 
    rename(dimension = sub_dimension)
  
  
  

# FX Entities.  -----------------------------------------------------------

# Entities for REG TOT, filtered by Base, and x2 == woACQ section.   
  woacq <- tables %>% 
    filter(dimension == "Entity") %>% 
    filter(sub_dimension == "EN-GTS_REG_TOT") %>% 
    filter(member_type == "Base") %>% 
    filter(grepl("GTS_woACQ",x2))
  

# Total Base entities Without Acquisitions is equivalent to 884 
# Reported Entities on the Entity list of FX Report == 549  
  
  without_ACQ <- count(woacq)
  entity_list <- count(current.fx)
  
  without_ACQ - entity_list
  
  
# P&L Accounts can be Tie Out with other reports whe using Total of 
# Entities available.   
  
  
  

# Digging into North America ----------------------------------------------

  
# Tools Entities not OPG , NA 
  na  <- woacq %>% filter(grepl("_NA_", x3)) %>% filter(!grepl("OPG",description))
  
# Entity List contruct on the FX Report  
  
# GTS Total Tools + OPG included NA  
  gts_NA <- woacq %>% filter(grepl("_NA_", x3)) 
  
# OPG only 
  opg_NA <- gts %>% anti_join(na, by = "name")
  
# Counting test: count(na) + count(opg) == count(gts)

# Questions: 

  #  Why do ANY of the P&L accounts for the FX do match with BAR TRACKER?
  #  Why can queries easily be match with P&L BAR TRACKER but just FX report 
  #  Do not ties out? / If Volume is not reliable what about rates?  
  
  
  
# EMEA
  emea  <- woacq %>% filter(grepl("_EMEA", x4)) %>% filter(!grepl("OPG",description))
  
# GTS Total Tools + OPG included EMEA  
  gts_EMEA <- woacq %>% filter(grepl("_EMEA", x4)) 
  
# OPG only EMEA
  opg_EMEA <- gts_EMEA %>% anti_join(emea, by = "name")
  
# EMEA Ties out with BAR TRACKER Under same logic. 
  

# Summarise Current FX April PNL Volumes. 
  
  current.fx <- openxlsx::read.xlsx('current.fx.xlsx')
  
  
  cfx <- current.fx %>% as_tibble %>% 
    janitor::clean_names() %>% 
    filter(entity != "[none]") %>% 
    mutate(region = str_trim(region)) %>% 
    group_by(region) %>% 
    summarise(sales_lc = sum(sales_lc))
  
  
  sum.cfx <- cfx %>%
    filter(!grepl("OPG", region)) %>% 
    janitor::adorn_totals()
  
  tot_sales_fx <- sum.cfx %>% filter(region == "Total") %>% pull(sales_lc)
  tot_sales_reported <- 54523.74
  
  diff_sales <- tot_sales_reported - tot_sales_fx
  
# FX Report be Tie Out, Tie Out all entities for GTS, 
# NA, EMEA general ::: GTS_Core_woOUT, Label entities
# With region and description. 

  # Do GTS_NA + Sum(GTS_NA_OP) = GTS_NA_LEG ? 
