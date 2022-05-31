# Data Dimensions : 

library(readxl)
library(tidyverse)

dimensions <- "C:/Users/AEG1130/Documents/DDIMENS.APR" 
reconc <- "C:/Users/AEG1130/Documents/Reconcilation" 
dimens <- "C:/Users/AEG1130/Documents/dimension" 
fx     <- "C:/Users/AEG1130/Documents/fx" 



setwd(dimensions)
# Dimensions Do Change MoM. 


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
        na.rm = T, remove = F, sep = "-") %>%
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
  
  
  

  
# FX Entities Analysis.  -----------------------------------------------------------

# Entities for REG TOT, filtered by Base, and x2 == woACQ section.   
  woacq <- tables %>% 
    filter(dimension == "Entity") %>% 
    filter(sub_dimension == "EN-GTS_REG_TOT") %>% 
    filter(member_type == "Base") %>% 
    filter(grepl("GTS_woACQ",x2))
  
  # For May filterable column will be x3, April ~ x2
  # how Possibly can be filterable column predictable? 
  # Data Structure should be consistent across. 
  

# Total Base entities Without Acquisitions is equivalent to 884 
# Reported Entities on the Entity list of FX Report == 549  
  
  setwd(reconc)
  
  current.fx <- openxlsx::read.xlsx('current.fx.xlsx')
  
  without_ACQ <- count(woacq)
  entity_list <- count(current.fx)
  
  without_ACQ - entity_list
  
  
# P&L Accounts can be Tie Out with other reports whe using Total of 
# Entities available.   
  
  

# Digging into North America ----------------------------------------------

# GTS Total Tools + OPG included NA  
  
 entities_NA <- function(){  
  
    # GTS Total Tools + OPG included NA  
    gts_NA <- woacq %>% filter(grepl("_NA_", x3)) %>% 
    mutate(segment  = "Tools") %>% 
    mutate(region  = "NA") %>% 
    rename(sub_region = x4) %>% 
    select(!contains("x")) %>% 
    filter(!grepl("DO NOT USE",description)) %>% 
    
    mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                               TRUE ~ as.character(description))) %>% 
      
    mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
    select(segment, dimension, sub_dimension,
           region, sub_region, currency, entity = name
    )
  
  
  
    # Tools Entities not OPG , NA 
     tools_NA  <- woacq %>% 
      filter(grepl("_NA_", x3)) %>%
      filter(!grepl("OPG",description)) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
        select(dimension, sub_dimension,
               currency, entity = name
             )
      
    
# OPG only 
  opg_NA <- gts_NA %>% anti_join(tools_NA, by = "name")
 
  print("Should Tie Out with GTS_NA_REG_TOT & BAR + HFM Trackers")
  return(gts_NA) 
  
  
 }
  
# Counting test: count(na) + count(opg) == count(gts)

  
  
  
# Questions: 

  #  Why do ANY of the P&L accounts for the PNL FX do match with BAR TRACKER?
  #  Why can queries easily be match with P&L BAR TRACKER but just FX report 
  #  Do not ties out? / If Volume is not reliable what about rates?  
  #  Where is the Entity List construct on the FX Report coming from? 
  
  
  #  All regions instead of NA available on column x4, NA ~ x3,
  #  Columns facilitate the data classification for a Dynamic Queries. 
  #  Dimensions Data hosted on SharePoint, How to make it more accessible? 
  
  
# EMEA
  tools_EMEA  <- woacq %>% filter(grepl("_EMEA", x4)) %>% filter(!grepl("OPG",description))
  
# GTS Total Tools + OPG included EMEA  
  gts_EMEA <- woacq %>% filter(grepl("_EMEA", x4)) 
  
# OPG only EMEA
  opg_EMEA <- gts_EMEA %>% anti_join(tools_EMEA, by = "name")
  
# EMEA Ties out with BAR TRACKER Under same logic. 
  

# Summarise Current FX April PNL Volumes. 
  

  cfx <- current.fx %>% as_tibble %>% 
    janitor::clean_names() %>% 
    filter(entity != "[none]") %>% 
    mutate(region = str_trim(region)) %>% 
    group_by(region) %>% 
    summarise(sales_lc = sum(sales_lc))
  
  
# All in LC (Local Currency)  
  
  tools_cfx_sales <- cfx %>%
    filter(!grepl("OPG", region)) %>% 
    janitor::adorn_totals()
  
  tot_sales_fx <- tools_cfx_sales %>% filter(region == "Total") %>% pull(sales_lc)
  tot_sales_reported <- 54523.74
  
  diff_sales <- tot_sales_reported - tot_sales_fx
  
  
  
# FX Report be Tie Out, Tie Out all entities for GTS, 
# NA, EMEA general ::: GTS_Core_woOUT, Label entities
# With region and description. 

  # Do GTS_NA + Sum(GTS_NA_OPG) = GTS_NA_REG_TOT !! 
  # Under this logic, It should be feasible to Tie out
  # Globally, under GTS_REG_TOT = Tools_Regions + OPG_Regions
  # Where Data Come from woACQ families. 
  
  
  
