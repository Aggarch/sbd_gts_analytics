# Data Dimensions : 

library(readxl)
library(tidyverse)

dimensions <- "C:/Users/AEG1130/Documents/DDIMENS.M" 
reconc     <- "C:/Users/AEG1130/Documents/Reconcilation" 
dimens     <- "C:/Users/AEG1130/Documents/dimension" 
fx         <- "C:/Users/AEG1130/Documents/fx" 


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



# Looking for an Account  -------------------------------------------------

acct <- function(account)
  {
tables %>%
  filter(dimension == "Account") %>%
  filter(grepl("FX",structure)) %>%
  filter(grepl(account,structure)) %>% select(dimension, sub_dimension, name, description)
}

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

# SUM an X to all below depending on the Month of observation. 

# Entities for REG TOT, filtered by Base, and x2 == woACQ section.   
  woacq <- tables %>% 
    filter(dimension == "Entity") %>% 
    filter(sub_dimension == "EN-GTS_REG_TOT") %>% 
    filter(member_type == "Base") %>% 
    filter(grepl("GTS_woACQ",x3))
  

woacq.apr<-woacq # x ~ 2  
woacq.may<-woacq # x ~ 3


  
  # For May filterable column will be x3, April ~ x2
  # how Possibly can be filterable column predictable? 
  # Data Structure should be consistent across. 
  

# Total Base entities Without Acquisitions is equivalent to 884 
# Reported Entities on the Entity list of FX Report == 549  
  
  setwd(reconc)
  
  current.fx <- openxlsx::read.xlsx('current.fx.xlsx') %>% 
    as_tibble %>% janitor::clean_names()
  
  current.fx %>% group_by(region) %>% 
    summarise(sales_lc = sum(sales_lc)) %>%
    janitor::adorn_totals()
  
  
  
# Assuming::: Currency == LC, account == Net Sales, 
# Entities == 551, Period == Apr, Scenario == ACTUAL.
  
  without_ACQ <- count(woacq)
  entity_list <- count(current.fx)
  
  without_ACQ - entity_list
  
  
# P&L Accounts can be Tie Out with other reports whe using Total of 
# Entities available.   
  
  

# Digging into North America ----------------------------------------------

# GTS Total Tools + OPG included NA  
  
  
Entities <- function(){   
  

# NORTH AMERICA -----------------------------------------------------------

 entities_NA <- function(){  
  
    # GTS Total Tools + OPG included NA  
    gts_NA <- woacq %>% filter(grepl("_NA_", x4)) %>% 
    mutate(segment  = "Tools") %>% 
    mutate(region  = "NA") %>% 
    rename(sub_region = x5) %>% 
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
  opg_NA <- gts_NA %>% anti_join(tools_NA, by = "entity")
 
  print("Should Tie Out with GTS_NA_REG_TOT & BAR + HFM Trackers")
  return(gts_NA) 
  
  
 }
  
# Counting test: count(na) + count(opg) == count(gts)

  
# MAY ''NA'' x4 Subregion == x5
# APR ''NA'' x3 Subregion == x4 
 
  
  
# Questions: 

  #  Why do ANY of the P&L accounts for the PNL FX do match with BAR TRACKER?
  #  Why can queries easily be match with P&L BAR TRACKER but just FX report 
  #  Do not ties out? / If Volume is not reliable what about rates?  
  #  Where is the Entity List construct on the FX Report coming from? 
  
  
  #  All regions instead of NA available on column x4, NA ~ x3,
  #  Columns facilitate the data classification for a Dynamic Queries. 
  #  Dimensions Data hosted on SharePoint, How to make it more accessible? 
  
  

# EMEA --------------------------------------------------------------------

  entities_EMEA <- function(){  
    
    # GTS Total Tools + OPG included NA  
    gts_EMEA <- woacq %>% filter(grepl("EMEA", x5)) %>% 
      mutate(segment  = "Tools") %>% 
      mutate(region  = "EMEANZ") %>% 
      rename(sub_region = x6) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment, dimension, sub_dimension,
             region, sub_region, currency, entity = name
      )
    
    
    
    # Tools Entities not OPG , NA 
    tools_EMEA  <- woacq %>% 
      filter(grepl("EMEA", x5)) %>%
      filter(!grepl("OPG",description)) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      select(dimension, sub_dimension,
             currency, entity = name
      )
    
    
    # OPG only 
    opg_EMEA <- gts_EMEA %>% anti_join(tools_EMEA, by = "entity")
    
    print("Should Tie Out with GTS_NA_REG_TOT & BAR + HFM Trackers")
    return(gts_EMEA) 
    
    
  }
  
  
  
# EMEA Ties out with BAR TRACKER Under same logic. 
  
  
  # LAG --------------------------------------------------------------------
  
  entities_LAG <- function(){  
    
    # GTS Total Tools + OPG included NA  
    gts_LAG <- woacq %>% filter(grepl("GTS_LAG_REG_TOT", x6)) %>% 
      mutate(segment  = "Tools") %>% 
      mutate(region  = "LAG") %>% 
      rename(sub_region = x7) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment, dimension, sub_dimension,
             region, sub_region, currency, entity = name
      )
    
    
    
    # Tools Entities not OPG , NA 
    tools_LAG  <- woacq %>% 
      filter(grepl("LAG", x5)) %>%
      filter(!grepl("OPG",description)) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      select(dimension, sub_dimension,
             currency, entity = name
      )
    
    
    # OPG only 
    opg_LAG <- gts_LAG %>% anti_join(tools_LAG, by = "entity")
    
    print("Should Tie Out with GTS_NA_REG_TOT & BAR + HFM Trackers")
    return(gts_LAG) 
    
    
  }
  
  
  

# ASIA --------------------------------------------------------------------

  entities_ASIA <- function(){  
    
    # GTS Total Tools + OPG included NA  
    gts_ASIA <- woacq %>% filter(grepl("ASIA_REG", x6)) %>% 
      mutate(segment  = "Tools") %>% 
      mutate(region  = "ASIA") %>% 
      rename(sub_region = x6) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment, dimension, sub_dimension,
             region, sub_region, currency, entity = name
      )
    
    
    
    # Tools Entities not OPG , NA 
    tools_ASIA  <- woacq %>% 
      filter(grepl("ASIA", x5)) %>%
      filter(!grepl("OPG",description)) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      select(dimension, sub_dimension,
             currency, entity = name
      )
    
    
    # OPG only 
    opg_ASIA <- gts_ASIA %>% anti_join(tools_ASIA, by = "entity")
    
    print("Should Tie Out with GTS_NA_REG_TOT & BAR + HFM Trackers")
    return(gts_ASIA) 
    
    
  }
  
  
  
# ASIA MFG ----------------------------------------------------------------
  
  entities_ASIA_MFG <- function(){  
    
    # GTS Total Tools + OPG included NA  
    gts_ASIA_MFG <- woacq %>% filter(grepl("ASIA_MFG_REG_TOT", x5)) %>% 
      mutate(segment  = "Tools") %>% 
      mutate(region  = "ASIA_MFG") %>% 
      rename(sub_region = x6) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment, dimension, sub_dimension,
             region, sub_region, currency, entity = name
      )
    
    
    
    # Tools Entities not OPG , NA 
    tools_ASIA_MFG  <- woacq %>% 
      filter(grepl("ASIA_MFG_REG_TOT", x5)) %>%
      filter(!grepl("OPG",description)) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      select(dimension, sub_dimension,
             currency, entity = name
      )
    
    
    # OPG only 
    opg_ASIA_MFG <- gts_ASIA_MFG %>% anti_join(tools_ASIA_MFG, by = "entity")
    
    print("Should Tie Out with GTS_NA_REG_TOT & BAR + HFM Trackers")
    return(gts_ASIA_MFG) 
    
    
  }
  
  
  
# HQ ----------------------------------------------------------------------
  
  entities_HQ <- function(){  
    
    # GTS Total Tools + OPG included NA  
    gts_HQ <- woacq %>% filter(grepl("GTS_HQ_WOAdj_REG_TOT", x5)) %>% 
      mutate(segment  = "Tools") %>% 
      mutate(region  = "GTS_HQ") %>% 
      rename(sub_region = x6) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment, dimension, sub_dimension,
             region, sub_region, currency, entity = name
      )
    
    
    
    # Tools Entities not OPG , NA 
    tools_HQ  <- woacq %>% 
      filter(grepl("GTS_HQ_woAdj_TOT", x5)) %>%
      filter(!grepl("OPG",description)) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      select(dimension, sub_dimension,
             currency, entity = name
      )
    
    
    # OPG only 
    opg_HQ <- gts_HQ %>% anti_join(tools_HQ, by = "entity")
    
    print("Should Tie Out with GTS_NA_REG_TOT & BAR + HFM Trackers")
    return(gts_HQ) 
    
    
  }
  
  

# HEDGE -------------------------------------------------------------------

  
  entities_HEDGE <- function(){  
    
    # GTS Total Tools + OPG included NA  
    gts_HEDGE <- woacq %>% filter(grepl("GTS_HQ_ADJ_TOT", x5)) %>% 
      mutate(segment  = "Tools") %>% 
      mutate(region  = "GTS_HEDGE") %>% 
      rename(sub_region = x6) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment, dimension, sub_dimension,
             region, sub_region, currency, entity = name
      )
    
    
    
    # Tools Entities not OPG , NA 
    tools_HEDGE  <- woacq %>% 
      filter(grepl("GTS_HQ_woAdj_TOT", x5)) %>%
      filter(!grepl("OPG",description)) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      select(dimension, sub_dimension,
             currency, entity = name
      )
    
    
    # OPG only 
    opg_HEDGE <- gts_HEDGE %>% anti_join(tools_HEDGE, by = "entity")
    
    print("Should Tie Out with GTS_NA_REG_TOT & BAR + HFM Trackers")
    return(gts_HEDGE) 
    
    
  }
  

  
  
NAm   <- entities_NA()
EMEA  <- entities_EMEA()
LAG   <- entities_LAG()
ASIA  <- entities_ASIA()
ASIAM <- entities_ASIA_MFG()
HQ    <- entities_HQ()
HEDGE <- entities_HEDGE()

return(list(NAm   = NAm,
            EMEA  = EMEA,
            LAG   = LAG,
            ASIA  = ASIA,
            ASIAM = ASIAM,
            HQ    = HQ,
            HEDGE = HEDGE))
}
  
Ent_all <- Entities() %>% 
    map_dfr(.,bind_rows) %>% 
  as_tibble() %>% 
  mutate(region = ifelse(is.na(region),"NA",region)) %>% 
  mutate(region =  case_when(region == "NA"~"North America",
                             region == "EMEANZ"~"EMEA ANZ",
                             region == "LAG"~"LAG Tools",
                             region == "ASIA"~"ASIA Tools",
                             region == "ASIA_MFG"~"ASIA MFG",
                             region == "GTS_HQ"~"GTS HQ",
                             region == "GTS_HEDGE"~"GTS HEDGE"))

setwd(reconc)

Ent_all %>% openxlsx::write.xlsx(.,"Ent_all_raw.xlsx", overwrite = T) 

                             


# Compare PSum FX  --------------------------------------------------------

  
      
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
  
  

# Global PNL -----------------------------------------------------------

  # Volumes distribution by account and LC with reported currency 
  # Just entities not on current.fx

setwd(reconc)
  
  
fx.pnl.gaps <- function(){   
    
  # Global Sales  
  
  
  current.fx <- openxlsx::read.xlsx('current.fx.xlsx') %>% 
    as_tibble %>% janitor::clean_names()
  
  
  sales_ent <- openxlsx::read.xlsx("global_sales_ytd.xlsx") %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    mutate(region = ifelse(is.na(region),"NA",region)) %>% 
    select(segment, region, sub_region, lc, entity,month, value) %>% 
    filter(!entity %in% current.fx$entity ) %>% 
    filter(value != 0)
  
  sales <-  sales_ent %>% 
    group_by(segment,lc, month) %>% 
    summarise(v = sum(value),
              entities = str_flatten(entity, collapse =","),
              .groups = "drop") %>% 
    filter(lc != "USD") %>%
    mutate(f = v*1000000) %>% 
    janitor::adorn_totals() %>% 
    relocate(.after = f, entities) %>% 
    filter(f != 0)
  
  
  
  # Global SGM  
  
  sgm_ent <- openxlsx::read.xlsx("global_sgm_ytd.xlsx") %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    mutate(region = ifelse(is.na(region),"NA",region)) %>% 
    select(segment, region, sub_region, lc, entity,month, value)
  
  sgm <-  sgm_ent %>% 
    filter(!entity %in% current.fx$entity ) %>% 
    group_by(segment,lc, month) %>% 
    summarise(v = sum(value),
              entities = str_flatten(entity, collapse =","),
              .groups = "drop") %>% 
    filter(lc != "USD") %>%
    mutate(f = v*1000000) %>% 
    janitor::adorn_totals() %>% 
    relocate(.after = f,entities) %>% 
    filter(f != 0)
  
  
  
# Global SG&A  
  
  sganda_ent <- openxlsx::read.xlsx("global_sganda_ytd.xlsx") %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    mutate(region = ifelse(is.na(region),"NA",region)) %>% 
    select(segment, region, sub_region, lc, entity,month, value)
  
  sga <-   sganda_ent %>%
    filter(!entity %in% current.fx$entity ) %>% 
    group_by(segment,lc, month) %>% 
    summarise(v = sum(value),
              entities = str_flatten(entity, collapse =","),
              .groups = "drop") %>% 
    filter(lc != "USD") %>%
    mutate(f = v*1000000) %>% 
    janitor::adorn_totals() %>% 
    relocate(.after = f, entities) %>% 
    filter(f !=0)
    
  
  
# Global OCOS  
  
  cosoth_ent <- openxlsx::read.xlsx("global_ocos_ytd.xlsx") %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    mutate(region = ifelse(is.na(region),"NA",region)) %>% 
    select(segment, region, sub_region, lc, entity,month, value)
  
  ocos <- cosoth_ent %>% 
    filter(!entity %in% current.fx$entity) %>% 
    group_by(segment,lc, month) %>% 
    summarise(v = sum(value),
              entities = str_flatten(entity, collapse =","),
              .groups = "drop") %>% 
    filter(lc != "USD") %>%
    mutate(f = v*1000000) %>% 
    janitor::adorn_totals() %>% 
    relocate(.after = f, entities) %>% 
    filter(f != 0)
  
  
  return(list(sales =  sales,
              sgm   =  sgm,
              sga   =  sga,
              ocos  =  ocos))
  }
  

# Original Ents
original_ents <- current.fx %>% filter(!grepl("OPG",business)) %>% filter(!grepl("USD",lc))

# Updated Ents
current_ents <- Ent_all %>% filter(segment != "OPG", currency != "USD")

current_ents <- Ent_all %>% filter(currency != "USD")

  
# Missing Ents

current_ents %>% anti_join(original_ents, by ="entity")


# Ents.new 
ents.new <- openxlsx::read.xlsx("ents.new.xlsx") %>% as_tibble() %>% 
  rename(entity = Name) %>% 
  select(entity, Description, Currency)
