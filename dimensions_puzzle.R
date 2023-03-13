# Data Dimensions : 

library(readxl)
library(tidyverse)

reconc     <- "C:/Users/AEG1130/Documents/Reconcilation" 
setwd(reconc)


# Data resource Map.

data <- openxlsx::read.xlsx("entities_dimensions.xlsx") %>% 
  as_tibble()%>%
  janitor::clean_names() %>% 
  janitor::remove_empty(., which = "cols") %>% 
  unite("structure", x1:everything(),
        na.rm = T, remove = F, sep = "-") %>% 
  relocate(name, description, structure, everything()) 
  #select(-pln_level, -member_type)



  reg_all <- data %>% filter(grepl("REG_ALL",structure)) %>% 
    filter(member_type == "Base") 
  

  
# Entities Iteracy.     
n.entities <- function(){     
  
# Digging into NA Entities:
  

# North America -----------------------------------------------------------

  entities_NA <- function(){  
    
    # GTS Total Tools + OPG   
    gts_NA <- reg_all %>% filter(grepl("_NA_", x4)) %>% 
      mutate(region  = "North America") %>% 
      rename(sub_region = x5) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment,
             region, sub_region, currency, entity = name
      )
    
    
    print("Should Tie Out with GTS_NA_REG_ALL")
    return(gts_NA) 
    
    
  }

  
  
# EMEA --------------------------------------------------------------------
  
  entities_EMEA <- function(){  
    
    # GTS Total Tools + OPG  
    gts_EMEA <- data %>% filter(grepl("GTS_EMEANZ_REG_ALL", x5)) %>% 
      # filter(grepl("_MKT_ALL", x8)) %>% 
      mutate(region  = "EMEA ANZ") %>% 
      rename(sub_region = x7) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment, 
             region, sub_region, currency, entity = name
      )
    

    print("Should Tie Out with GTS_EMEANZ_REG_ALL")
    return(gts_EMEA) 
    
    
  }
  
  
  
# LAG ---------------------------------------------------------------------

  entities_LAG <- function(){  
    
    gts_LAG <- reg_all %>% filter(grepl("GTS_LAG_REG_ALL", x6)) %>% 
      #filter(grepl("LAG_MKTS_REG_TOT", x8)) %>% 
      
      mutate(region  = "LAG Tools") %>% 
      rename(sub_region = x7) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment, 
             region, sub_region, currency, entity = name
      )
    
    print("Should Tie Out with GTS_LAG_REG_ALL")
    return(gts_LAG) 
    
    
  }
  

  
# ASIA --------------------------------------------------------------------
  
  entities_ASIA <- function(){  
    
    gts_ASIA <- reg_all %>% filter(grepl("GTS_ASIA_REG_TOT", x6)) %>% 
      #ilter(grepl("ASIA_MKTS_ALL", x8)) %>% 
      
      mutate(region  = "ASIA Tools") %>% 
      rename(sub_region = x7) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment,
             region, sub_region, currency, entity = name
      )
    

    
    print("Should Tie Out with GTS_ASIA_REG_ALL")
    return(gts_ASIA) 
    
    
  }

    
  
# ASIA MFG ----------------------------------------------------------------
  
  entities_ASIA_MFG <- function(){  
    
    gts_ASIA_MFG <- reg_all %>% filter(grepl("GTS_ASIA_MFG_REG_TOT", x5)) %>% 
      mutate(region  = "ASIA MFG") %>% 
      rename(sub_region = x6) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment,
             region, sub_region, currency, entity = name
      )
    
    
    
    print("Should Tie Out with GTS_ASIA_MFG_REG_TOT")
    return(gts_ASIA_MFG) 
    
    
  }
  
  
# HQ ----------------------------------------------------------------------
  
  entities_HQ <- function(){  
    
    gts_HQ <- reg_all %>% 
      filter(grepl("GTS_HQ_WOAdj_REG_ALL", x5)) %>% 
      mutate(region  = "GTS HQ") %>% 
      rename(sub_region = x6) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment,
             region, sub_region, currency, entity = name
      )
    
    
    
    print("Should Tie Out with GTS_HQ_WOAdj_REG_ALL")
    return(gts_HQ) 
    
    
  }
  

  
# HEDGE ------------------------------------------------------------------
  
  entities_HEDGE <- function(){  
    
    gts_HEDGE <- reg_all %>% filter(grepl("GTS_HQ_ADJ_ALL", x5)) %>% 
      mutate(region  = "GTS HEDGE") %>% 
      rename(sub_region = x5) %>% 
      select(!contains("x")) %>% 
      filter(!grepl("DO NOT USE",description)) %>% 
      
      mutate(segment = case_when(str_detect(description,"OPG")~"OPG",
                                 TRUE ~ as.character(description))) %>% 
      
      mutate(segment = ifelse(segment == "OPG", "OPG", "Tools")) %>%   
      
      select(segment,
             region, sub_region, currency, entity = name
      )
    
    
    
    print("Should Tie Out with GTS_HQ_ADJ_ALL")
    return(gts_HEDGE) 
    
    
  }
  
  

# Function Return ---------------------------------------------------------


  
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



# Reverse Engeenering

export_vect_entities <- function(){ 

Ent_all_new <- n.entities() %>% 
  map_dfr(.,bind_rows) %>% 
  as_tibble()


setwd(reconc)

Ent_all_new %>% openxlsx::write.xlsx(.,"Ent_all_new.vect.xlsx", overwrite = T) 

print("new vector ready")

}


