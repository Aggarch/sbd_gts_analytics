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
  mutate(structure = str_replace_all(structure,"-Parent","")) %>% 
  mutate(structure = str_replace_all(structure,"-Base",""))%>%
  mutate(structure = str_replace_all(structure,"-TRUE","")) %>% 
  mutate(structure = str_replace_all(structure,"-FALSE","")) %>% 
  select(!contains("x")) %>% 
  relocate(dimension, sub_dimension, name,
           description, structure, everything())



return(data)

}

tables <- resources %>% pmap(dimens) %>% 
          map_dfr(.,bind_rows) %>% 
  select(-pln_member_type, -currency, -base)


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
  
  
  
