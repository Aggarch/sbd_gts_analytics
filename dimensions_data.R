# Data Dimensions : 

library(readxl)
library(tidyverse)

dimensions <- "C:/Users/AEG1130/Documents/DDIMENS" 
dimens <- "C:/Users/AEG1130/Documents/dimension" 

setwd(dimensions)


 resources <- list.files() %>%
  as_tibble() %>% 
  mutate(key = str_replace_all(value, ".xlsx","")) %>% 
  mutate(key = str_replace_all(key, "_GTS_GEM","")) %>% 
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



