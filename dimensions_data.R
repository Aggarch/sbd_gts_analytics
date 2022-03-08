# Data Dimensions : 

library(readxlsb)
library(tidyverse)

dimen <- "C:/Users/AEG1130/Documents/DDIMENS" 
setwd(dimen)

 resources <- list.files() %>%
  as_tibble() %>% 
  mutate(key = str_replace_all(value, ".xlsb","")) %>% 
  mutate(key = str_replace_all(key, "_GTS_GEM","")) %>% 
   filter(!grepl(c("DM"),key)) %>% 
   filter(!grepl(c("Growth"),key)) %>% 
   filter(!grepl(c("Commit"),key)) %>% 
   filter(!grepl(c("Vendor"),key)) %>% 
   filter(!grepl(c("Version"),key)) %>% 
   filter(!grepl(c("Frequency"),key)) %>% 
   filter(!grepl(c("Geography"),key)) %>% 
   filter(!grepl(c("Period"),key))
 
 
 
 
dimens <- function(value, key){ 

data <- read_xlsb(value,key) %>% as_tibble() %>% 
  mutate(dimension = key) %>%
  janitor::clean_names() %>% 
  unite("alpha", x1:column_135,
        na.rm = TRUE, remove = FALSE, sep = "-") %>%
  select(dimension,member_type, name, description,
         pln_level, generation,alpha)

 

return(data)

}

tables <- map2(resources$value, resources$key, dimens) 

