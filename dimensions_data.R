# Data Dimensions : 

library(readxlsb)
library(tidyverse)

dimen <- "C:/Users/AEG1130/Documents/DDIMENS" 
setwd(dimen)

list.files() %>% as_tibble


data <- read_xlsb("Entity_GTS_GEM.xlsb","Entity") %>% as_tibble()


data %>% 
  janitor::clean_names() %>% 
  unite("alpha", x1:column_135,
        na.rm = TRUE, remove = FALSE, sep = "-") %>%
  select(member_type, name, description,
         pln_level, generation,alpha)

