# Heatmap Data 

setwd("C:/Users/AEG1130/Documents/heatmap_PNL")

hmap <- openxlsx::read.xlsx("raw_hd.xlsx") %>% 
  as_tibble()


library(tidyverse)

setwd("C:/Users/AEG1130/Documents")

dmt <- openxlsx::read.xlsx("Construction Tech C11_LCC.xlsx", sheet = 1) %>% as_tibble()


dc <- openxlsx::read.xlsx("Construction Tech C11_LCC.xlsx", sheet = 2) %>% as_tibble()
