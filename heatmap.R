# Heatmap Data 

setwd("C:/Users/AEG1130/Documents/heatmap_PNL")

hmap <- openxlsx::read.xlsx("raw_hd.xlsx") %>% 
  as_tibble()

