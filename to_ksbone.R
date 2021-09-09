

library(tidyverse)

TO = "S:/North_America/Baltimore-BLT/Transformation Office/Admn/TO Reporting"

setwd(TO)

ccraw <- openxlsx::read.xlsx("cc9401500230_TO_Jul.xlsx") 


ccraw %>% as_tibble()

