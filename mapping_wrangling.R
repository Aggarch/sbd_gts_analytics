

# Mapping CC to Function --------------------------------------------------


# Open DMT 
# Citrix Script to run given a specific target. 

# Cost_to_fucntion

getwd()
setwd("C:/Users/AEG1130/Documents/mapping")

# MK provided file:::

cc_function <- openxlsx::read.xlsx("cc_function.xlsx") %>%
  as_tibble() %>% 
  janitor::clean_names() %>%
  select(-x7) %>% 
  filter(!is.na(bar_function))

collapsed<- cc_function %>% 
  select(source_cost_cntr, bar_function) %>% 
  distinct() %>% 
  group_by(source_cost_cntr) %>% 
  summarise(bar_function = str_flatten(bar_function,collapse = ","))

# DMT MAP COCTR Function

dmt_function_cc <- openxlsx::read.xlsx("dmt_function_cc.xlsx") %>%
  as_tibble() %>% 
  janitor::clean_names()



dmt_function_cc2 <- openxlsx::read.xlsx("prod2_table.xlsx") %>%
  as_tibble() %>% 
  janitor::clean_names()


dmt_function_cc %>% filter(coctr == "1605641461")
dmt_function_cc %>% filter(coctr == "9401500172")
dmt_function_cc %>% filter(coctr == "6013040164")

dmt_function_cc %>% filter(coctr == "9401500170")


# CC To Function Current Mapping Table. 
current <- dmt_function_cc %>% filter(coctr %in% collapsed$source_cost_cntr)

current2 <- dmt_function_cc2 %>% filter(coctr %in% collapsed$source_cost_cntr)


# Mike Kemp Reported Not in Construction table. 
reported <- collapsed %>% filter(!source_cost_cntr %in% dmt_function_cc$coctr)


# Mike Reported Structure: 
summary_c <- collapsed %>%
  mutate(count = str_count(bar_function,",")) %>% 
  mutate(bar_function_counting = count+1) %>% 
  select(-count)

# Current State of BAR CC to Function: 
summary_r <- current %>%
  group_by(coctr) %>% 
  summarise(bar_function = str_flatten(bar_function, collapse=",")) %>% 
  mutate(count = str_count(bar_function,",")) %>% 
  mutate(bar_function_counting = count+1) %>% 
  select(-count)


# Reported Not in Current State 

diff<- summary_c %>% filter(!source_cost_cntr %in% summary_r$coctr)


# Verify Patel Entity and Function ----------------------------------------


dmt_function_cc <- openxlsx::read.xlsx("dmt_function_cc.xlsx") %>%
  as_tibble() %>% 
  janitor::clean_names()


dmt_ent_usd <- openxlsx::read.xlsx("ent_table_usd.xlsx") %>%
  as_tibble() %>% 
  janitor::clean_names()


patelg<- openxlsx::read.xlsx("patel_guide.xlsx") %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(lcc = as.character(lcc))



# Roll files over ---------------------------------------------------------

library(tidyverse)
library(lubridate)

f4<- "C:/Users/AEG1130/Documents/Forecast_04/F04_BL"

setwd(f4)

hoy <- today() %>% 
  as_tibble() %>% 
  mutate(value = str_replace_all(value,"-","_"))

time<- now() %>% 
  as_tibble() %>%
  mutate(value = as.character(value)) %>% 
  separate(value, c("day","time"), sep = " ") %>% 
  separate(time, c("hour","mins","secs"),sep = ":") %>%
  select(-day,-secs) %>% 
  unite("time", hour:mins)


files <- list.files() %>% as_tibble() %>% 
  mutate(value = str_replace_all(value,".xlsx","")) %>% 
  mutate(append = paste("_BL_",hoy,"@",time,".xlsx")) %>% 
  mutate(append = str_replace_all(append," ","")) %>% 
  mutate(append = str_trim(append)) %>% 
  mutate(fname = paste0(value,append)) %>% 
  select(-append)


getwd()
old_files <- list.files()


new_files <- files %>% pull(fname)

file.copy(from = old_files, to = new_files)
# Clear out the old files
file.remove(old_files)

# BL Elements Created refreshes on Live Versions
# At this point should be just about Breaking Links. 

rnf<- old_files %>% as_tibble() %>% mutate(fname = str_replace_all(value,"11_14","11AM"))

new_files <- rnf %>% pull(fname)

file.copy(from = old_files, to = new_files)
file.remove(old_files)



