# SOC2C

library(tidyverse)
library(openxlsx)

soc <- "C:/Users/AEG1130/Documents/SOC2C" 
setwd(soc)


# SOC2C -------------------------------------------------------------------

# Create new column form Common Criteria header, use document after converting from PDF to Word,
# Convert toc to excel copying tables. 
# Delete empty spaces after rolling the new column values, 
# Elimanate redundant titles and formating.
# e.g. (soc2Ct.xlsx)


soc2 <- openxlsx::read.xlsx("soc2Ct.xlsx") %>% as_tibble() %>% 
  janitor::clean_names() 

descriptions <- soc2 %>% 
  filter(!is.na(criteria_descrption)) %>%
  group_by(common_criteria) %>% 
  summarise(criteria_description = str_flatten(criteria_descrption)) %>% 
  mutate(common_criteria = str_replace_all(common_criteria,"Related to","")) %>% 
  mutate(common_criteria = str_trim(common_criteria))




# Criteria Policy ---------------------------------------------------------

criteria_policy <- soc2 %>% select(common_criteria,
                                   nasdaq_policies_and_controls
                                   ) %>% 
  mutate(common_criteria = str_trim(common_criteria)) %>% 
  mutate(nasdaq_policies_and_controls = str_trim(nasdaq_policies_and_controls)) %>% 
  mutate(policy_ref = substr(nasdaq_policies_and_controls,0,10)) %>% 
  mutate(policy_ref = str_trim(policy_ref)) %>% 
  select(-nasdaq_policies_and_controls) %>%
  filter(grepl("CT",policy_ref)) %>% 
  distinct() %>% 
  mutate(common_criteria = str_replace_all(common_criteria,"Related to","")) %>% 
  mutate(policy_ref = str_remove_all(policy_ref,":"))
  
  

# Policy Controls ---------------------------------------------------------------------

  policy_controls <-  soc2 %>%
    select(nasdaq_policies_and_controls) %>%
    mutate(join = substr(nasdaq_policies_and_controls,0,9)) %>% 
    mutate(ttt = str_detect(join,"CT")) %>% 
    mutate(ppp = ifelse(ttt=="TRUE",join,NA)) %>% 
    fill(ppp, .direction = 'down') %>% 
    select(-join,-ttt) %>% 
  distinct()


# EY Results ---------------------------------------------------------------------

ey_results <-  soc2 %>%
  select(nasdaq_policies_and_controls, ey_tests, ey_test_results) %>%
  mutate(join = substr(nasdaq_policies_and_controls,0,9)) %>% 
  mutate(ttt = str_detect(join,"CT")) %>% 
  mutate(ppp = ifelse(ttt=="TRUE",join,NA)) %>% 
  fill(ppp, .direction = 'down') %>% 
  select(-join,-ttt, -nasdaq_policies_and_controls) %>% 
  distinct() %>% 
  mutate(ey_tests = str_trim(ey_tests)) %>% 
  filter(!is.na(ey_tests)) %>% 
  distinct() %>%
  mutate(ey_test_results = ifelse(is.na(ey_test_results),
                                  "No deviations noted.", ey_test_results)) %>% 
  mutate(ey_test_results = str_trim(ey_test_results))
  


# Summary Table -----------------------------------------------------------

summary_table <- criteria_policy %>% 
  left_join(policy_controls, by = c("policy_ref" = "ppp"))%>% 
  mutate(nasdaq_policies_and_controls = str_trim(nasdaq_policies_and_controls)) %>% 
  distinct() %>% 
  filter(!is.na(nasdaq_policies_and_controls)) %>% 
  group_by(common_criteria, policy_ref) %>% 
  summarise(nasdaq_policies_and_controls = str_flatten(nasdaq_policies_and_controls," "),
            .groups = "drop") %>% 
  ungroup() %>% 
  left_join(ey_results, by = c("policy_ref" = "ppp"))
  
  
  
  
  