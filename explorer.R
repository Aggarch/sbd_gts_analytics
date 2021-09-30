
library(tidyverse)
library(lubridate)
library(progressr)
library(readxl)
library(future)
library(tictoc)
library(furrr)
library(zoo)


# Extension scripting of prodebugg.R created to verify structure across all 
# NonCB related F09 Forecast, create a large table containing all F09 consolidation 
# and contrast it with F9 Targets, to identify possible miss recordings across docs. 
# the scripts also allows to identify specific areas where updating its necessary 
# a lot of cases will be correctly updated naturally because of the redundancy of 
# the system XOR no changes forecasted.   


bottom_up      <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Bottoms Up Detail" 
myDocs         <- "C:/Users/AEG1130/Documents"


# Resources
resources <- function(period){ 
  
datos = list.dirs() %>%
  as_tibble() %>% 
  filter(grepl(period,value)) %>% 
  rename(file = value) %>% 
  separate(file, c("main","second","third","fourth"),sep = "([/])") %>% 
  mutate(path = paste0(second,"/", third,"/", fourth)) %>% 
  filter(!is.na(fourth)) %>%
  select(-main) %>% 
  mutate(files = map(.$path, list.files)) %>% 
  unnest(cols = files) %>% 
  filter(third == "Forecast") %>% 
  filter(files == "Final") %>% 
  mutate(fullp = paste0(path,"/",files)) %>% 
  mutate(excels = map(.$fullp, list.files)) %>% 
  unnest(cols = excels) %>% 
  mutate(fullp = paste0(fullp,"/",excels)) %>% 
  mutate(sheets = map(.$fullp, excel_sheets)) %>% 
  unnest(cols = sheets) %>% 
  filter(!grepl("Summary|Dropdowns|Sheet", sheets)) %>% 
  mutate(type = case_when(str_detect(sheets,"NonCB")~"NonCB",
                          str_detect(sheets,"CB")~"CB",
                          str_detect(sheets,"Capex")~"Capex",
                          str_detect(sheets,"SalesSGM")~"SalesSGM",
                          TRUE ~ as.character(sheets))) %>% 
  distinct() %>% 
  filter(type == "NonCB")

return(datos)

}

# Reading
read_forecast_ncb <- function(file, sheet){ 
  
    data <- openxlsx::read.xlsx(file, sheet) 
    
    
    names(data) <- as.character(data[1,])
    
    data <- data %>% janitor::clean_names() %>% 
      mutate(sheet_name = sheet) %>% 
      mutate(file_name = file) %>% 
      slice(-1) %>% 
      as_tibble() %>% 
      mutate(file = file,
             sheet = sheet) %>% 
      select(!contains("q")) %>% 
      select(!contains("fy")) %>% 
      select(!contains("na")) %>% 
      select(!contains("forecast_region"))
    
    
    
    
    # select(team, growth_initiative, spend_category, spend_description, 
    #        october, november, december, q4) %>% 
    # filter(!is.na(growth_initiative)) %>% 
    # mutate_at(c("october", "november", "december","q4"), as.numeric)  
    # 
    
    
    return(data)
    
    # CB data colnames exists in second row because blank column dosnt count 
    # this additional column exist to set up the salary formula correctly. 
    # the salary formula most be re-engineered since it's unnecesary complex. 
    
    # Non-CB  Capex files colnames exists in fist row, SGMSales its a special 
    # example, where each sheet contains 3 or 4 different small tables. 
    
}


  
# Consolidation
consolidation <-function(data){
  
  
  n_cores <- parallel::detectCores()
  plan(multisession, workers = n_cores)
  
  options(future.rng.onMisuse="ignore")
  
  
  #tic()
  data <- future_map2(data$fullp, 
                      data$sheets,
                      read_forecast_ncb) %>% 
          future_map_dfr(., bind_rows, .progress = T)
  
  #toc()

  
  plan(sequential)
  
  return(data)
  
}



# ExEC --------------------------------------------------------------------


setwd(bottom_up) 


looping <- consolidation(resources(9))


ncb_fct_data<- looping %>% 
  select(team, growth_initiative, spend_category, spend_description, 
         september,october, november, december) %>% 
  filter(!is.na(growth_initiative)) %>% 
  mutate_at(c("september",
              "october",
              "november",
              "december"), as.double)



collected <- ncb_fct_data%>% 
  mutate(team = ifelse(is.na(team),"NA",team)) %>% 
  pivot_longer(!c(team, growth_initiative, spend_category,
                  spend_description),
               names_to = "month", values_to = "value") %>% 
  replace_na(list(value = 0)) 

  


# Standard ----------------------------------------------------------------

setwd(myDocs)

target <- openxlsx::read.xlsx("data/non_cb_f9.xlsx") %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(team = ifelse(is.na(team),"NA",team)) %>% 
  select(team, growth_initiative, 
         spend_category, spend_description,
         september, october,
         november, december) %>% 
  mutate(team = ifelse(growth_initiative == "China Accenture Accelerator", "Asia", team )) %>% 
  pivot_longer(!c(team, growth_initiative, spend_category,
                  spend_description),
               names_to = "month", values_to = "value") %>% 
  replace_na(list(value = 0))



# Diagnose ----------------------------------------------------------------


# simplify
st = target    %>% filter(value != 0) %>% select(team, growth_initiative, month, value)
sc = collected %>% filter(value != 0) %>% select(team, growth_initiative, month, value)


# labeled
sharep = sc %>% group_by(team,growth_initiative,month) %>%
  summarise(value = sum(value),.groups = "drop") %>% 
  rename(cloud_value = value)


rule =st %>% group_by(team,growth_initiative,month) %>%
  summarise(value = sum(value),.groups = "drop") %>% 
  rename(ruled_value = value)


# diagnostics 
diagnose = rule %>% left_join(sharep, 
         by = c("team","growth_initiative", "month")) %>% 
  mutate(divergence =  cloud_value - ruled_value)


# Sanity Check 
sum(diagnose$divergence)
sum(st$value) - sum(sc$value)


# Prioritize 
details <- diagnose %>% 
  filter(divergence != 0) %>% 
  mutate(divergence = round(divergence)) %>% 
  arrange(desc(divergence))

sum(details$divergence)


# The reason why there is divergence its due to the fact that there exist multiple 
# files with the same team, initiative and month, usually duplicated in 7 files. 
# that belongs to NA & HQ. 

# Reconciled. 


