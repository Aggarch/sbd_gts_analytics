#Calendar

setwd("C:/Users/AEG1130/Documents")

fcal <- openxlsx::read.xlsx("calendar.xlsx") %>% 
  as_tibble() %>% 
  mutate(date2 = str_replace_all(date,"October","Oct")) %>% 
  mutate(date2 = str_replace_all(date2,"October","Oct")) %>% 
  mutate(date2 = str_replace_all(date2,"September","Sep")) %>% 
  mutate(date2 = str_replace_all(date2,"Sept","Sep")) %>% 
  mutate(date2 = str_replace_all(date2,"Week Of","")) %>% 
  mutate(date2 = str_trim(date2)) %>% 
  mutate(period = substr(date2,0,3)) %>% 
  mutate(day = substr(date2, 4,6)) %>% 
  mutate(day = str_replace_all(day, "t","")) %>%
  mutate(day = str_replace_all(day, "n","")) %>% 
  mutate(day = str_replace_all(day," ","")) %>% 
  mutate(day = as.numeric(day))
  
  
  sep<-fcal %>% filter(period == "Sep") %>% arrange(day)
  oct<-fcal %>% filter(period == "Oct") %>% arrange(day)
  
  calendar_arrange <- sep %>% bind_rows(oct) %>% 
  rename(original = date) %>% 
  rename(date = date2)

  calendar_arrange %>% openxlsx::write.xlsx("calendar_arrange.xlsx", overwrite = T)
  