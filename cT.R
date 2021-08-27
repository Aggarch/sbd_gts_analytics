

# cTrader 

setwd("C:/Users/AEG1130/Documents/data")


fintra<-openxlsx::read.xlsx('fintra.xlsx') %>% slice(-1)
names(fintra) <- as.character(fintra[1,])

details <- fintra %>% janitor::clean_names() %>% slice(-1) %>% 
  separate(transaction_date, c("date","time"),sep = "([.?:])") %>% 
  mutate(date = as.numeric(date)) %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  select(-time) %>% 
  filter(status != 'Canceled')
  

wider_detail <- details %>% 
  pivot_wider(names_from = 'transaction_type',
              values_from = 'amount' ) %>% 
  janitor::clean_names() %>% 
  replace_na(list(withdrawal = 0, deposit = 0)) %>%
  mutate(withdrawal = as.double(withdrawal),
         deposit = as.double(deposit)) %>% 
  janitor::adorn_totals()


balance <- details %>%
  group_by(transaction_type) %>%
  mutate(amount=as.numeric(amount) ) %>% 
  summarise(amount=sum(amount)) %>% 
  janitor::adorn_totals()
