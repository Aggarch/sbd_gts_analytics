
# FX rates Sales Impact #

fx    <- "//americas.swk.pri/Apps/Enterprise/Reval/IntRptg/FXRates/Reval/Loaded" 
model <- "C:/Users/AEG1130/Documents/fx"   
docs  <- "C:/Users/AEG1130/Documents" 


# Rates & Keep Bzns Days

setwd(fx)

rate_files <- list.files() %>% as_tibble() %>% 
  filter(!grepl("Arch", value)) %>% 
  rename(rates = value) %>% 
  janitor::clean_names() %>% 
  mutate(dates = substr(rates, 18,27))
  

# Bzns Calendar 

setwd(docs)

fcalend <- openxlsx::read.xlsx("fiscal_calendar.xlsx") %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  select(!contains("left"),-report_date,-week_2) %>% 
  mutate(data_date = as.Date(data_date, origin = "1899-12-30")) %>% 
  mutate(dates = as.character(data_date)) %>% 
  select(-data_date)


# Resources

resource <- rate_files %>%
  left_join(fcalend, by = "dates") %>% 
  filter(!is.na(hfm_day))


# FX Construct 

setwd(model)

model <- openxlsx::read.xlsx("GTS_FX_2022_FEB28_SPOT_03022022@7PM.xlsx",
                    sheet = "SBD Rates 2022")

struct <- model %>% as_tibble() %>% select(X2) %>% filter(!is.na(X2)) %>% 
  rename(from_ccy = X2)


# Extractor

setwd(fx)

f <- function(rates){ 

files <- read.csv(rates) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(from_ccy,date,spot_rate) %>%  
  left_join(resource %>% select(dates,week),
            by = c("date"="dates"))

  
  # name <- files %>% distinct(date)
  # 
  # colnames(files)[3] <- name$date
  # 
  # files <- files %>% select(-date) 
  

  
  return(files)
  
}


# Ideal structure calculating 

pull_long <- map(resource$rates,f) %>% 
  map_dfr(., bind_rows)

  
wider_pull <- pull_long %>% select(-week) %>% 
  pivot_wider(names_from = date, values_from = spot_rate)
  


setwd()

struct %>% left_join(wider_pull, by = "from_ccy") %>% 
  openxlsx::write.xlsx("rates_fx.xlsx")






