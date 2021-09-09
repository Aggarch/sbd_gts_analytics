

#  Location
bottom_up      <- "C:/Users/AEG1130/Stanley Black & Decker/Heavner, Bill - Growth Initiatives/Bottoms Up Detail" 
setwd(bottom_up) 



# List of resources 

resources <- function(period, account){ 
  
data <- list.dirs() %>%
  as_tibble() %>% 
  filter(grepl(period,value)) %>% 
  rename(file = value) %>% 
  separate(file, c("main","second","third","fourth"),sep = "([/])") %>% 
  mutate(path = paste0(second,"/", third,"/", fourth)) %>% 
  filter(!is.na(fourth)) %>%
  select(-main) %>% 
  mutate(files = map(.$path, list.files)) %>% 
  unnest(cols = files) %>% 
  mutate(fullp = paste0(path,"/",files)) %>% 
  mutate(sheets = map(.$fullp, excel_sheets)) %>% 
  unnest(cols = sheets) %>% 
  filter(grepl(account, sheets))

return(data)

}



# specific list of resources 
object = resources('F3', ' CB')



# wrangler function original 
read_data <- function(file, sheet){ 
  
  data <- openxlsx::read.xlsx(file, sheet) 
  
  
  names(data) <- as.character(data[2,])
  
  data <- data %>% janitor::clean_names() %>% 
    mutate(sheet_name = sheet) %>% 
    mutate(file_name = file) %>% 
    slice(-2) %>% 
    as_tibble() %>% 
  select(!contains("actuals_")) %>% 
  select(!contains("na_"))
  # filter(!is.na(team),
  #       !is.na(growth_initiative))
  
  
  
  return(data)
  
}


# detail seeker addition to wrangler 
colnames_reader = function (file, sheet){read_data(file, sheet) %>% colnames()}



# evaluation 
datata = object %>%
  mutate(struct = map2(.$fullp, .$sheets, colnames_reader)) %>%
  unnest(cols = c(struct)) %>% 
  group_by(fullp,files, sheets) %>% 
  summarise(struct = str_flatten(struct, 
                                 collapse = ","), .groups = "drop")



# diagnostics 
dataproblem = datata %>% mutate(test = str_detect(struct, "team"))
dataproblem = datata %>% filter(!grepl("team", struct))





