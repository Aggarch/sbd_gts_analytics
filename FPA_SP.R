
checklist <- "C:/Users/AEG1130/Stanley Black & Decker/GTS Group FP&A - Documents/2021/Close/11 November 2021/Closing Files/Close Checklist"
FPA.SP <- "C:/Users/AEG1130/Stanley Black & Decker/GTS Group FP&A - Documents/2021/Close"


setwd(FPA.SP)
list.files()


library(tidyverse)


directories <- function(period){ 

resources <- list.dirs() %>%
  as_tibble() %>% 
  rename(directories = value) %>% 
  filter(grepl(period,directories))


return(resources)

}


report_time <- function(){ 

setwd(checklist)
  

# ref <- format(as.POSIXct(lubridate::now(),format='%I:%M %p'),format="%I:%M %p")
# 
# timing <- ref %>%
#   as_tibble   %>%
#   mutate(ttt = as.double(substr(value,0,2))) %>%
#   mutate(timing = as.character(ttt)) %>%
#   pull(timing)
  
timing <- c(1:10) %>% as.character()
  

checkl<- openxlsx::read.xlsx(list.files()) %>% 
  as_tibble() %>%
  filter(X7 == "A. Garcia") %>%
  select(-X10, -X2) %>% 
  rename(close_day =X3, deliv_description = X4 ,source =X9, 
         frequency = X5, due_date_time = X6, owner = X7, status = X8) %>% 
  mutate_all(str_trim) %>% janitor::clean_names() %>% 
  mutate(close_day = as.double(close_day)) %>% 
  mutate(close_day = as.Date(close_day, origin = "1899-12-30")) %>% 
  filter(close_day == lubridate::today()+1) %>% 
  filter(status != "Completed") 
  
  # filter(grepl(timing, due_date_time))

resum <- checkl %>% select(-source)

print(ref)

link <- checkl %>% select(source)

map(link$source, browseURL)

print(link)

return(resum)


}
  
  
# shell.exec(list.files())




#Email Trigger when "Hey R - Send Update"

library(blastula)
library(keyring)

#andres.garcia@sbdinc.com


# 
# create_smtp_creds_key(
#   id = "gmail",
#   user = "financialaboratory@gmail.com",
#   provider = "gmail",
#   overwrite = T
# )
#  


view_credential_keys()


unalloc <- "C:/Users/AEG1130/Stanley Black & Decker/GTS Group FP&A - Documents/2021/Close/11 November 2021/Closing Files/Close Trackers/Unallocated Report"
setwd(unalloc)


email  <- compose_email(body = email.to.do,
                        footer = email_footer)%>%
  add_attachment(file = "GTS_PRODUCT_UNALLOCATED_2021_NOV_CLOSE_12092021@9AM.xlsx", filename = "unalloc")


smtp_send(email,
          from = "financialaboratory@gmail.com",
          to = "andresgarcia.dc@gmail.com",
          subject = "Product Unallocated Report December 9 @ 12PM",
          credentials = creds_key("gmail")
)


# Batch file 
# C:\"Users"\AEG1130\Documents\R\R-4.1.1\bin\Rscript.exe C:\Users\AEG1130\Documents\Trigger\emailTriggers.R
