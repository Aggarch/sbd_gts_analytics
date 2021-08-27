

library(tidyverse)

setwd("C:/Users/AEG1130/Documents/data")


# Exploratory -------------------------------------------------------------



# Import >
reqs_approved<-openxlsx::read.xlsx("reqs_approved_BR.xlsx")
reqs_bill<-openxlsx::read.xlsx("reqs_bill.xlsx")
reqs_mike<-openxlsx::read.xlsx("reqs_mike.xlsx")

reqs_all <- reqs_approved %>% 
   bind_rows(reqs_bill) %>% 
   bind_rows(reqs_mikes) %>% 
   as_tibble()



# Manipulate >    
wrangler<- function(data){
   
   
  output <- data %>% as_tibble %>% janitor::clean_names() %>% 
      separate(gts_justification, c("main","team"),sep = "([.?:])") %>% 
      select(-main) %>% rename(gts_justification = team) %>% 
      mutate_if(is.character, str_trim) %>% 
      separate(business, c("region","segment","sbu"),sep = "([-])") %>% 
      replace_na(list(sbu = 'unknown')) %>% 
      mutate_if(is.character, str_trim) %>% 
      mutate(br_number = ifelse(is.na(br_number),br_number_2,br_number))
  
  return(output)
   
   
} 

reqs_all  <- wrangler(reqs_all)
# 267 observation: approved = 106, bill 145, mike 16 == 267 


# Sumariszed >
resumen <- function(data){ 

   
   output <- data %>% 
   group_by(region,segment, sbu, gts_justification) %>% 
   summarise(n = n(), .groups = "drop" ) %>% 
   janitor::adorn_totals()
   
   return(output)
   

}




# F(x)nctional --------------------------------------------------------------


cb_resources <- function(){ 

setwd("C:/Users/AEG1130/Documents/data/cb")

# All resources 
resources <- list.files() %>% as_tibble() %>% 
rename(files = value) %>% 
 mutate(sheets = map(.$files, excel_sheets)) %>% 
 unnest(cols = sheets) %>% 
   filter(sheets != "Dropdowns") %>% 
   mutate(team = substr(files, 0,4)) %>% 
   mutate(team = str_extract(team, pattern = "[^_]+")) %>% 
   mutate(dimen= ifelse(team %in% c("GCX","HTAS","OPS","PTG","OPG"),
                        "fcasted","regular"))
   

# Sheets and names by file  

sheet_names_dist = resources %>%
   group_by(files) %>%
   summarise(no_sheets = n(),
             names = str_flatten(sheets, collapse = ", "))



# 2 distinct types of dimensions files, distribution 

structure_types = resources %>% 
   select(team, dimen) %>%
   distinct() %>% 
   group_by(dimen) %>% 
   summarise(n = n(), 
             teams = str_flatten(team,
                                 collapse = ", "))



# final result will be to tables to respect dimensions 

regs_rec <- resources %>% filter(dimen == "regular")
fcst_rec <- resources %>% filter(dimen == "fcasted")



return(list(resources = resources,
            sheet_names_dist = sheet_names_dist,
            structure_types  = structure_types,
            regs_rec = regs_rec, fcst_rec = fcst_rec
            ))


}


# clean data by file and sheet. 
read_clean <- function(file, sheet){ 

data <- openxlsx::read.xlsx(file, sheet) %>% 
   as_tibble() %>% slice(-1)


names(data) <- as.character(data[1,])

data <- data %>% janitor::clean_names() %>% 
   slice(-1) %>% 
   filter(!is.na(team))


return(data)

}


#  Iterate function and merge results. as a function 



consolidation <-function(data){
   
                 map2(data$files, 
                      data$sheets,
                      read_clean) %>% 
                 map_dfr(., bind_rows) %>% 
   mutate(actual_start_date = as.numeric(actual_start_date)) %>% 
   mutate(actual_start_date = as.Date(actual_start_date, origin = "1899-12-30")) %>% 
   # mutate(actual_salary_usd = as.double(actual_salary_usd)) %>% 
   replace_na(list(actual_salary_usd = 0))
   
}




longt <- consolidation(cb_resources()$fcst_rec)
short <- consolidation(cb_resources()$regs_rec)


# a) check if these brassring#s are in our SharePoint files 
# b) log the requisitions that are missing from our files and in our forecast.
#    This is all in an effort to report how many open roles we have in BrassRing related to Growth Investments. 


cross_testing <- reqs_all %>%
   mutate(longt.test = ifelse(br_number %in% longt$brassring_req_number,1,0)) %>% 
   mutate(short.test = ifelse(br_number %in% short$brassring_req_number,1,0)) %>% 
   select(br_number, contains(".test")) %>% 
   mutate(f.test = longt.test + short.test)


# Where {0} in the final test highlight those cases where the given req number
# do not exist or match with any in the stored universe. 

cases_to_fill <- cross_testing %>% filter(f.test == 0)
cases_filled <- cross_testing %>% filter(f.test == 1)

completed_data <- reqs_all %>% semi_join(cases_to_fill, by = "br_number")




