# essentials 

library(tidyverse)
fxps   <- "C:/Users/AEG1130/Documents/fx"   
setwd(fxps)

ps = openxlsx::read.xlsx("ps.rr.xlsx") %>% as_tibble %>% 
  janitor::clean_names()

ps.r = openxlsx::read.xlsx("ps.rr.xlsx","q") %>% as_tibble %>% 
  janitor::clean_names()

ps.r %>% anti_join(ps, by="entity") %>% 
  filter(value > 0 )

ps %>% anti_join(ps.r, by="entity")

# FX accounts under new Entity vector.

netsales = openxlsx::read.xlsx("fx_psum_acct_ytd.xlsx", "netsales") %>% as_tibble
sganda = openxlsx::read.xlsx("fx_psum_acct_ytd.xlsx", "sganda") %>% as_tibble
ocos = openxlsx::read.xlsx("fx_psum_acct_ytd.xlsx", "ocos") %>% as_tibble
sgm = openxlsx::read.xlsx("fx_psum_acct_ytd.xlsx", "sgm") %>% as_tibble


# salesfx = openxlsx::read.xlsx("fx_psum_acct.xlsx", "salesfx") %>% as_tibble
# sgafx = openxlsx::read.xlsx("fx_psum_acct.xlsx", "sgafx") %>% as_tibble
# ocosfx = openxlsx::read.xlsx("fx_psum_acct.xlsx", "ocosfx") %>% as_tibble


all_acct<- netsales %>% select(-account) %>% 
  left_join(sganda %>% select(-account)) %>% 
  left_join(ocos %>% select(-account)) %>% 
  left_join(sgm %>% select(-account)) 
  
  # left_join(salesfx%>% select(-account)) %>% 
  # left_join(sgafx%>% select(-account)) %>% 
  # left_join(ocosfx%>% select(-account)) 

compress<-all_acct %>% select(segment,region,sub_region,lc,entity,month,
                   netsales,sganda,ocos,sgm) %>% 
             replace(is.na(.), 0) 

wider_compress <- compress %>% 
  pivot_wider(names_from = month, values_from = c(netsales, sganda, ocos, sgm)) %>% 
  mutate(general = rowSums(across(where(is.numeric)))) %>% 
  filter(general!=0) %>% 
  filter(segment != "GTS") %>% 
  mutate(region = toupper(region)) %>% 
  select(-general)
  
  
  
ptable <- openxlsx::read.xlsx("PTableFX.xlsx","ps") %>% as_tibble() %>% 
  filter(!grepl("none",entity)) %>% 
  janitor::clean_names() %>% 
  #filter(business != "OPG Core") %>% 
  filter(month == "Aug") %>% 
  select(business,market_1,lc,region,entity) %>% 
  distinct()



comp <- wider_compress %>% 
  select(region, entity, lc) %>% 
  left_join(ptable, by = c("region","entity","lc")) 
 

fx_categ <- openxlsx::read.xlsx("fx_categ.xlsx") %>% as_tibble()


fx_categ_ytd = comp %>% select(entity) %>% distinct() %>% 
  left_join(fx_categ %>% 
  select(entity, business, region, market_1,market_2,lc),by = "entity") %>% 
  left_join(wider_compress %>% select(entity, lc,region), by = "entity") %>% 
  rename(lc=lc.y) %>% 
  rename(region = region.y) %>% 
  select(!contains(".x"))

# fx_categ_ytd %>% openxlsx::write.xlsx("fx_categ_ytd.xlsx", overwrite = T)


fx_orph <- fx %>% filter(is.na(business))

# clues >
reg_all %>% filter(name %in% fx_orph$entity) %>% select(name, description,structure)


# When comparing the Compress version total for NetSales and the same vector at
# FX Pivot Summaries, the result differs, however the vector is the same. 

comp <- function(period){
  
  table = wider_compress%>% select(entity,contains(period))

  netsales = paste0("netsales_",period)
  sganda = paste0("sganda_",period)
  ocos = paste0("ocos_",period)
  sgm = paste0("sgm_",period)
  
  
  
  return(table)
  
  
}

fx.ent.list <- openxlsx::read.xlsx("fx.entity.list.xlsx") %>% as_tibble()
acq.div <- openxlsx::read.xlsx("fx.entity.list.xlsx","opgacq") %>% as_tibble()


