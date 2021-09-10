
actual_t = "S:/North_America/Baltimore-BLT/Transformation Office/Growth Investment Tracking/Actual Tracking"

setwd(actual_t)


fiscalcal = openxlsx::read.xlsx("fiscalcal_2021.xlsx")

fisCal = fiscalcal %>% 
  mutate(p0 = as.Date(p0, origin = "1899-12-30")) %>% 
  mutate(p1 = as.Date(p1, origin = "1899-12-30"))



setwd("~/projects/sbd_gts_analytics")

saveRDS(fisCal, "fisCal.Rds")

readRDS("fisCal.Rds")