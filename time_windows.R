
# Alternative MTD approach 
# Higher consistency 

dp_forecast(tw)$detailed %>% 
  bind_rows(dp_close_actuals(tw)$detailed) %>%
  bind_rows(dp_op_plan(tw)$detailed) %>%
  filter(period == tw) %>% 
  select(category,type,value) %>% 
  pivot_wider(names_from = type, 
              values_from = value,
              values_fn = sum) %>% 
  rename(MTD_forecast = fcast,
         MTD_actuals = actuals,
         MTD_OP =op) %>% 
  select(category, MTD_actuals,MTD_forecast,MTD_OP) %>%
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>%  
  mutate(MTD_actuals  = MTD_actuals/1000,
         MTD_forecast = MTD_forecast/1000,
         MTD_OP       = MTD_OP/1000) %>% 
  mutate(MTD_VF  = MTD_actuals-MTD_forecast,
         MTD_VOP = MTD_actuals-MTD_OP) %>% 
  mutate_if(is.numeric, round) 

#janitor::adorn_totals()


# Alternative QTD approach 
# Higher consistency 

QTD = dp_forecast(tw)$detailed %>% 
  bind_rows(dp_close_actuals(tw)$detailed) %>%
  bind_rows(dp_op_plan(tw)$detailed) %>%
  filter(period <= tw) %>% 
  filter(quarter == q) %>% 
  select(category,type,value) %>% 
  pivot_wider(names_from = type, 
              values_from = value,
              values_fn = sum) %>% 
  rename(QTD_forecast = fcast,
         QTD_actuals = actuals,
         QTD_OP =op) %>% 
  select(category, QTD_actuals,QTD_forecast,QTD_OP) %>%
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>%  
  mutate(QTD_actuals  = QTD_actuals/1000,
         QTD_forecast = QTD_forecast/1000,
         QTD_OP       = QTD_OP/1000) %>% 
  mutate(QTD_VF  = QTD_actuals-QTD_forecast,
         QTD_VOP = QTD_actuals-QTD_OP) %>% 
  mutate_if(is.numeric, round) 




# Alternative QTD approach 
# Higher consistency 

YTD = dp_forecast(tw)$detailed %>% 
  bind_rows(dp_close_actuals(tw)$detailed) %>%
  bind_rows(dp_op_plan(tw)$detailed) %>%
  filter(period <= tw) %>% 
  select(category,type,value) %>% 
  pivot_wider(names_from = type, 
              values_from = value,
              values_fn = sum) %>% 
  rename(YTD_forecast = fcast,
         YTD_actuals = actuals,
         YTD_OP =op) %>% 
  select(category, YTD_actuals,YTD_forecast,YTD_OP) %>%
  mutate_if(~ any(is.na(.)),~ if_else(is.na(.),0,.)) %>%  
  mutate(YTD_actuals  = YTD_actuals/1000,
         YTD_forecast = YTD_forecast/1000,
         YTD_OP       = YTD_OP/1000) %>% 
  mutate(YTD_VF  = YTD_actuals-YTD_forecast,
         YTD_VOP = YTD_actuals-YTD_OP) %>% 
  mutate_if(is.numeric, round) 
