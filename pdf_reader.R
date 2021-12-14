

# PDF Reading  ------------------------------------------------------------

#install.packages("pdftools")
#install.packages("tm")


library(pdftools)
library(tidyverse)
library(tm)

down <- "C:/Users/AEG1130/Downloads"

setwd(down)

file <- list.files() %>% as_tibble() %>%
  filter(grepl(c("GTS"),value)) %>% 
  filter(grepl(c(".pdf"),value)) %>% 
  arrange(desc(value)) %>% 
  slice(1)
  

text <- pdf_text(file$value)

text2 <- strsplit(text, "\n")
