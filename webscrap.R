
# WEB SCRAPPING


# email updates: https://r-bar.net/r-scripts-mobile-device-email-triggers/

require(rvest)                                       #package to parse HTML
page <- read_html("https://apps.r-bar.net/PID1025/") #Get the Page
tbls <- html_nodes(page, "table")                    #Get the Tables
Product_Status <- html_table(tbls[1])[[1]]           #Get First Table

ToShip <- grep(pattern = "Shipping Yesterday|Shipping Today", 
               x = Product_Status$`Product Metrics`)
Shipping_Product <- sum(Product_Status$Count[ToShip])

#Detemine how many minute before the shipping dead line
MinLeft <- gsub(pattern = '\\D', 
                replacement = "" , 
                Product_Status$`Product Metrics`)[[2]]

#The summary message to be e-mailed
email.msg <- 
  paste0(Shipping_Product, " units of product shipping as of ", date(), 
         ". There is ", MinLeft, 
         " minutes remaining before today's shipping deadline.")


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

email_body <-  paste("Hi! This new report was generated at", Sys.time())

email_footer <- "Please contact us with any questions"

email  <- compose_email(body = email.msg,
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
