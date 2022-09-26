library(readxl)
library(pillar)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rlang)

invoices <- read_excel("Moneyflow DS interview Case.xlsx", 
                                          sheet = "Train")

dim(invoices)
glimpse(invoices)

qplot(invoices$invoice_amount)
qplot(invoices$invoice_amount[invoices$invoice_amount<250000])
qplot(invoices$seller_age)
qplot(invoices$seller_risika_score)

qplot(invoices$inv_iss2due)
qplot(invoices$inv_iss2purch)
qplot(invoices$inv_due2today)

table(invoices$defaulted, useNA = 'ifany')
table(invoices$seller_is_public, useNA = 'ifany')
table(invoices$seller_danish_pep, useNA = 'ifany')

nrow(table(invoices$seller_main_industry_code))
nrow(table(invoices$seller_company_type))
table(invoices$seller_company_type)

invoices_clean <- invoices %>% 
  mutate(
    response = case_when(defaulted == 'y' ~ 1, T ~ 0),
    seller_danish_pep_num = as.numeric(seller_danish_pep),
    invoice_issue_date = today() - days(inv_due2today),
    invoice_issue_yearmonth = paste0(year(invoice_issue_date),
                                     case_when(
                                        nchar(month(invoice_issue_date)) == 1 ~ paste0('0',month(invoice_issue_date)), 
                                        T ~ as.character(month(invoice_issue_date)))))
                                     
  

invoices_clean %>% 
  group_by(invoice_issue_yearmonth) %>% 
  summarise(nn = n(),
            br = mean(response))

table(invoices_clean$seller_danish_pep_num)
