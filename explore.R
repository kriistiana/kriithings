library(readxl)
library(pillar)
library(ggplot2)
library(dplyr)

invoices <- read_excel("Moneyflow DS interview Case.xlsx", 
                                          sheet = "Train")

dim(invoices)
glimpse(invoices)

qplot(invoices$invoice_amount)
qplot(invoices$invoice_amount[invoices$invoice_amount<250000])
qplot(invoices$seller_age)
qplot(invoices$seller_risika_score)

table(invoices$defaulted, useNA = 'ifany')
table(invoices$seller_is_public, useNA = 'ifany')
table(invoices$seller_danish_pep, useNA = 'ifany')

nrow(table(invoices$seller_main_industry_code))
nrow(table(invoices$seller_company_type))
table(invoices$seller_company_type)

invoices_clean <- invoices %>% 
  mutate(
    response = case_when(defaulted == 'y' ~ 1, T ~ 0),
    seller_danish_pep_num = as.numeric(seller_danish_pep)
  )

table(invoices_clean$seller_danish_pep_num)
