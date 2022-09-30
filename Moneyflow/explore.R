library(readxl) #excel import
library(pillar) #glimpse
library(ggplot2) #visualizations
library(dplyr) #data wrangling
library(lubridate) #dates
library(rlang) #debugging

invoices <- read_excel("Moneyflow DS interview Case.xlsx", 
                       sheet = "Train")

dim(invoices)
glimpse(invoices)

# Seller data, payer data and invoice data

qplot(invoices$invoice_amount)
qplot(invoices$invoice_amount[invoices$invoice_amount<100000])
qplot(invoices$seller_age)
qplot(invoices$payerr_age)
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

seller_industry_size <- invoices %>% 
  group_by(seller_main_industry_code) %>% 
  summarise(seller_ind_size = n()/nrow(invoices)) %>% 
  select(seller_main_industry_code, seller_ind_size)

seller_company_size <- invoices %>% 
  group_by(seller_company_type) %>% 
  summarise(seller_comp_size = n()/nrow(invoices)) %>% 
  select(seller_company_type, seller_comp_size)

payer_industry_size <- invoices %>% 
  group_by(payer_main_industry_code) %>% 
  summarise(payer_ind_size = n()/nrow(invoices)) %>% 
  select(payer_main_industry_code, payer_ind_size)

payer_company_size <- invoices %>% 
  group_by(payer_company_type) %>% 
  summarise(payer_comp_size = n()/nrow(invoices)) %>% 
  select(payer_company_type, payer_comp_size)


invoices_clean <- invoices %>% 
  mutate(
    response = case_when(defaulted == 'y' ~ 1, T ~ 0),
    seller_is_public_num = as.numeric(seller_is_public),
    seller_danish_pep_num = as.numeric(seller_danish_pep),
    payer_is_public_num = suppressWarnings(as.numeric(payer_is_public)),
    payer_danish_pep_num = as.numeric(payer_danish_pep),
    payer_eu_sanctions_num = as.numeric(payer_eu_sanctions),
    
    payer_risika_score_num = suppressWarnings(as.numeric(payer_risika_score)),
    
    offer_seller_is_first_invoice_num = suppressWarnings(as.numeric(offer_seller_is_first_invoice)),
    payer_age_num = suppressWarnings(case_when(as.numeric(payerr_age) == 2021 ~ 1,
                                               T ~ as.numeric(payerr_age))), # assuming 2021 is an error
    seller_age_num = as.numeric(seller_age),
    invoice_issue_date = today() - days(inv_due2today),
    invoice_issue_yearmonth = paste0(year(invoice_issue_date),
                                     case_when(
                                       nchar(month(invoice_issue_date)) == 1 ~ paste0('0',month(invoice_issue_date)), 
                                       T ~ as.character(month(invoice_issue_date)))),
    seller_payer_company_type_match = case_when(seller_company_type == payer_company_type ~ 1, T ~ 0),
    seller_payer_industry_type_match = case_when(seller_main_industry_code == payer_main_industry_code ~ 1, T ~ 0)) %>%
  
  left_join(seller_industry_size, by = 'seller_main_industry_code') %>% 
  left_join(seller_company_size, by = 'seller_company_type') %>% 
  left_join(payer_industry_size, by = 'payer_main_industry_code') %>% 
  left_join(payer_company_size, by = 'payer_company_type') %>% 
  mutate(payer_comp_type_f = case_when(payer_comp_size >0.50 ~'Large',
                                            payer_comp_size > 0.1 ~ 'Medium',
                                            T ~ 'Small'),
         seller_comp_type_f = case_when(seller_comp_size >0.50 ~'Large',
                                       seller_comp_size > 0.1 ~ 'Medium',
                                           T ~ 'Small'),
         payer_industry_size_f = case_when(payer_ind_size >0.1 ~'Large',
                                           payer_ind_size > 0.01 ~ 'Medium',
                                           T ~ 'Small'),
         seller_industry_size_f = case_when(seller_ind_size >0.07 ~'Large',
                                       seller_ind_size > 0.01 ~ 'Medium',
                                       T ~ 'Small'))



invoices_clean %>% 
  group_by(invoice_issue_yearmonth) %>% 
  summarise(nn = n(),
            br = mean(response))

invoices_clean %>% 
  group_by(seller_payer_industry_type_match) %>% 
  summarise(nn = n(),
            br = mean(response),
            avg_cost = mean(offer_cost_percent)) %>% View()

invoices_clean %>% 
  group_by(seller_payer_company_type_match) %>% 
  summarise(nn = n(),
            br = mean(response),
            avg_cost = mean(offer_cost_percent)) %>% View()

invoices_clean %>% 
  group_by(seller_payer_industry_type_match) %>% 
  summarise(nn = n(),
            br = mean(response),
            avg_cost = mean(offer_cost_percent)) %>% View()

invoices_clean %>% 
  group_by(cut(invoice_amount, breaks = seq(0,50000, by = 1000), dig.lab = 10)) %>% 
  summarise(nn = n(),
            br = mean(response),
            avg_cost = mean(offer_cost_percent),
            perc_total = nn/nrow(.)) %>% View()



table(invoices_clean$seller_danish_pep_num)

qplot(invoices_clean$seller_age_num)
qplot(invoices_clean$payer_age_num)
qplot(invoices_clean$payer_age_num[invoices_clean$payer_age_num<500])
qplot(invoices_clean$offer_cost_percent)
