library(devtools)
install_github("riv","tomasgreif")
library(woe)
library(riv)

library(InformationValue)


lapply(invoices_clean,function(x) IV(x,"response"))

IV_mult <- for(i in colnames(invoices_clean)){
  browser()
  IV(invoices_clean[, i], invoices_clean[,"response"],valueOfGood = 0)
}

# payer_risika_score
IV(X=factor(invoices_clean$payer_risika_score,
               levels = c("?","0.0","1.0", "2.0","3.0","4.0","5.0","6.0","7.0","8.0","9.0","10.0")),
   Y=invoices_clean$response,valueOfGood = 1)

pp <- WOETable(X=factor(invoices_clean$payer_risika_score,
            levels = c("?","0.0","1.0", "2.0","3.0","4.0","5.0","6.0","7.0","8.0","9.0","10.0")),
   Y=invoices_clean$response,valueOfGood = 1)
pp %>% mutate(br = GOODS/TOTAL) %>% ggplot(aes(CAT,br))+geom_point()

# offer_seller_is_first_invoice
IV(X=factor(invoices_clean$offer_seller_is_first_invoice),
   Y=invoices_clean$response,valueOfGood = 1)

WOETable(X=factor(invoices_clean$offer_seller_is_first_invoice),
         Y=invoices_clean$response,valueOfGood = 1)%>% mutate(br = GOODS/TOTAL)

# payer_comp_type_f
IV(X=factor(invoices_clean$payer_comp_type_f),
   Y=invoices_clean$response,valueOfGood = 1)

WOETable(X=factor(invoices_clean$payer_comp_type_f),
         Y=invoices_clean$response,valueOfGood = 1)%>% mutate(br = GOODS/TOTAL)

# payer_industry_size_f
IV(X=factor(invoices_clean$payer_industry_size_f),
   Y=invoices_clean$response,valueOfGood = 1)

WOETable(X=factor(invoices_clean$seller_industry_size_f),
         Y=invoices_clean$response,valueOfGood = 1)%>% mutate(br = GOODS/TOTAL)

# seller_industry_size_f
IV(X=factor(invoices_clean$seller_industry_size_f),
   Y=invoices_clean$response,valueOfGood = 1)

WOETable(X=factor(invoices_clean$seller_industry_size_f),
         Y=invoices_clean$response,valueOfGood = 1)%>% mutate(br = GOODS/TOTAL)

# offer_seller_is_first_invoice

IV(X=factor(invoices_clean$offer_seller_is_first_invoice),
   Y=invoices_clean$response,valueOfGood = 1)

WOETable(X=factor(invoices_clean$offer_seller_is_first_invoice),
         Y=invoices_clean$response,valueOfGood = 1)%>% mutate(br = GOODS/TOTAL)










woe(Data=invoices_clean,
    Independent = "invoice_amount",
    Continuous = T,
    Dependent = "response",
    C_Bin = 10,
    Bad = 1,
    Good = 0
    )


t<-factor(invoices_clean$payer_risika_score,
          levels = c("?" ,   "0.0",  "1.0",  "10.0", "2.0" , "3.0" , "4.0" , "5.0" , "6.0",  "7.0" , "8.0"  ,"9.0"))
